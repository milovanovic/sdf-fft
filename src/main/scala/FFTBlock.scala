// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

trait AXI4FFTStandaloneBlock extends AXI4FFTBlock[FixedPoint] {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
  
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

abstract class FFTBlock [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: FFTParams[T], beatBytes: Int, configInterface: Boolean = false) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val streamNode = AXI4StreamIdentityNode()

  // add slave node for fft configuration
  val slaveParams = AXI4StreamSlaveParameters()
  val configNode  = if (configInterface == true) Some(AXI4StreamSlaveNode(slaveParams)) else None

  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    //  FFT module
    val fft = Module(new SDFFFT(params))
    // Number of stages
    val numStages = log2Ceil(params.numPoints)
    // Status registers
    val busy            = RegInit(false.B)

    busy := fft.io.busy
    var commonFields = Seq[RegField]()

    if (params.runTime) {
      val fftSize = RegInit(numStages.U(log2Ceil(numStages + 1).W))
      fftSize.suggestName("fftSize")
      commonFields = commonFields :+ RegField(log2Ceil(numStages), fftSize,
      RegFieldDesc(name = "fftSize", desc = "contains fft size which is used for run time configurability control"))
      if (configInterface) {
        val configReg = RegInit(numStages.U(log2Ceil(numStages + 1).W))
        val configMode = RegInit(false.B)
        configMode.suggestName("configMode")
        // number of stages is sent/ this can be AXI4 stream with datawidth 8
        when (configMode === true.B) {
          when (configNode.get.in(0)._1.fire()) {
            configReg := configNode.get.in(0)._1.bits.data.asUInt
          }
          configNode.get.in(0)._1.ready := ~busy
          fft.io.fftSize.get := configReg
        }
        .otherwise {
          fft.io.fftSize.get := fftSize
        }
        configNode.get.in(0)._1.bits.last := false.B //DontCare
        commonFields = commonFields :+ RegField(1, configMode,
        RegFieldDesc(name = "configMode", desc = "Defines the way of fftSize configuration"))
      }
      else {
        fft.io.fftSize.get := fftSize
      }
    }
    if (params.keepMSBorLSBReg) {
      val keepMSBorLSBReg = RegInit(0.U((numStages).W))
      keepMSBorLSBReg.suggestName("keepMSBorLSBReg")
      fft.io.keepMSBorLSBReg.get := keepMSBorLSBReg.asBools
      commonFields = commonFields :+ RegField(numStages, keepMSBorLSBReg,
        RegFieldDesc(name = "keepMSBorLSBReg", desc = "defines scaling behaviour for each stage"))
    }
    if (params.fftDirReg) {
      val fftDir = RegInit(true.B)
      fft.io.fftDirReg.get := fftDir
      fftDir.suggestName("fftDir")
      commonFields = commonFields :+ RegField(1, fftDir,
        RegFieldDesc(name = "fftDir", desc = "transform direction: fft or ifft"))
    }
    if (params.overflowReg) {
      val overflowReg = RegInit(0.U(log2Ceil(numStages).W))
      overflowReg.suggestName("overflowReg")
      overflowReg := fft.io.overflow.get.asUInt
      commonFields = commonFields :+ RegField.r(log2Ceil(numStages), overflowReg,
        RegFieldDesc(name = "overflowReg", desc = "returns overflow status for the each stage"))
    }
    commonFields = commonFields :+ RegField.r(1, busy,
     RegFieldDesc(name = "busy", desc = "indicates if fft core is in the state sFlush or sProcess"))

    // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(commonFields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
    
    // Connect inputs
    fft.io.in.valid    := in.valid
    require(in.bits.data.getWidth >= fft.io.in.bits.imag.getWidth + fft.io.in.bits.real.getWidth, "Data width is not appropriately set")

    // when AXI4 stream data width is not equal to fft input data width be careful with assignment
    // custom, because it is assumed that preproc block precedes and pack data on this way
    fft.io.in.bits.imag  := in.bits.data(in.bits.data.getWidth/2 - 1, 0).asTypeOf(params.protoIQ.imag)
    fft.io.in.bits.real  := in.bits.data(in.bits.data.getWidth-1, in.bits.data.getWidth/2).asTypeOf(params.protoIQ.real)
    //fft.io.in.bits     := in.bits.data.asTypeOf(params.protoIQ)
    in.ready           := fft.io.in.ready
    fft.io.lastIn      := in.bits.last
    
    // Connect output
    out.valid        := fft.io.out.valid
    fft.io.out.ready := out.ready
    out.bits.data    := fft.io.out.bits.asUInt
    out.bits.last := fft.io.lastOut
     // It is not necessary to connect these signals
    // out.bits.id   := 0.U
    // out.bits.dest := 0.U
  }
}

class AXI4FFTBlock[T <: Data : Real: BinaryRepresentation](params: FFTParams[T], address: AddressSet, _beatBytes: Int = 4, configInterface: Boolean)(implicit p: Parameters) extends FFTBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes, configInterface) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

class TLFFTBlock[T <: Data : Real: BinaryRepresentation](val params: FFTParams[T], address: AddressSet, beatBytes: Int = 4, configInterface: Boolean)(implicit p: Parameters) extends FFTBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](params, beatBytes) with TLDspBlock with TLHasCSR {
  val devname = "TLFFTBlock"
  val devcompat = Seq("fft", "radardsp")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem = Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = beatBytes))
}

object FFTDspBlockTL extends App
{
  val paramsFFT = FFTParams.fixed(
    dataWidth = 16,
    twiddleWidth = 16,
    numPoints = 1024,
    runTime = true,
    numAddPipes = 1,
    numMulPipes = 1,
    expandLogic = Array.fill(log2Up(1024))(0),
    keepMSBorLSB = Array.fill(log2Up(1024))(true),
    overflowReg = true,
    keepMSBorLSBReg = true,
    binPoint = 1
  )
  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty
  val fftModule = LazyModule(new TLFFTBlock(paramsFFT, AddressSet(baseAddress + 0x100, 0xFF), beatBytes = 4, configInterface = false) with dspblocks.TLStandaloneBlock)
  
  chisel3.Driver.execute(args, ()=> fftModule.module)
}

object FFTDspBlockAXI4 extends App
{
  val paramsFFT = FFTParams.fixed(
    dataWidth = 16,
    twiddleWidth = 16,
    numPoints = 1024,
    useBitReverse = false,
    runTime = true,
    numAddPipes = 1,
    numMulPipes = 1,
    expandLogic = Array.fill(log2Up(1024))(0),
    keepMSBorLSB = Array.fill(log2Up(1024))(true),
    overflowReg = false,
    keepMSBorLSBReg = false,
    binPoint = 0,
    minSRAMdepth = 1024
  )
  val baseAddress = 0x500 // just to check if verilog code is succesfully generated or not
  implicit val p: Parameters = Parameters.empty
  val fftModule = LazyModule(new AXI4FFTBlock(paramsFFT, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4, configInterface = false) with AXI4FFTStandaloneBlock)
  chisel3.Driver.execute(args, ()=> fftModule.module)
}

object FFTDspBlockAXI4WithConfig extends App
{
  val paramsFFT = FFTParams.fixed(
    dataWidth = 16,
    twiddleWidth = 16,
    numPoints = 1024,
    useBitReverse = false,
    runTime = true,
    numAddPipes = 1,
    numMulPipes = 1,
    expandLogic = Array.fill(log2Up(1024))(0),
    keepMSBorLSB = Array.fill(log2Up(1024))(true),
    overflowReg = true,
    keepMSBorLSBReg = true,
    binPoint = 0,
    minSRAMdepth = 1024
  )
  val baseAddress = 0x500 // just to check if verilog code is succesfully generated or not
  implicit val p: Parameters = Parameters.empty
  val fftModule = LazyModule(new AXI4FFTBlock(paramsFFT, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4, configInterface = true) with AXI4FFTStandaloneBlock {
     val config_in = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 1)))
     configNode.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 1)) := config_in
     val config = InModuleBody { config_in.makeIO() }
  })
  chisel3.Driver.execute(args, ()=> fftModule.module)
}
