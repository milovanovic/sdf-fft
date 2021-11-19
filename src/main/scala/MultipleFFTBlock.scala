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

trait AXI4MultipleFFTsStandaloneBlock extends AXI4MultipleFFTsBlock[FixedPoint] {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val numIns = 2

  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until numIns) yield {
    implicit val valName = ValName(s"inIOs_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
    streamNode :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters())) :=
      in
    InModuleBody { in.makeIO() }
  }
  val outIOs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until numIns) yield {
    implicit val valName = ValName(s"outIOs_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out :=
      AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) :=
      streamNode
    InModuleBody { out.makeIO() }
  }
// For simulation i guess that this should be uncommented
//   val ioInNode1 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
//   val ioOutNode1 = BundleBridgeSink[AXI4StreamBundle]()
//
//   ioOutNode1 :=
//     AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
//     streamNode :=
//     BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) :=
//     ioInNode1
//
//   val in1 = InModuleBody { ioInNode1.makeIO() }
//   val out1 = InModuleBody { ioOutNode1.makeIO() }
//
//   val ioInNode2 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
//   val ioOutNode2 = BundleBridgeSink[AXI4StreamBundle]()
//
//   ioOutNode2 :=
//   AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
//   streamNode :=
//   BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) :=
//   ioInNode2
//
//   val in2 = InModuleBody { ioInNode2.makeIO() }
//   val out2 = InModuleBody { ioOutNode2.makeIO() }
}

abstract class MultipleFFTsBlock [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: FFTParams[T], beatBytes: Int, configInterface: Boolean = false) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

  val streamNode = AXI4StreamNexusNode(
    masterFn = (ms: Seq[AXI4StreamMasterPortParameters]) =>
      AXI4StreamMasterPortParameters(ms.map(_.masters).reduce(_ ++ _)),
    slaveFn = ss => {
      AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _))
    }
  )

  // add slave node for fft configuration
  val slaveParams = AXI4StreamSlaveParameters()
  val configNode  = if (configInterface == true) Some(AXI4StreamSlaveNode(slaveParams)) else None


  lazy val module = new LazyModuleImp(this) {
    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip

    val numStages = log2Ceil(params.numPoints)
    // Status registers
    val busy      = RegInit(false.B)
    // busy := fft.io.busy
    var commonFields = Seq[RegField]()

    val fftSize = if (params.runTime) Some(RegInit(numStages.U(log2Ceil(numStages + 1).W))) else None
    val configReg = if (params.runTime && configInterface) Some(RegInit(numStages.U(log2Ceil(numStages + 1).W))) else None
    val configMode = if (params.runTime && configInterface) Some(RegInit(false.B)) else None

    if (params.runTime) {
      fftSize.get.suggestName("fftSize")
      commonFields = commonFields :+ RegField(log2Ceil(numStages), fftSize.get,
      RegFieldDesc(name = "fftSize", desc = "contains fft size which is used for run time configurability control"))
      if (configInterface) {
        configMode.get.suggestName("configMode")
        commonFields = commonFields :+ RegField(1, configMode.get,
        RegFieldDesc(name = "configMode", desc = "Defines the way of fftSize configuration"))
      }
    }
    val keepMSBorLSBReg = if (params.keepMSBorLSBReg) Some(RegInit(0.U((numStages).W))) else None
    if (params.keepMSBorLSBReg) {
      keepMSBorLSBReg.get.suggestName("keepMSBorLSBReg")
      // fft.io.keepMSBorLSBReg.get := keepMSBorLSBReg.asBools
      commonFields = commonFields :+ RegField(numStages, keepMSBorLSBReg.get,
        RegFieldDesc(name = "keepMSBorLSBReg", desc = "defines scaling behaviour for each stage"))
    }
    val fftDirReg = if (params.fftDirReg)  Some(RegInit(true.B)) else None
    if (params.fftDirReg) {
      fftDirReg.get.suggestName("fftDir")
      commonFields = commonFields :+ RegField(1, fftDirReg.get,
        RegFieldDesc(name = "fftDir", desc = "transform direction: fft or ifft"))
    }
    val overflowReg = if (params.fftDirReg) Some(RegInit(0.U(log2Ceil(numStages).W))) else None
    if (params.overflowReg) {
      overflowReg.get.suggestName("overflowReg")
      // overflowReg := fft.io.overflow.get.asUInt
      commonFields = commonFields :+ RegField.r(log2Ceil(numStages), overflowReg.get,
        RegFieldDesc(name = "overflowReg", desc = "returns overflow status for the each stage"))
    }
    commonFields = commonFields :+ RegField.r(1, busy,
      RegFieldDesc(name = "busy", desc = "indicates if fft core is in the state sFlush or sProcess"))

    // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(commonFields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)

    println(s"outs.length = ${outs.length}")

    for ((in, inIdx) <- ins.zipWithIndex) {
      val fft = Module(new SDFFFT(params))
      fft.io.in.valid    := in.valid
      require(in.bits.data.getWidth >= fft.io.in.bits.imag.getWidth + fft.io.in.bits.real.getWidth, "Data width is not appropriately set")

      // if timing problem occurs then add unique register for each fft module
      if (params.runTime) {
        if (configInterface) {
          when (configMode.get === true.B) {
            when (configNode.get.in(0)._1.fire()) {
              configReg.get := configNode.get.in(0)._1.bits.data.asUInt
            }
            configNode.get.in(0)._1.ready := ~busy
            fft.io.fftSize.get := configReg.get
          }
          .otherwise {
            fft.io.fftSize.get := fftSize.get
          }
          configNode.get.in(0)._1.bits.last := false.B //DontCare*/
        }
        else {
          fft.io.fftSize.get := fftSize.get
        }
      }
      if (params.fftDirReg) {
        fft.io.fftDirReg.get := fftDirReg.get
      }
      if (params.keepMSBorLSBReg) {
        fft.io.keepMSBorLSBReg.get := keepMSBorLSBReg.get.asBools
      }
      // custom packager, it is assumed that preproc block precedes and pack data on this way
      fft.io.in.bits.imag  := in.bits.data(in.bits.data.getWidth/2 - 1, 0).asTypeOf(params.protoIQ.imag)
      fft.io.in.bits.real  := in.bits.data(in.bits.data.getWidth-1, in.bits.data.getWidth/2).asTypeOf(params.protoIQ.real)
      in.ready           := fft.io.in.ready
      fft.io.lastIn      := in.bits.last

      outs(inIdx).valid := fft.io.out.valid
      outs(inIdx).bits.data := fft.io.out.bits.asUInt
      outs(inIdx).bits.last := fft.io.lastOut
      fft.io.out.ready := outs(inIdx).ready
    }
  }
}

class AXI4MultipleFFTsBlock[T <: Data : Real: BinaryRepresentation](params: FFTParams[T], address: AddressSet, _beatBytes: Int = 4, configInterface: Boolean)(implicit p: Parameters) extends MultipleFFTsBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes, configInterface) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

class TLMultipleFFTsBlock[T <: Data : Real: BinaryRepresentation](val params: FFTParams[T], address: AddressSet, beatBytes: Int = 4, configInterface: Boolean)(implicit p: Parameters) extends MultipleFFTsBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](params, beatBytes) with TLDspBlock with TLHasCSR {
  val devname = "TLMultipleFFTsBlock"
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

object MultipleFFTsDspBlockTL extends App
{
  val paramsMultipleFFTs = FFTParams.fixed(
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
  val fftModule = LazyModule(new TLMultipleFFTsBlock(paramsMultipleFFTs, AddressSet(baseAddress + 0x100, 0xFF), beatBytes = 4, configInterface = false) with dspblocks.TLStandaloneBlock)

  chisel3.Driver.execute(args, ()=> fftModule.module)
}

object MultipleFFTsDspBlockAXI4 extends App
{
  val paramsMultipleFFTs = FFTParams.fixed(
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
  val fftModule = LazyModule(new AXI4MultipleFFTsBlock(paramsMultipleFFTs, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4, configInterface = false) with AXI4MultipleFFTsStandaloneBlock)
  chisel3.Driver.execute(args, ()=> fftModule.module)
}

object MultipleFFTsDspBlockAXI4WithConfig extends App
{
  val paramsMultipleFFTs = FFTParams.fixed(
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
  val fftModule = LazyModule(new AXI4MultipleFFTsBlock(paramsMultipleFFTs, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4, configInterface = true) with AXI4MultipleFFTsStandaloneBlock {
     val config_in = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 1)))
     configNode.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 1)) := config_in
     val config = InModuleBody { config_in.makeIO() }
  })
  chisel3.Driver.execute(args, ()=> fftModule.module)
}
