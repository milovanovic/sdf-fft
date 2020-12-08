// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

abstract class FFTBlock [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: FFTParams[T], beatBytes: Int, hasMemMap: Boolean) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {
  // Define regmap and addCSR functions needed for memory mapped registers
  def regmap(mapping: RegField.Map*)
  def addCSR(address: Int, field: Seq[RegField]): Unit = {}

  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    //  FFT module
    val fft = Module(new SDFFFT(params))

    // Number of stages
    val numStages = log2Ceil(params.numPoints)
    
    // Generate FFT inputs
    val io = IO(new Bundle {
        val fftSize = if (fft.io.fftSize != None && hasMemMap == false) Some(Input(UInt((log2Ceil(numStages)).W))) else None
        val fftDir  = if (fft.io.fftDirReg != None && hasMemMap == false) Some(Input(Bool())) else None
        val flushData = if (hasMemMap == false)    Some(Input(Bool())) else None
        val keepMSBorLSBReg = if (fft.io.keepMSBorLSBReg != None && hasMemMap == false) Some(Input(Vec(log2Ceil(params.numPoints),Bool()))) else None

        val busy = if (hasMemMap == false) Some(Output(Bool())) else None
        val overflowReg = if (fft.io.overflow != None && hasMemMap == false) Some(Output(UInt((log2Ceil(params.numPoints)).W)))  else None
    })
    // Case with memory mapped registers
    if (hasMemMap){
      // Control registers
      val fftSize         = RegInit(0.U(log2Ceil(numStages).W))
      val fftDir          = RegInit(true.B)
//      val flushData       = RegInit(false.B)
      val keepMSBorLSBReg = RegInit(0.U((numStages).W))

      // Status registers
      val busy            = RegInit(false.B)
      val overflowReg     = RegInit(0.U(log2Ceil(numStages).W))

      // Connect FFT signals to registers
      //fft.io.flushDataOut := flushData
      busy := fft.io.busy
      if (params.runTime) {
        fft.io.fftSize.get := fftSize
      }
      if (params.keepMSBorLSBReg) {
        fft.io.keepMSBorLSBReg.get := keepMSBorLSBReg.asBools
      }
      if (params.fftDirReg) {
        fft.io.fftDirReg.get := fftDir
      }
      if (params.overflowReg) {
        overflowReg := fft.io.overflow.get.asUInt
      }

      // Define register fields
      val fields = Seq(
        // settable registers
        RegField(log2Ceil(numStages), fftSize,
          RegFieldDesc(name = "fftSize", desc = "contains fft size which is used for run time configurability control")),
        RegField(1, fftDir, 
          RegFieldDesc(name = "fftDir", desc = "transform direction: fft or ifft")),
        //RegField(1, flushData,
          //RegFieldDesc(name = "flushData", desc = "trigger flushing")),
        RegField(numStages, keepMSBorLSBReg,
          RegFieldDesc(name = "keepMSBorLSBReg", desc = "defines scaling behaviour for each stage")),
        // read-only status registers
        RegField.r(1, busy, 
          RegFieldDesc(name = "busy", desc = "indicates if fft core is in the state flush data")),
        RegField.r(log2Ceil(numStages), overflowReg,
          RegFieldDesc(name = "overflowReg", desc = "returns overflow status for the each stage"))
      )

      // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
      regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
    }
    
    // Case without memory mapped registers
    else {
      // Connect FFT signals to IO signals
     // fft.io.flushDataOut := io.flushData.get
      io.busy.get         := fft.io.busy
      if (fft.io.fftSize != None) {
        fft.io.fftSize.get := io.fftSize.get
      }
      if (fft.io.keepMSBorLSBReg != None) {
        fft.io.keepMSBorLSBReg.get := io.keepMSBorLSBReg.get
      }
      if (fft.io.fftDirReg != None) {
        fft.io.fftDirReg.get := io.fftDir.get
      }
      if (fft.io.overflow != None) {
        io.overflowReg.get := fft.io.overflow.get.asUInt
      }
    }
    
    // Connect inputs
    fft.io.in.valid    := in.valid
    fft.io.in.bits     := in.bits.data.asTypeOf(params.protoIQ)
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


class AXI4FFTBlock[T <: Data : Real: BinaryRepresentation](params: FFTParams[T], address: AddressSet, _beatBytes: Int = 8, hasAXI4: Boolean)(implicit p: Parameters) extends FFTBlock[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, _beatBytes, hasAXI4) with AXI4DspBlock {
  override val mem = if (hasAXI4) Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes)) else None
  override def regmap(mapping: (Int, Seq[RegField])*): Unit = if (hasAXI4) mem.get.regmap(mapping:_*) else None
}

class TLFFTBlock[T <: Data : Real: BinaryRepresentation](val params: FFTParams[T], address: AddressSet, beatBytes: Int = 4, hasTL: Boolean)(implicit p: Parameters) extends FFTBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](params, beatBytes, hasTL) with TLDspBlock {
  val devname = "TLFFTBlock"
  val devcompat = Seq("fft", "radardsp")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem = if (hasTL) Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = beatBytes)) else None
  override def regmap(mapping: (Int, Seq[RegField])*): Unit = if (hasTL) mem.get.regmap(mapping:_*) else None
}

object FFTDspBlockWithTL extends App
{
  // here just define parameters
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
  val fftModule = LazyModule(new TLFFTBlock(paramsFFT, AddressSet(baseAddress + 0x100, 0xFF), beatBytes = 4, hasTL = true) with dspblocks.TLStandaloneBlock)
  // check parameters of the TLStandaloneBlock
  
  chisel3.Driver.execute(args, ()=> fftModule.module) // generate verilog code
}

object FFTDspBlockWithAXI4 extends App
{
  // here just define parameters
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
  val baseAddress = 0x500 // just to check if verilog code is succesfully generated or not
  implicit val p: Parameters = Parameters.empty
  val fftModule = LazyModule(new AXI4FFTBlock(paramsFFT, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4, hasAXI4 = true) with dspblocks.AXI4StandaloneBlock {
    override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  })
  chisel3.Driver.execute(args, ()=> fftModule.module) // generate verilog code
}

object FFTDspBlockWithoutAXI4 extends App
{
  // here just define parameters
  val paramsFFT = FFTParams.fixed(
    dataWidth = 16,
    twiddleWidth = 16,
    numPoints = 512,
    runTime = false,
    expandLogic = Array.fill(log2Up(512))(1),
    keepMSBorLSB = Array.fill(log2Up(512))(true),
    overflowReg = false,
    keepMSBorLSBReg = false,
    binPoint = 1
  )
  val baseAddress = 0x500 // just to check if verilog code is succesfully generated or not
  implicit val p: Parameters = Parameters.empty
  val fftModule = LazyModule(new AXI4FFTBlock(paramsFFT, AddressSet(baseAddress + 0x100, 0xFF), _beatBytes = 4, hasAXI4 = false) with dspblocks.AXI4StandaloneBlock {
    override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  })
  chisel3.Driver.execute(args, ()=> fftModule.module) // generate verilog code
}
