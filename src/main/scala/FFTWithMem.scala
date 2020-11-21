package fft

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dspblocks._ // included because of DspQueue
import dsptools._
import dsptools.numbers._

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

class FFTWithMem[T <: Data : Real : BinaryRepresentation](val paramsFFT: FFTParams[T], val fftAddress: AddressSet, val fftRAM: AddressSet, val memAddress: AddressSet, val protoMem: T, val beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  
 // Instantiate lazy modules
  val fft = LazyModule(new AXI4FFTBlock(paramsFFT, fftAddress, beatBytes))
  //LazyModule(new FFTBlockWithWindowing(csrAddress = fftAddress, ramAddress = fftRAM, paramsFFT, beatBytes = beatBytes))
  
  val memForTest = LazyModule(new AXI4MemForTestingFFT(DspComplex(protoMem), memAddress, beatBytes, paramsFFT.numPoints))
  
  fft.streamNode := memForTest.streamNode
  
  val ioStreamNode = BundleBridgeSink[AXI4StreamBundle]()
        ioStreamNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := fft.streamNode
        val outStream = InModuleBody { ioStreamNode.makeIO() }
        
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)

  lazy val blocks = Seq(fft, memForTest)
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  for (b <- blocks) {
    b.mem.foreach { _ := bus.node }
  }
  
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
    BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
    ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
  
  lazy val module = new LazyModuleImp(this)
}

object FFTWithMemApp extends App
{
   val fftParams = FFTParams.fixed(
      dataWidth = 16,
      binPoint  = 0,
      twiddleWidth = 16,
      numPoints = 1024,
      useBitReverse  = false,
      runTime = true,
      numAddPipes = 1,
      numMulPipes = 1,
      expandLogic = Array.fill(log2Up(1024))(0),
      keepMSBorLSB = Array.fill(log2Up(1024))(true),
      minSRAMdepth = 1024 // just do not use ShiftRegisterMem
    )
   val fftAddress      = AddressSet(0x60000100, 0xFF)
   val fftRAM          = AddressSet(0x60002000, 0xFFF)
   val memAddress      = AddressSet(0x60003000, 0xF)
   val  protoMem        = FixedPoint(16.W, 0.BP)
   val  beatBytes         = 4
  
  implicit val p: Parameters = Parameters.empty
  val testModule = LazyModule(new FFTWithMem(fftParams, fftAddress, fftRAM, memAddress, protoMem, beatBytes) )
  
  chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "FFTWithMem"), ()=> testModule.module)
}

/*
trait FFTWithMemStandaloneBlock extends FFTWithMem {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
  
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode

  val out = InModuleBody { ioOutNode.makeIO() }
}

case class FFTWithMemParams(
  val fftParams    : FFTParams[FixedPoint],
  val fftAddress   : AddressSet,
  val fftRAM       : AddressSet,
  val memAddress   : AddressSet,
  val protoMem     : FixedPoint,
  val beatBytes    : Int
)

class FFTWithMem(params: FFTWithMemParams)(implicit p: Parameters) extends LazyModule {
  
 // Instantiate lazy modules
  val fft = LazyModule(new FFTBlockWithWindowing(csrAddress = params.fftAddress, ramAddress = params.fftRAM, params.fftParams, beatBytes = params.beatBytes))
  
  val memForTest = LazyModule(new AXI4MemForTestingFFT(DspComplex(params.protoMem), params.memAddress, params.beatBytes, params.fftParams.numPoints))
  
  val masterParams = AXI4StreamMasterParameters(
		name = "AXI4 Stream memory",
		n = 4,
		u = 0,
		numMasters = 1
	)
  val streamNode = AXI4StreamMasterNode(masterParams)
  streamNode := fft.streamNode := memForTest.streamNode
  
  lazy val blocks = Seq(fft, memForTest)
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  for (b <- blocks) {
    b.mem.foreach { _ := bus.node }
  }
 
  lazy val module = new LazyModuleImp(this)
}*/

// object FFTWithMemApp extends App
// {
//   val params = FFTWithMemParams(
//     fftParams = FFTParams.fixed(
//       dataWidth = 16,
//       binPoint  = 0,
//       twiddleWidth = 16,
//       numPoints = 1024,
//       useBitReverse  = false,
//       runTime = true,
//       numAddPipes = 1,
//       numMulPipes = 1,
//       expandLogic = Array.fill(log2Up(1024))(0),
//       keepMSBorLSB = Array.fill(log2Up(1024))(true),
//       minSRAMdepth = 1024 // just do not use ShiftRegisterMem
//     ),
//     fftAddress      = AddressSet(0x60000100, 0xFF),
//     fftRAM          = AddressSet(0x60002000, 0xFFF),
//     memAddress      = AddressSet(0x60002050, 0xF),
//     protoMem        = FixedPoint(16.W, 0.BP),
//     beatBytes         = 4
//   )
//   implicit val p: Parameters = Parameters.empty
//   val testModule = LazyModule(new FFTWithMem(params) with FFTWithMemStandaloneBlock)
//   
//   chisel3.Driver.execute(Array("--target-dir", "verilog", "--top-name", "FFTWithMem"), ()=> testModule.module)
// }


