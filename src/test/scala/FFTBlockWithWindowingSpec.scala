// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.experimental._
import chisel3.util._

import chisel3.iotesters.Driver
import chisel3.iotesters.PeekPokeTester
import dspblocks.{AXI4DspBlock, AXI4StandaloneBlock}
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import breeze.math.Complex
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg._
import org.scalatest.{FlatSpec, Matchers}
import scala.math.{Pi, pow}

trait FFTStandaloneBlock extends FFTBlockWithWindowing[FixedPoint] {
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

class FFTBlockWithWindowingTester
(
  dut: FFTBlockWithWindowing[FixedPoint] with FFTStandaloneBlock,
  csrAddress: AddressSet,
  ramAddress: AddressSet,
  windowFunc: WindowFunctionType = WindowFunctionTypes.Blackman(),
  beatBytes : Int = 4,
  freq      : Double = 15.54/1024,
  silentFail: Boolean = false
) extends PeekPokeTester(dut.module) with AXI4StreamModel with AXI4MasterModel with HasSignalUtils {
  //override def memAXI: AXI4Bundle = dut.ioMem.get.getWrappedValue
  def memAXI: AXI4Bundle = dut.ioMem.get

  val mod = dut.module
  val params = dut.params
  val fftSize = params.numPoints
  val master = bindMaster(dut.in.getWrappedValue)//bindMaster(dut.in)
  
  val windowSeq = windowFunc match {
    case WindowFunctionTypes.Hamming(_, alpha, beta, _) => WindowFunctions.hammingWindow(params.numPoints, alpha, beta)
    case WindowFunctionTypes.Hanning(_, _) => WindowFunctions.hanningWindow(params.numPoints)
    case WindowFunctionTypes.Blackman(_, a0, a1, a2, _) => WindowFunctions.blackmanWindow(params.numPoints, a0, a1, a2)
    case WindowFunctionTypes.Triangular(_, _) => WindowFunctions.triangularWindow(params.numPoints)
    case WindowFunctionTypes.User(_, userWindow) => {
      require(userWindow.length == params.numPoints, "Length of specified window function is not the same as fft size")
      userWindow
    }
    case WindowFunctionTypes.None(_) => Seq.fill(params.numPoints)(1.0)
  }
  //window.map(c => println(c))
  val inData = getTone(numSamples = fftSize, freq).map(c => c * scala.math.pow(2, 14).toInt)
  //val inData = genRandRealSignal(numSamples = fftSize).map(c => (c * scala.math.pow(2, 14)).toInt)
  
//   println("Input data is:")
//   inData.map(c => println(c.toString))

  
  val inWithWindowing = inData.zip(windowSeq).map { case(sig, win) => sig*win }
  println("Input data with windowing:")
  inWithWindowing.map(c => println(c.toString))
  
  val fftScala = fourierTr(DenseVector(inWithWindowing.toArray)).toScalaVector
//   println("Output data is:")
//   fftScala.map(c => println((c/fftSize).toString))

  val absFFTScala = fftScala.map(c => Complex(c.real/fftSize, c.imag/fftSize).abs.toInt).toSeq
  val axi4StreamIn = formAXI4StreamComplexData(inData, 16) //formAXI4StreamRealData(inData, 16)
  val windowHDW = windowSeq.map(c => BigDecimal(c * (1 << (params.protoWin.getWidth - 2))).toBigInt)
 
  step(1)
  poke(dut.out.ready, true.B)
  var cycle = 0
  
  // enable windowing
  memWriteWord(csrAddress.base + 2*beatBytes, BigInt(1))
  
  // write window function to RAM
  while (cycle < fftSize) {
    memWriteWord(ramAddress.base + cycle*beatBytes, windowHDW(cycle))
    cycle += 1
  }
  
  step(5)
  val expectedDepth = dut.params.numPoints
  
  // add one more stream
 // master.addTransactions((0 until axi4StreamIn.size).map(i => AXI4StreamTransaction(data = axi4StreamIn(i))))
  master.addTransactions(axi4StreamIn.zipWithIndex.map { case (data, idx) => AXI4StreamTransaction(data = data,  last = if (idx == axi4StreamIn.length - 1) true else false) })
  
  //step(1)
  // this is not visible while master is binded to stream model
  //poke(dut.in.bits.last, false)
  
  var outSeq = Seq[Int]()
  var peekedVal: BigInt = 0
  var realSeq = Seq[Int]()
  var imagSeq = Seq[Int]()
  var tmpReal: Short = 0
  var tmpImag: Short = 0

  // check only one fft window 
  while (outSeq.length < fftSize) {
    if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
      peekedVal = peek(dut.out.bits.data)
      outSeq = outSeq :+ peekedVal.toInt
      tmpReal = (peekedVal.toInt / pow(2,16)).toShort
      tmpImag = (peekedVal.toInt - (tmpReal.toInt * pow(2,16))).toShort
      realSeq = realSeq :+ tmpReal.toInt
      imagSeq = imagSeq :+ tmpImag.toInt
    }
    step(1)
  }
  
//   realSeq.map(c => println(c.toString))
//   imagSeq.map(c => println(c.toString))
  
  val complexOut = realSeq.zip(imagSeq).map { case (real, imag) => Complex(real, imag) }
  
  // check tolerance
  val complexScala = fftScala.map(c => Complex((c.real/fftSize), c.imag/fftSize))
  checkFFTError(complexOut, complexScala)
  
  // plot fft 
  plot_fft(complexOut, complexScala, dB = true)
  
  // if fftSize is equal to 1024 then test run time configurability without windowing
  if (fftSize == 1024) {
    val inData256 = getTone(numSamples = 256, freq).map(c => c * scala.math.pow(2, 14).toInt)
    val axi4StreamIn256 = formAXI4StreamComplexData(inData256, 16)
    
    step(10)
    
    memWriteWord(csrAddress.base, BigInt(log2Up(256)))
    memWriteWord(csrAddress.base + 2*beatBytes, BigInt(0))
    
    master.addTransactions(axi4StreamIn256.zipWithIndex.map { case (data, idx) => AXI4StreamTransaction(data = data,  last = if (idx == axi4StreamIn256.length - 1) true else false) })
    
    outSeq = Seq()
    realSeq = Seq()
    imagSeq = Seq()

    // check only one fft window 
    while (outSeq.length < 256) {
      if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
        peekedVal = peek(dut.out.bits.data)
        outSeq = outSeq :+ peekedVal.toInt
        tmpReal = (peekedVal.toInt / pow(2,16)).toShort
        tmpImag = (peekedVal.toInt - (tmpReal.toInt * pow(2,16))).toShort
        realSeq = realSeq :+ tmpReal.toInt
        imagSeq = imagSeq :+ tmpImag.toInt
      }
      step(1)
    }
    
    val fftScala256 = fourierTr(DenseVector(inData256.toArray)).toScalaVector
    println("Output data for smaller number of points:")
    fftScala256.map(c => println((c/256).toString))

    val complexOut256 = realSeq.zip(imagSeq).map { case (real, imag) => Complex(real, imag) }
    // check tolerance
    val complexScala256 = fftScala256.map(c => Complex(c.real/256, c.imag/256))
    checkFFTError(complexOut256, complexScala256)
    
    // plot fft 
    plot_fft(complexOut256, complexScala256, dB = true)
    
    
    val inData64 = getTone(numSamples = 64, freq).map(c => c * scala.math.pow(2, 14).toInt)
    val axi4StreamIn64 = formAXI4StreamComplexData(inData64, 16)
    
    step(10)
    
    memWriteWord(csrAddress.base, BigInt(log2Up(64)))
    memWriteWord(csrAddress.base + 2*beatBytes, BigInt(0)) // do not use windowing
    
    master.addTransactions(axi4StreamIn64.zipWithIndex.map { case (data, idx) => AXI4StreamTransaction(data = data,  last = if (idx == axi4StreamIn64.length - 1) true else false) })
    
    outSeq = Seq()
    realSeq = Seq()
    imagSeq = Seq()

    // check only one fft window 
    while (outSeq.length < fftSize/16) {
      if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
        peekedVal = peek(dut.out.bits.data)
        outSeq = outSeq :+ peekedVal.toInt
        tmpReal = (peekedVal.toInt / pow(2,16)).toShort
        tmpImag = (peekedVal.toInt - (tmpReal.toInt * pow(2,16))).toShort
        realSeq = realSeq :+ tmpReal.toInt
        imagSeq = imagSeq :+ tmpImag.toInt
      }
      step(1)
    }
    
    val fftScala64 = fourierTr(DenseVector(inData64.toArray)).toScalaVector
    println("Output data for smaller number of points:")
    fftScala64.map(c => println((c/64).toString))

    val complexOut64 = realSeq.zip(imagSeq).map { case (real, imag) => Complex(real, imag) }
    // check tolerance
    val complexScala64 = fftScala64.map(c => Complex(c.real/64, c.imag/64))
    checkFFTError(complexOut64, complexScala64)
    
    // plot fft 
    plot_fft(complexOut64, complexScala64, dB = true)
    
    
    val inData16 = getTone(numSamples = 16, freq).map(c => c * scala.math.pow(2, 14).toInt)
    val axi4StreamIn16 = formAXI4StreamComplexData(inData16, 16)
    
    step(10)
    
    memWriteWord(csrAddress.base, BigInt(log2Up(16)))
    memWriteWord(csrAddress.base + 2*beatBytes, BigInt(0)) // do not use windowing
    
    master.addTransactions(axi4StreamIn16.zipWithIndex.map { case (data, idx) => AXI4StreamTransaction(data = data,  last = if (idx == axi4StreamIn16.length - 1) true else false) })
    
    outSeq = Seq()
    realSeq = Seq()
    imagSeq = Seq()

    // check only one fft window 
    while (outSeq.length < 16) {
      if (peek(dut.out.valid) == 1 && peek(dut.out.ready) == 1) {
        peekedVal = peek(dut.out.bits.data)
        outSeq = outSeq :+ peekedVal.toInt
        tmpReal = (peekedVal.toInt / pow(2,16)).toShort
        tmpImag = (peekedVal.toInt - (tmpReal.toInt * pow(2,16))).toShort
        realSeq = realSeq :+ tmpReal.toInt
        imagSeq = imagSeq :+ tmpImag.toInt
      }
      step(1)
    }
    
    val fftScala16 = fourierTr(DenseVector(inData16.toArray)).toScalaVector

    val complexOut16 = realSeq.zip(imagSeq).map { case (real, imag) => Complex(real, imag) }
    // check tolerance
    val complexScala16 = fftScala16.map(c => Complex(c.real/16, c.imag/16))
    checkFFTError(complexOut64, complexScala64)
    
    // plot fft 
    plot_fft(complexOut16, complexScala16, dB = true)
  }
  step(100)
  stepToCompletion(expectedDepth*5, silentFail = silentFail)
  
}

class FFTBlockWithWindowingSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  val paramsFFT = FFTParams.fixed(
    dataWidth = 16,
    binPoint = 0,
    useBitReverse = true,
    twiddleWidth = 16,
    numPoints = 1024,
    numAddPipes = 1,
    numMulPipes = 1,
    runTime = true,
    expandLogic = Array.fill(log2Up(1024))(0),
    keepMSBorLSB = Array.fill(log2Up(1024))(true),
    minSRAMdepth = 1024
  )
  //BigDecimal(ex).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
  
  it should "Test fft dsp block with windowing" in {
    val lazyDut = LazyModule(new FFTBlockWithWindowing(csrAddress = AddressSet(0x010000, 0xFF), ramAddress = AddressSet(0x000000, 0xFFF), paramsFFT, beatBytes = 4) with FFTStandaloneBlock)
    
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new FFTBlockWithWindowingTester(lazyDut, csrAddress = AddressSet(0x010000, 0xFF), ramAddress = AddressSet(0x000000, 0xFFF), WindowFunctionTypes.None(), beatBytes = 4, silentFail = true)
    } should be (true)
  }
}
