// SPDX-License-Identifier: Apache-2.0

package fft

import dsptools.DspTester
import dsptools.numbers._

import chisel3.iotesters.PeekPokeTester
import chisel3.experimental.FixedPoint
import chisel3.util.log2Up

import breeze.math.Complex
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.DenseVector

import scala.math.pow
import scala.util.Random
import scala.io.Source

/**
 * Contains useful functions for testing sdf-fft generator
 */

trait SDFFFTTester[T <: chisel3.Data] extends HasTesterUtil[SDFFFT[T]] with HasSignalUtils { this: PeekPokeTester[SDFFFT[T]] =>
  def c: SDFFFT[T] //abstract need to be defined in classes which extends this trait (val)

  def simpleTestFFT(tol: Int = 6, testSignal: Seq[Complex], params: FFTParams[T]): (Seq[Complex],Seq[Complex]) = {

    val cyclesWait = 5 * params.numPoints
    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)
    val inp = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val out = if (params.decimType == DITDecimType) fourierTr(DenseVector(testSignal.toArray)).toScalaVector else bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector)
    //out.map(c => println(c.toString))

    // used only when trimEnable signal is active
    val dataWidthIn = params.protoIQ.real.getWidth
    val dataWidthOut= params.protoIQOut.real.getWidth
    val div2Num = numStages - (dataWidthOut - dataWidthIn)

    val trimEnableDiv = if (div2Num > 0) pow(2, div2Num) else 1
    val scalingFactor = if (params.trimEnable) trimEnableDiv else pow(2, params.expandLogic.filter(_ == 0).size).toInt
    // println("Scaling factor is:")
    // println(scalingFactor.toString)

    val input = inp.iterator
    var output = Seq[Complex]()

    poke(c.io.in.valid, 0)
    poke(c.io.out.ready, 0)
    step(2)
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)

    while (output.length < fftSize) {
      if (input.hasNext && peek(c.io.in.ready)) {
        poke(c.io.in.bits, input.next())
      }
      if (peek(c.io.out.valid)) {
        params.protoIQ.real match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, out(output.length)/scalingFactor) }
          case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, out(output.length)/scalingFactor) }
        }
        output = output :+ peek(c.io.out.bits)
      }
      step(1)
    }
    poke(c.io.in.valid, 0)
    step(inp.length)
    reset(2)
    val bitrevorderOutput = if (params.decimType == DIFDecimType) bitrevorder_data(output.toVector) else output
    val scalafft = fourierTr(DenseVector(testSignal.toArray)).toScalaVector

    /*println("Expected result:")
    out.map(c => println((c/scalingFactor).toString))

    println("Result:")
    output.map(c => println((c/scalingFactor).toString))*/

//     println("Bitrevorder data is:")
//     bitrevorderOutput.map(c => println((c*scalingFactor).toString))
//     println("Scala fft:")
//     scalafft.map(c => println(c.toString))
    (bitrevorderOutput.map(c => c*scalingFactor), scalafft)
  }
  
  /**
   * The function testFFT_IFFT tests fft and ifft - control register fftDir is included.
   * Flushing data is used before ifft calculation.
   */
  def testFFT_IFFT(tol: Int = 6, testSignal: Seq[Complex], params: FFTParams[T]): (Seq[Complex], Seq[Complex]) = {
    require(params.fftDirReg == true, "Control register ffDir should have been enabled for this tester")

    val cyclesWait = 5 * params.numPoints
    var idx = 0
    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)
    val inp_fft = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val out_fft = if (params.decimType == DITDecimType) fourierTr(DenseVector(testSignal.toArray)).toScalaVector else bitrevorder_data(fourierTr(DenseVector(inp_fft.toArray)).toScalaVector)

    val scalingFactor = pow(2, params.expandLogic.filter(_ == 0).size).toInt
    var output_fft = Seq[Complex]()
    val input_fft = inp_fft.iterator
    var cntValidIn = 0

    poke(c.io.in.valid, 0)
    poke(c.io.out.ready, 0)
    poke(c.io.fftDirReg.get, 1) //set fft
    step(3)
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)

    while (input_fft.hasNext && peek(c.io.in.ready)) {
      if (cntValidIn == (inp_fft.size - 1)) { 
        poke(c.io.lastIn, 1)
      }
      cntValidIn += 1
      poke(c.io.in.bits, input_fft.next())
      step(1)
    }
    cntValidIn = 0
    poke(c.io.in.valid, 0)
    poke(c.io.lastIn, 0) // flush data
    wait_for_assert(c.io.out.valid, cyclesWait)

    var cntValidOut = 0
    while (cntValidOut < inp_fft.size) {
      if (peek(c.io.out.valid) == true && cntValidOut < inp_fft.size) {
        params.protoIQ.real match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, out_fft(output_fft.length)/scalingFactor) }
          case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, out_fft(output_fft.length)/scalingFactor) }
        }
        output_fft = output_fft :+ peek(c.io.out.bits) // append new data
        cntValidOut +=1
      }
      step(1)
    }

    reset(2)
    step(2)
    //state machine is in sIdle state

    val bitrevorderOutput_fft = bitrevorder_data(output_fft) 
    val inp_ifft = bitrevorderOutput_fft
    val out_ifft = if (params.decimType == DIFDecimType) bitrevorder_data(testSignal) else testSignal
    // iFourierTr(DenseVector(bitrevorderOutput.toArray)).toScalaVector
    val input_ifft = inp_ifft.iterator
    var output_ifft = Seq[Complex]()

    poke(c.io.in.valid, 0)
    poke(c.io.out.ready, 0)
    poke(c.io.fftDirReg.get, 0) // set ifft mode
    step(5)
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)

    while (input_ifft.hasNext && peek(c.io.in.ready)) {
      if (cntValidIn == (inp_ifft.size - 1)) {
        poke(c.io.lastIn, 1)
      }
      cntValidIn += 1
      poke(c.io.in.bits, input_ifft.next())
      step(1)
    }
    poke(c.io.in.valid, 0)
    poke(c.io.lastIn, 0)

    var cnt = 0
    wait_for_assert(c.io.out.valid, cyclesWait)
    while (cnt < inp_ifft.size) {
      if (peek(c.io.out.valid) == true && cnt < inp_ifft.size) {
        params.protoIQ.real match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, out_ifft(cnt)/scalingFactor) }
          case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, out_ifft(cnt)/scalingFactor) }
        }
        output_ifft = output_ifft :+ peek(c.io.out.bits)
        cnt +=1 
      }
      step(1)
    }
    reset(2)
    val bitrevorderOutput_ifft = if (params.decimType == DIFDecimType) bitrevorder_data(output_ifft) else output_ifft
    (bitrevorderOutput_fft.map(c=>c*scalingFactor), bitrevorderOutput_ifft.map(c=>c*scalingFactor))
  }
  
  def testInitialStoring(tolLSBs: Int = 3, testSignal: Seq[Complex], params: FFTParams[T]) {
    require((params.numAddPipes + params.numMulPipes) != 0, s"This test requires that number of pipeline registers is not equal to zero")
    
    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)
    
    val cyclesWait = 20*params.numPoints // this value depends on number of pipeline registers

    val inp = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val out = if (params.decimType == DITDecimType) fourierTr(DenseVector(testSignal.toArray)).toScalaVector else bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector)
    
    val scalingFactor = pow(2, params.expandLogic.filter(_ == 0).size).toInt
    
    val input1 = inp.iterator
    var output1 = Seq[Complex]()
    
//    println("Expected result should be: ")
//    out.map(c => println((c/scalingFactor).toString))
    
    step(5)
    poke(c.io.in.valid, 0)
    poke(c.io.out.ready, 0)
    step(5)
    poke(c.io.in.valid, 1)
    
    var cntValidOut = 0
    var cntValidIn = 0
    
    while (input1.hasNext && peek(c.io.in.ready)) {
      poke(c.io.in.bits, input1.next())
      cntValidIn += 1
      step(1)
    }
    val input2 = inp.iterator
    
    cntValidIn = 0
    // if pipes are zero this loop is skipped
    while (input2.hasNext && peek(c.io.in.ready)) {
      poke(c.io.in.bits, input2.next())
      if (cntValidIn == inp.length-1) {
        poke(c.io.lastIn, 1)
      }
      cntValidIn += 1
      step(1)
    }
    poke(c.io.in.valid, 0)
    step(10)
    poke(c.io.out.ready, 1) // loading data is possible
    while (input2.hasNext && peek(c.io.in.ready)) {
      poke(c.io.in.valid, 1)
      poke(c.io.in.bits, input2.next())
      if (cntValidIn == inp.length-1){
        poke(c.io.lastIn, 1)
      }
      if (peek(c.io.out.valid)) {
        fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
        cntValidOut += 1
      }
      cntValidIn += 1
      step(1)
    }
    poke(c.io.in.valid, 0)
    poke(c.io.lastIn, 0)
    
    wait_for_assert(c.io.out.valid, cyclesWait)
    // flushing is activated here
    while (peek(c.io.out.valid)) {
      if (cntValidOut == params.numPoints)
        cntValidOut = 0
      fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
      cntValidOut +=1
      step(1)
    }
    
    while (peek(c.io.out.valid)) {
      if (cntValidOut == params.numPoints)
        cntValidOut = 0
      fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
      cntValidOut +=1
      step(1)
    }
    step(2)
  }

  /**
   * Tests fft streaming with and without flush
   * Does not return data!
   */
  def testFFT(tolLSBs: Int = 3, testSignal: Seq[Complex], params: FFTParams[T]) {
    
    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)
    
    val cyclesWait = 20*params.numPoints // this value depends on number of pipeline registers

    val inp = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val out = if (params.decimType == DITDecimType) fourierTr(DenseVector(testSignal.toArray)).toScalaVector else bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector)
    
    val scalingFactor = pow(2, params.expandLogic.filter(_ == 0).size).toInt
    
    val input1 = inp.iterator
    val input2 = inp.iterator
    var output1 = Seq[Complex]()
    var output2 = Seq[Complex]()

    step(5)
    poke(c.io.in.valid, 0)
    poke(c.io.out.ready, 0)
    step(5)
    poke(c.io.out.ready, 1)
    //poke(c.io.in.valid, 1)
    
    var cntValidOut = 0
    var cntValidIn = 0
    
    while (input1.hasNext && peek(c.io.in.ready)) {
      poke(c.io.in.valid, 0)
      val delay = Random.nextInt(5)
      step(delay)
      poke(c.io.in.valid, 1)
      ////////////////////////////////////
      if (cntValidIn == inp.length-1) {
        poke(c.io.lastIn, 1)
      }
      ///////////////////////////////////
      poke(c.io.in.bits, input1.next())
      cntValidIn += 1
      if (peek(c.io.out.valid) == true) {
        fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits, out(output1.length)/scalingFactor) }
        output1 = output1 :+ peek(c.io.out.bits) // only can happen if number of pipeline registers is zero
        cntValidOut +=1
      }
      step(1)
    }
    cntValidIn = 0
    
//     while (input2.hasNext && peek(c.io.in.ready)) {
//       if (cntValidOut == inp.size) {
//         cntValidOut = 0
//       }
//       poke(c.io.in.valid, 1)
//       if (cntValidIn == inp.length-1) {
//         poke(c.io.lastIn, 1)
//       }
//       poke(c.io.in.bits, input2.next())
//       cntValidIn += 1
//       if (peek(c.io.out.valid) == true) {
//         if (output1.length < inp.size) {
//           fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits, out(output1.length)/scalingFactor) }
//           output1 = output1 :+ peek(c.io.out.bits)
//         }
//         else {
//           fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits, out(output2.length)/scalingFactor) }
//           output2 = output2 :+ peek(c.io.out.bits)
//         }
//         cntValidOut +=1
//       }
//       step(1)
//     }
    
    if (cntValidOut == inp.size) {
      cntValidOut = 0
    }
    poke(c.io.in.valid, 0)
    poke(c.io.lastIn, 0)

    wait_for_assert(c.io.out.valid, cyclesWait)
    
    while (cntValidOut < inp.size) {
      if (peek(c.io.out.valid) == true && cntValidOut < inp.size) {
        if (cntValidOut == inp.size - 1) {
          expect(c.io.lastOut, 1)
        }
        fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits, out(output1.length)/scalingFactor) }
        output1 = output1 :+ peek(c.io.out.bits)
        cntValidOut +=1
      }
      step(1)
    }
    step(2)
    
    val input3 = inp.iterator
    var output3 = Seq[Complex]()
    
    wait_for_assert(c.io.in.ready, cyclesWait)
    // here idle state is active
    poke(c.io.in.valid, 1)
    while (output3.length < inp.size) {
      if (input3.hasNext && peek(c.io.in.ready)) {
        poke(c.io.in.bits, input3.next())
      }

      if (peek(c.io.out.valid)) {
        fixTolLSBs.withValue(tolLSBs) { expect(c.io.out.bits, out(output3.length)/scalingFactor) }
        output3 = output3 :+ peek(c.io.out.bits)
      }
      step(1)
    }
    val bitrevorderOutput = if (params.decimType == DIFDecimType) bitrevorder_data(output2) else output2
    
//     if (fftSize > 256) {
//       val scaledfft =  fourierTr(DenseVector(testSignal.toArray)).toScalaVector.map(c => c/scalingFactor)
//       plot_fft(bitrevorderOutput, scaledfft) //defined in HasTesterUtil.scala
//     }
//    println("Expected result should be: ")
//    out.map(c => println((c/scalingFactor).toString))
    step(2*inp.length)
  }
  def testBitReversal(tol: Int = 3, testSignal: Seq[Complex], params: FFTParams[T]) {
    
    // inp in natural, out in natural order
    val inp = testSignal
    val cyclesWait = 5 * params.numPoints
    val fftSize = c.params.numPoints
    val numStages = log2Up(fftSize)
    val out = fourierTr(DenseVector(inp.toArray)).toScalaVector 
    val scalingFactor = pow(2, c.params.expandLogic.filter(_ == 0).size).toInt

//    println("Expected result should be: ")
//    out.map(c => println((c/scalingFactor).toString))
    
    val input1 = inp.iterator
    val input2 = inp.iterator
    val input3 = inp.iterator
    val input4 = inp.iterator
    var cntValidOut = 0
    var cntValidIn = 0
    
    poke(c.io.in.valid, 0)
    poke(c.io.out.ready, 0)
    step(2)
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
    while (input1.hasNext && peek(c.io.in.ready)) {
      poke(c.io.in.bits, input1.next())
      step(1)
    }
    
    while (input2.hasNext && peek(c.io.in.ready)) {
      poke(c.io.in.bits, input2.next())
      step(1)
    }
    
    while (input3.hasNext && peek(c.io.in.ready)) {
      poke(c.io.in.bits, input3.next())
      if (peek(c.io.out.valid)) {
       c.params.protoIQ.real match {
        case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
          case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
        }
        cntValidOut += 1
      }
      step(1)
    }
    
    if (cntValidOut == fftSize)
      cntValidOut = 0
    
    poke(c.io.in.valid, 0)
    // think how to make this test better! 
    for (i <- 0 until 10) {
      if (peek(c.io.out.valid)) {
       c.params.protoIQ.real match {
        case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
          case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
        }
        if (cntValidOut == (fftSize-1))
          cntValidOut = 0
        else 
          cntValidOut += 1
      }
      step(1)
    }
    poke(c.io.in.valid, 1)
  
    while (input4.hasNext && peek(c.io.in.ready)) {
      if (cntValidIn == (inp.length - 1)) {
        poke(c.io.lastIn, 1)
      }
      poke(c.io.in.bits, input4.next())
      cntValidIn += 1
      if (peek(c.io.out.valid)) {
        c.params.protoIQ.real match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
          case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, out(cntValidOut)/scalingFactor) }
        }
        if (cntValidOut == (fftSize-1))
          cntValidOut = 0
        else 
          cntValidOut += 1
      }
      step(1)
    }
    cntValidOut = 0

    poke(c.io.lastIn, 0)
    step(40)
  }
  def testWindowFunctions(tol: Int = 6, testSignal: Seq[Complex], params: FFTParams[T]) = {
    
    val cyclesWait = 5 * params.numPoints
    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)
    
    val windowSeq = params.windowFunc match {
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
    
    val inWithWindowing = testSignal.zip(windowSeq).map { case(sig, win) => sig*win }
    
    val inp = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val out = if (params.decimType == DITDecimType) fourierTr(DenseVector(inWithWindowing.toArray)).toScalaVector else bitrevorder_data(fourierTr(DenseVector(inWithWindowing.toArray)).toScalaVector)
    
    val scalingFactor = pow(2, params.expandLogic.filter(_ == 0).size).toInt
    val input = inp.iterator
    var output = Seq[Complex]()
    
    poke(c.io.in.valid, 0)
    poke(c.io.out.ready, 0)
    step(2)
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)

    while (output.length < fftSize) {
      if (input.hasNext && peek(c.io.in.ready)) {
        poke(c.io.in.bits, input.next())
      }
      if (peek(c.io.out.valid)) {
        params.protoIQ.real match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, out(output.length)/scalingFactor) }
          case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, out(output.length)/scalingFactor) }
        }
        output = output :+ peek(c.io.out.bits)
      }
      step(1)
    }
    poke(c.io.in.valid, 0)
    step(inp.length)
    reset(2)
    val bitrevorderOutput = if (params.decimType == DIFDecimType) bitrevorder_data(output.toVector) else output
    val scalafft = fourierTr(DenseVector(inWithWindowing.toArray)).toScalaVector
    
    // uncomment to see effect of window functions
    // plot_fft(bitrevorderOutput, scalafft.map(c => c/scalingFactor), true)
    
    /*println("Expected result:")
    out.map(c => println((c/scalingFactor).toString))
    
    println("Result:")
    output.map(c => println((c/scalingFactor).toString))*/
  }
}

class FixedPointSDFFFTTester(val c: SDFFFT[FixedPoint]) extends DspTester(c) with SDFFFTTester[FixedPoint] {
}
class DspRealSDFFFTTester(val c: SDFFFT[DspReal]) extends DspTester(c) with SDFFFTTester[DspReal] {
}

class FixedSDFFFTTester {
  def fixedTesterSimple(params: FFTParams[FixedPoint], testSignal: Seq[Complex], tolLSBs: Int = 2): Boolean = {
	  dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")){ c => new FixedPointSDFFFTTester(c) {
	  val (cFFTtest, sFFTtest) = this.simpleTestFFT(tolLSBs, testSignal, params) } }
  }
   def dspRealTesterSimple(params: FFTParams[DspReal], testSignal: Seq[Complex], tolReal: Int = 12): Boolean = {
	  dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")){ c => new DspRealSDFFFTTester(c) {
	  val (cFFTtest, sFFTtest) = this.simpleTestFFT(tolReal, testSignal, params) } }
  }
  def fixedTester(params: FFTParams[FixedPoint], testSignal: Seq[Complex], tolLSBs: Int = 2, win: Boolean = false): Boolean = {
    dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")){ c => new FixedPointSDFFFTTester(c) { 
        if (params.useBitReverse) {
          this.testBitReversal(tolLSBs, testSignal, params)
        }
        else if (win) {
          updatableDspVerbose.withValue(false) {
            this.testWindowFunctions(tolLSBs, testSignal, params)
          }
        }
        else {
          this.testFFT(tolLSBs, testSignal, params)
        }
      } 
    }
  }
  def dspRealTester(params: FFTParams[DspReal], testSignal: Seq[Complex], tolReal: Int = 12): Boolean = {
    dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")){ c => new  DspRealSDFFFTTester(c) { 
    this.testFFT(tolReal, testSignal, params) } }
  }
}
