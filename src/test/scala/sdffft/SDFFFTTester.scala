package dspblocks.sdffft

import chisel3._
import chiseltest.iotesters.PeekPokeTester
import chisel3.util.log2Up
import dsptools.misc.PeekPokeDspExtensions
import breeze.math.Complex
import breeze.signal.fourierTr
import breeze.linalg.DenseVector
import breeze.numerics.abs
import dspblocks.utils.dsp.windowing.{WindowFunctionTypes, WindowFunctions}

import scala.math.pow
import scala.util.Random

/**
  * Contains useful functions for testing sdf-fft generator
  */

case class SDFFFTTesterBase[T <: Data](override val dut: SDFFFT[T])
    extends PeekPokeTester(dut)
    with PeekPokeDspExtensions {
  def compare_data(expected: Complex, received: Complex, tol: Double) {
    assert(abs(expected.real - received.real) <= tol, "Mismatch!!!")
    assert(abs(expected.imag - received.imag) <= tol, "Mismatch!!!")
  }
}
class SDFFFTTester[T <: chisel3.Data](dut: SDFFFT[T])
    extends SDFFFTTesterBase(dut)
    with HasTesterUtil[SDFFFT[T]]
    with HasSignalUtils {

  def simpleTestFFT(
    tol:        Double = 0.02,
    testSignal: Seq[Complex],
    params:     FFTParams[T]
  ): (Seq[Complex], Seq[Complex]) = {

    val cyclesWait = 5 * params.numPoints
    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)
    val inp = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val windowSeq = params.windowFunc match {
      case WindowFunctionTypes.Hamming(_, alpha, beta, _) =>
        WindowFunctions.hammingWindow(params.numPoints, alpha, beta)
      case WindowFunctionTypes.Hanning(_, _) => WindowFunctions.hanningWindow(params.numPoints)
      case WindowFunctionTypes.Blackman(_, a0, a1, a2, _) =>
        WindowFunctions.blackmanWindow(params.numPoints, a0, a1, a2)
      case WindowFunctionTypes.Triangular(_, _) => WindowFunctions.triangularWindow(params.numPoints)
      case WindowFunctionTypes.User(_, userWindow) => {
        require(
          userWindow.length == params.numPoints,
          "Length of specified window function is not the same as fft size"
        )
        userWindow
      }
      case WindowFunctionTypes.None(_) => Seq.fill(params.numPoints)(1.0)
    }
    /*val tmp = bitrevorder_data(
      fourierTr(DenseVector(inp.toArray.zip(windowSeq).map { case (a, b) => a * b }).toScalaVector)
    )*/

    val out =
      if (params.decimType == DITDecimType)
        fourierTr(DenseVector(testSignal.toArray.zip(windowSeq).map { case (a, b) => a * b })).toScalaVector
      else {
        if (params.useBitReverse) {
          fourierTr(DenseVector(inp.toArray.zip(windowSeq).map { case (a, b) => a * b })).toScalaVector
        } else {
          bitrevorder_data(
            fourierTr(DenseVector(inp.toArray.zip(windowSeq).map { case (a, b) => a * b })).toScalaVector
          ) //bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector)
        }
      }

    // used only when trimEnable is active
    val dataWidthIn = params.protoIQ.real.getWidth
    val dataWidthOut = params.protoIQOut.real.getWidth
    val div2Num = numStages - (dataWidthOut - dataWidthIn)

    val trimEnableDiv = if (div2Num > 0) pow(2, div2Num) else 1
    val scalingFactor = if (params.trimEnable) trimEnableDiv else pow(2, params.expandLogic.filter(_ == 0).size).toInt
    // println("Scaling factor is:")
    // println(scalingFactor.toString)

    val input = inp.iterator
    var output = Seq[Complex]()
    var cntIn = 0

    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    step(2)
    poke(dut.io.out.ready, 1)
    poke(dut.io.in.valid, 1)
    if (params.windowFunc != WindowFunctionTypes.None()) {
      poke(dut.io.enWind.get, 1)
    }

    while (output.length < fftSize) {
      if (input.hasNext && peek(dut.io.in.ready) == 1) {
        poke(dut.io.in.bits, input.next())
      }
      if (peek(dut.io.out.valid) == 1) {
        params.protoIQ.real match {
          case uInt: UInt => expect(dut.io.out.bits, out(output.length) / scalingFactor)
          case sInt: SInt =>
            expect(dut.io.out.bits, out(output.length) / scalingFactor) //expect(dut.io.out.bits, in(out.length))
          case _ =>
            compare_data(
              Complex(out(output.length).real / scalingFactor, out(output.length).imag / scalingFactor),
              peek(dut.io.out.bits),
              tol
            )
        }
        /*params.protoIQ.real match {
           case dspR: DspReal =>
             realTolDecPts.withValue(tol) { expect(dut.io.out.bits, out(output.length) / scalingFactor) }
           case _ => fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(output.length) / scalingFactor) }
         }*/
        output = output :+ peek(dut.io.out.bits)
      }
      step(1)
    }
    poke(dut.io.in.valid, 0)
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
    //     scalafft.map(c => println(dut.toString))
    (bitrevorderOutput.map(c => c * scalingFactor), scalafft)
  }

  /**
    * The function testFFT_IFFT tests fft and ifft - control register fftDir is included.
    * Flushing data is used before ifft calculation.
    */
  def testFFT_IFFT(tol: Double = 6, testSignal: Seq[Complex], params: FFTParams[T]): (Seq[Complex], Seq[Complex]) = {
    require(params.fftDirReg, "Control register ffDir should have been enabled for this tester")

    val cyclesWait = 5 * params.numPoints
    var idx = 0
    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)
    val inp_fft = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val out_fft =
      if (params.decimType == DITDecimType) fourierTr(DenseVector(testSignal.toArray)).toScalaVector
      else bitrevorder_data(fourierTr(DenseVector(inp_fft.toArray)).toScalaVector)

    val scalingFactor = pow(2, params.expandLogic.count(_ == 0)).toInt
    var output_fft = Seq[Complex]()
    val input_fft = inp_fft.iterator
    var cntValidIn = 0

    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    poke(dut.io.fftDirReg.get, 1) //set fft
    step(3)
    poke(dut.io.out.ready, 1)
    poke(dut.io.in.valid, 1)

    while (input_fft.hasNext && peek(dut.io.in.ready) == 1) {
      if (cntValidIn == (inp_fft.size - 1)) {
        poke(dut.io.lastIn, 1)
      }
      cntValidIn += 1
      poke(dut.io.in.bits, input_fft.next())
      step(1)
    }
    cntValidIn = 0
    poke(dut.io.in.valid, 0)
    poke(dut.io.lastIn, 0) // flush data
    wait_for_assert(dut.io.out.valid, cyclesWait)

    var cntValidOut = 0
    while (cntValidOut < inp_fft.size) {
      if (peek(dut.io.out.valid) == 1 && cntValidOut < inp_fft.size) {
        /*params.protoIQ.real match {
           case dspR: DspReal =>
             realTolDecPts.withValue(tol) { expect(dut.io.out.bits, out_fft(output_fft.length) / scalingFactor) }
           case _ => fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out_fft(output_fft.length) / scalingFactor) }*/
        // }
        compare_data(
          Complex(out_fft(output_fft.length).real / scalingFactor, out_fft(output_fft.length).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        output_fft = output_fft :+ peek(dut.io.out.bits) // append new data
        cntValidOut += 1
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

    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    poke(dut.io.fftDirReg.get, 0) // set ifft mode
    step(5)
    poke(dut.io.out.ready, 1)
    poke(dut.io.in.valid, 1)

    while (input_ifft.hasNext && peek(dut.io.in.ready) == 1) {
      if (cntValidIn == (inp_ifft.size - 1)) {
        poke(dut.io.lastIn, 1)
      }
      cntValidIn += 1
      poke(dut.io.in.bits, input_ifft.next())
      step(1)
    }
    poke(dut.io.in.valid, 0)
    poke(dut.io.lastIn, 0)

    var cnt = 0
    wait_for_assert(dut.io.out.valid, cyclesWait)
    while (cnt < inp_ifft.size) {
      if (peek(dut.io.out.valid) == 1 && cnt < inp_ifft.size) {
        compare_data(
          Complex(out_ifft(cnt).real / scalingFactor, out_ifft(cnt).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        /*params.protoIQ.real match {
           case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits, out_ifft(cnt) / scalingFactor) }
           case _ => fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out_ifft(cnt) / scalingFactor) }
         }*/
        output_ifft = output_ifft :+ peek(dut.io.out.bits)
        cnt += 1
      }
      step(1)
    }
    reset(2)
    val bitrevorderOutput_ifft = if (params.decimType == DIFDecimType) bitrevorder_data(output_ifft) else output_ifft
    (bitrevorderOutput_fft.map(c => c * scalingFactor), bitrevorderOutput_ifft.map(c => c * scalingFactor))
  }

  def testInitialStoring(tol: Double = 3, testSignal: Seq[Complex], params: FFTParams[T]): Unit = {
    require(
      (params.numAddPipes + params.numMulPipes) != 0,
      s"This test requires that number of pipeline registers is not equal to zero"
    )

    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)

    val cyclesWait = 20 * params.numPoints // this value depends on number of pipeline registers

    val inp = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val out =
      if (params.decimType == DITDecimType) fourierTr(DenseVector(testSignal.toArray)).toScalaVector
      else bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector)
    //out.map(c => println(dut.toString))
    bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector).map(c => println(c.toString))
    val scalingFactor = pow(2, params.expandLogic.count(_ == 0)).toInt

    val input1 = inp.iterator
    var output1 = Seq[Complex]()

    //    println("Expected result should be: ")
    //    out.map(c => println((c/scalingFactor).toString))

    step(5)
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    step(5)
    poke(dut.io.in.valid, 1)

    var cntValidOut = 0
    var cntValidIn = 0

    while (input1.hasNext && peek(dut.io.in.ready) == 1) {
      poke(dut.io.in.bits, input1.next())
      cntValidIn += 1
      step(1)
    }
    val input2 = inp.iterator

    cntValidIn = 0
    // if pipes are zero this loop is skipped
    while (input2.hasNext && peek(dut.io.in.ready) == 1) {
      poke(dut.io.in.bits, input2.next())
      if (cntValidIn == inp.length - 1) {
        poke(dut.io.lastIn, 1)
      }
      cntValidIn += 1
      step(1)
    }
    poke(dut.io.in.valid, 0)
    step(10)
    poke(dut.io.out.ready, 1) // loading data is possible
    while (input2.hasNext && peek(dut.io.in.ready) == 1) {
      poke(dut.io.in.valid, 1)
      poke(dut.io.in.bits, input2.next())
      if (cntValidIn == inp.length - 1) {
        //   poke(dut.io.lastIn, 1)
      }
      if (peek(dut.io.out.valid) == 1) {
        //fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
        compare_data(
          Complex(out(cntValidOut).real / scalingFactor, out(cntValidOut).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        cntValidOut += 1
      }
      cntValidIn += 1
      step(1)
    }
    poke(dut.io.in.valid, 0)
    poke(dut.io.lastIn, 0)

    wait_for_assert(dut.io.out.valid, cyclesWait)
    // flushing is activated here
    while (peek(dut.io.out.valid) == 1) {
      if (cntValidOut == params.numPoints)
        cntValidOut = 0
      //fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
      cntValidOut += 1
      step(1)
    }

    while (peek(dut.io.out.valid) == 1) {
      if (cntValidOut == params.numPoints)
        cntValidOut = 0
      //fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
      cntValidOut += 1
      step(1)
    }
    step(2)
  }

  /**
    * Tests fft streaming with and without flush
    * Does not return data!
    */
  def testFFT(tol: Double = 0.002, testSignal: Seq[Complex], params: FFTParams[T]): Unit = {

    val fftSize = params.numPoints
    val numStages = log2Up(fftSize)

    val cyclesWait = 20 * params.numPoints // this value depends on number of pipeline registers

    val inp = if (params.decimType == DITDecimType) bitrevorder_data(testSignal) else testSignal
    val out =
      if (params.decimType == DITDecimType) fourierTr(DenseVector(testSignal.toArray)).toScalaVector
      else bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector)

    val scalingFactor = pow(2, params.expandLogic.count(_ == 0)).toInt

    val input1 = inp.iterator
    val input2 = inp.iterator
    var output1 = Seq[Complex]()
    var output2 = Seq[Complex]()

    step(5)
    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    step(5)
    poke(dut.io.out.ready, 1)
    //poke(dut.io.in.valid, 1)

    var cntValidOut = 0
    var cntValidIn = 0

    while (input1.hasNext && peek(dut.io.in.ready) == 1) {
      poke(dut.io.in.valid, 0)
      val delay = Random.nextInt(5)
      //step(delay)
      step(1)
      poke(dut.io.in.valid, 1)
      ////////////////////////////////////
      if (cntValidIn == inp.length - 1) {
        poke(dut.io.lastIn, 1)
      }
      ///////////////////////////////////
      poke(dut.io.in.bits, input1.next())
      cntValidIn += 1
      if (peek(dut.io.out.valid) == 1) {
        //fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(output1.length) / scalingFactor) }

        output1 = output1 :+ peek(dut.io.out.bits) // only can happen if number of pipeline registers is zero
        cntValidOut += 1
      }
      step(1)
    }
    cntValidIn = 0

    //     while (input2.hasNext && peek(dut.io.in.ready)) {
    //       if (cntValidOut == inp.size) {
    //         cntValidOut = 0
    //       }
    //       poke(dut.io.in.valid, 1)
    //       if (cntValidIn == inp.length-1) {
    //         poke(dut.io.lastIn, 1)
    //       }
    //       poke(dut.io.in.bits, input2.next())
    //       cntValidIn += 1
    //       if (peek(dut.io.out.valid) == true) {
    //         if (output1.length < inp.size) {
    //           fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(output1.length)/scalingFactor) }
    //           output1 = output1 :+ peek(dut.io.out.bits)
    //         }
    //         else {
    //           fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(output2.length)/scalingFactor) }
    //           output2 = output2 :+ peek(dut.io.out.bits)
    //         }
    //         cntValidOut +=1
    //       }
    //       step(1)
    //     }

    if (cntValidOut == inp.size) {
      cntValidOut = 0
    }
    poke(dut.io.in.valid, 0)
    poke(dut.io.lastIn, 0)

    wait_for_assert(dut.io.out.valid, cyclesWait)

    while (cntValidOut < inp.size) {
      if (peek(dut.io.out.valid) == 1 && cntValidOut < inp.size) {
        if (cntValidOut == inp.size - 1) {
          expect(dut.io.lastOut, 1)
        }
        compare_data(
          Complex(out(output1.length).real / scalingFactor, out(output1.length).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        //fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(output1.length) / scalingFactor) }
        output1 = output1 :+ peek(dut.io.out.bits)
        cntValidOut += 1
      }
      step(1)
    }
    step(2)

    val input3 = inp.iterator
    var output3 = Seq[Complex]()

    wait_for_assert(dut.io.in.ready, cyclesWait)
    // here idle state is active
    poke(dut.io.in.valid, 1)
    while (output3.length < inp.size) {
      if (input3.hasNext && peek(dut.io.in.ready) == 1) {
        poke(dut.io.in.bits, input3.next())
      }

      if (peek(dut.io.out.valid) == 1) {
        //fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(output3.length) / scalingFactor) }
        compare_data(
          Complex(out(output3.length).real / scalingFactor, out(output3.length).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        output3 = output3 :+ peek(dut.io.out.bits)
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
    step(2 * inp.length)
  }

  def testBitReversal(tol: Double = 3, testSignal: Seq[Complex], params: FFTParams[T]): Unit = {

    // inp in natural, out in natural order
    val inp = testSignal
    val cyclesWait = 5 * params.numPoints
    val fftSize = dut.params.numPoints
    val numStages = log2Up(fftSize)
    val out = fourierTr(DenseVector(inp.toArray)).toScalaVector
    val scalingFactor = pow(2, dut.params.expandLogic.count(_ == 0)).toInt

    //    println("Expected result should be: ")
    //    out.map(c => println((c/scalingFactor).toString))

    val input1 = inp.iterator
    val input2 = inp.iterator
    val input3 = inp.iterator
    val input4 = inp.iterator
    var cntValidOut = 0
    var cntValidIn = 0

    poke(dut.io.in.valid, 0)
    poke(dut.io.out.ready, 0)
    step(2)
    poke(dut.io.out.ready, 1)
    poke(dut.io.in.valid, 1)

    while (input1.hasNext && peek(dut.io.in.ready) == 1) {
      poke(dut.io.in.bits, input1.next())
      step(1)
    }

    while (input2.hasNext && peek(dut.io.in.ready) == 1) {
      poke(dut.io.in.bits, input2.next())
      step(1)
    }

    while (input3.hasNext && peek(dut.io.in.ready) == 1) {
      poke(dut.io.in.bits, input3.next())
      if (peek(dut.io.out.valid) == 1) {
        compare_data(
          Complex(out(cntValidOut).real / scalingFactor, out(cntValidOut).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        /*dut.params.protoIQ.real match {
           case dspR: DspReal =>
             realTolDecPts.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
           case _ => fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
         }*/
        cntValidOut += 1
      }
      step(1)
    }
    if (cntValidOut == fftSize)
      cntValidOut = 0

    poke(dut.io.in.valid, 0)
    // think how to make this test better!
    for (i <- 0 until 10) {
      if (peek(dut.io.out.valid) == 1) {
        /*dut.params.protoIQ.real match {
           case dspR: DspReal =>
             realTolDecPts.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
           case _ => fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
         }*/
        compare_data(
          Complex(out(cntValidOut).real / scalingFactor, out(cntValidOut).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        if (cntValidOut == (fftSize - 1))
          cntValidOut = 0
        else
          cntValidOut += 1
      }
      step(1)
    }
    poke(dut.io.in.valid, 1)

    while (input4.hasNext && peek(dut.io.in.ready) == 1) {
      if (cntValidIn == (inp.length - 1)) {
        poke(dut.io.lastIn, 1)
      }
      poke(dut.io.in.bits, input4.next())
      cntValidIn += 1
      if (peek(dut.io.out.valid) == 1) {
        /*dut.params.protoIQ.real match {
           case dspR: DspReal =>
             realTolDecPts.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
           case _ => fixtolLSB.withValue(tol) { expect(dut.io.out.bits, out(cntValidOut) / scalingFactor) }
         }*/
        compare_data(
          Complex(out(cntValidOut).real / scalingFactor, out(cntValidOut).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        if (cntValidOut == (fftSize - 1))
          cntValidOut = 0
        else
          cntValidOut += 1
      }
      step(1)
    }
    cntValidOut = 0

    poke(dut.io.lastIn, 0)
    step(40)
  }

  def testRandomizedFFT(tol: Double = 3, testSignal: Seq[Complex], params: FFTParams[T]): Unit = {
    val inp = testSignal ++ testSignal ++ testSignal
    val inputTest = testSignal.iterator
    val inpIter = inp.iterator
    val fftSize = dut.params.numPoints
    val numStages = log2Up(fftSize)
    val out =
      if (params.useBitReverse) fourierTr(DenseVector(testSignal.toArray)).toScalaVector
      else bitrevorder_data(fourierTr(DenseVector(testSignal.toArray)).toScalaVector)
    val outCompare = out ++ out ++ out
    val scalingFactor = pow(2, dut.params.expandLogic.count(_ == 0)).toInt

    var cntValidIn = 0
    var cntValidOut = 0
    var inValid = 0
    var outReady = 0

    while (cntValidOut < inp.size) {
      inValid = Random.nextInt(2)
      outReady = Random.nextInt(2)
      poke(dut.io.out.ready, Random.nextInt(2))
      poke(dut.io.in.valid, Random.nextInt(2))

      if (inpIter.hasNext) {
        if (peek(dut.io.in.ready) == 1 && peek(dut.io.in.valid) == 1) {
          poke(dut.io.in.bits, inpIter.next())
          if (cntValidIn == (inp.size - 1)) {
            poke(dut.io.lastIn, true)
          }
          cntValidIn = cntValidIn + 1
        }
      } else {
        poke(dut.io.in.valid, 0)
        poke(dut.io.lastIn, 0)
      }
      if (peek(dut.io.out.ready) == 1 && peek(dut.io.out.valid) == 1) {
        compare_data(
          Complex(outCompare(cntValidOut).real / scalingFactor, outCompare(cntValidOut).imag / scalingFactor),
          peek(dut.io.out.bits),
          tol
        )
        cntValidOut = cntValidOut + 1
      }
      step(1)
    }
    poke(dut.io.in.valid, 0)
    step(50)
  }
}

//
// class FixedPointSDFFFTTester(val c: SDFFFT[FixedPoint]) extends DspTester(c) with SDFFFTTester[FixedPoint] {}
// class DspRealSDFFFTTester(val c: SDFFFT[DspReal]) extends DspTester(c) with SDFFFTTester[DspReal] {}
//
// class FixedSDFFFTTester {
//   def fixedTesterSimple(params: FFTParams[FixedPoint], testSignal: Seq[Complex], tol: Double = 2): Boolean = {
//     dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")) { c =>
//       new FixedPointSDFFFTTester(c) {
//         val (cFFTtest, sFFTtest) = this.simpleTestFFT(tol, testSignal, params)
//       }
//     }
//   }
//   def dspRealTesterSimple(params: FFTParams[DspReal], testSignal: Seq[Complex], tolReal: Int = 12): Boolean = {
//     dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")) { c =>
//       new DspRealSDFFFTTester(c) {
//         val (cFFTtest, sFFTtest) = this.simpleTestFFT(tolReal, testSignal, params)
//       }
//     }
//   }
//   def fixedTester(
//     params:     FFTParams[FixedPoint],
//     testSignal: Seq[Complex],
//     tol:    Int = 2,
//     win:        Boolean = false
//   ): Boolean = {
//     dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")) { c =>
//       new FixedPointSDFFFTTester(c) {
//         if (params.useBitReverse) {
//           this.testBitReversal(tol, testSignal, params)
//         } else {
//           this.testFFT(tol, testSignal, params)
//         }
//       }
//     }
//   }
//   def dspRealTester(params: FFTParams[DspReal], testSignal: Seq[Complex], tolReal: Int = 12): Boolean = {
//     dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")) { c =>
//       new DspRealSDFFFTTester(c) {
//         this.testFFT(tolReal, testSignal, params)
//       }
//     }
//   }
// }
