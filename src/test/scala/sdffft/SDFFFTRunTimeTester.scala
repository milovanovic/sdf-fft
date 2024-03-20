package dspblocks.sdffft

import chisel3._
import chisel3.util.log2Up

import breeze.math.Complex
import breeze.signal.fourierTr
import breeze.linalg.DenseVector

import scala.math.pow
import scala.util.Random

/*
 * This tester tests run time configurable fft size
 * Common tester is used for radix 2, radix 2^2, and radix 2^2 module which provides full run time configurability.
 */

class SDFFFTRunTimeTester[T <: chisel3.Data](dut: SDFFFT[T])
    extends SDFFFTTesterBase(dut)
    with HasTesterUtil[SDFFFT[T]]
    with HasSignalUtils {

  def testSDFFFTRunTime(tol: Double = 0.01, params: FFTParams[T]) {
    val numStages = log2Up(params.numPoints)
    var fftSizeColl: Seq[Int] = Seq.empty
    if (params.sdfRadix == "2" || params.runTimeR22.getOrElse(false)) {
      fftSizeColl = (1 to numStages).map(stage => pow(2, stage).toInt)
    } else {
      fftSizeColl =
        if (numStages % 2 == 0) (2 to numStages by 2).map(stage => pow(2, stage).toInt)
        else (1 to numStages by 2).map(stage => pow(2, stage).toInt)
    }
    var cyclesWait = 0
    var idx = 0
    poke(dut.io.out.ready, 1)
    while (idx < 1) { // for shuffling more than once, change this condition
      val fftSizesShuffle = Random.shuffle(fftSizeColl)
      for (size <- fftSizesShuffle) {
        cyclesWait = 0
        var cnt = 0
        var scalingFactor = size
        val testTone = getTone(size, 0.04, 0.1)
        val inp = if (params.decimType == DITDecimType) bitrevorder_data(testTone) else testTone
        val out =
          if (params.decimType == DITDecimType) fourierTr(DenseVector(testTone.toArray)).toScalaVector
          else bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector)
        if (params.expandLogic.sum != 0)
          if (params.decimType == DIFDecimType)
            scalingFactor = pow(2, params.expandLogic.drop(numStages - log2Up(size)).count(_ != 1)).toInt
          else
            scalingFactor = pow(2, params.expandLogic.take(log2Up(size)).count(_ != 1)).toInt
        else
          scalingFactor = size

        poke(dut.io.in.valid, 0)
        poke(dut.io.fftSize.get, log2Up(size).toInt)
        step(5)
        poke(dut.io.in.valid, 1)
        val input = inp.iterator
        var output = Seq[Complex]()

        while (output.length < size) {
          if (input.hasNext && peek(dut.io.in.ready) == BigInt(1)) {
            poke(dut.io.in.bits, input.next())
          }
          if (peek(dut.io.out.valid) == BigInt(1)) {
            /*params.protoIQ.real match {
              case dspR: DspReal =>
                realTolDecPts.withValue(tol) { expect(dut.io.out.bits, out(output.length) / scalingFactor) }
              case _ => fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, out(output.length) / scalingFactor) }
            }*/
            params.protoIQ.real match {
              case uInt: UInt => expect(dut.io.out.bits, out(output.length) / scalingFactor)
              case sInt: SInt =>
                expect(dut.io.out.bits, out(output.length) / scalingFactor)
              case _ =>
                compare_data(
                  Complex(out(output.length).real / scalingFactor, out(output.length).imag / scalingFactor),
                  peek(dut.io.out.bits),
                  tol
                )
            }
            output = output :+ peek(dut.io.out.bits)
          }
          step(1)
        }
        val bitrevorderOutput = if (params.decimType == DIFDecimType) bitrevorder_data(output) else output
        // uncomment this part for ploting diagrams
//         if (size > 256) {
//           val scaledfft =  fourierTr(DenseVector(testTone.toArray)).toScalaVector.map(c => c/scalingFactor)
//           plot_fft(bitrevorderOutput, scaledfft)
//         }
        poke(dut.io.in.valid, 0)
        step(2 * inp.length)
        reset(2)
      }
      idx += 1
    }
  }
}

//val c!
/*class FixedPointSDFFFTRunTimeTester(val c: SDFFFT[FixedPoint])
    extends DspTester(c)
    with SDFFFTRunTimeTester[FixedPoint] {}
class DspRealSDFFFTRunTimeTester(val c: SDFFFT[DspReal]) extends DspTester(c) with SDFFFTRunTimeTester[DspReal] {}

class FixedSDFFFTRunTimeTester {
  def fixedTester(params: FFTParams[FixedPoint], tolLSBs: Int = 8): Boolean = {
    dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")) { c =>
      new FixedPointSDFFFTRunTimeTester(c) { this.testSDFFFTRunTime(tolLSBs, params) }
    }
  }
  def dspRealTester(params: FFTParams[DspReal], tolReal: Int = 12): Boolean = {
    dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")) { c =>
      new DspRealSDFFFTRunTimeTester(c) { this.testSDFFFTRunTime(tolReal, params) }
    }
  }
}*/
