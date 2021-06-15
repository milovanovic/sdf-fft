// SPDX-License-Identifier: Apache-2.0

package fft

import org.scalatest.{FlatSpec, Matchers}

import breeze.plot._
import breeze.linalg._
import chisel3.util.log2Up

import dsptools._
import dsptools.numbers._


/**
 * This test shows how changing fft parameters affect signal to noise quantization ratio (SQNR) in the case of the Radix 2^2.
 * Analysis is applied on a large random data set. For presentation dif algorithm has been chosen. Other decimation type (dit) gives similar results.
 */

class Radix22SQNRExtendedSpec extends FlatSpec with Matchers {

  val numSamples = 1
  val fftSizeSeq = Seq(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024) //Seq(128)

  // define input and output data widths
  val dataWidthIn = 12
  val binPointIn = 10
  val dataWidthOut = 16
  val binPointOut = 10
  val N_stages = 4

  // grow N stages and then
  //-> so trimEnable is false and expand logic is enabled for first N stages
  var sqnr_results_N_stages_grow : Array[Double] = Array.fill(fftSizeSeq.length)(0)
  var sqnr_results_N_stages_grow_with_trim : Array[Double] = Array.fill(fftSizeSeq.length)(0)
  var sqnr_results_scale: Array[Double] = Array.fill(fftSizeSeq.length)(0)
  var sqnr_results_grow: Array[Double] = Array.fill(fftSizeSeq.length)(0)


  var sum_grow: Double = 0
  var sum_N_stages_grow : Double = 0
  var sum_N_stages_grow_with_trim : Double = 0
  var sum_scale: Double = 0

  val t1 = System.nanoTime

 /**
  * Test data width effect on SQNR.
  */

  for (i <- fftSizeSeq) {
    sum_N_stages_grow = 0
    sum_N_stages_grow_with_trim = 0
    sum_grow = 0
    sum_scale = 0

// grow certain number of stages
    val paramsFixed1 = FFTParams.fixed(
      numPoints = i,
      dataWidth = dataWidthIn,
      binPoint = binPointIn,
      twiddleWidth = 16,
      trimType = Convergent,
      expandLogic = Array.fill(log2Up(i))(1).zipWithIndex.map { case (e,ind) => if (ind < (N_stages)) 1 else 0 }, // can be simplified for sure
      keepMSBorLSB = Array.fill(log2Up(i))(true),
      sdfRadix = "2^2")

    dsptools.Driver.execute(
    () => new SDFFFT(paramsFixed1), Array("-tbn", "treadle")){ c =>
    new FixedPointSDFFFTTester(c) {
      var count = 0
      while (count < numSamples) {
        val testSignal = genRandSignal(i,i)
        updatableDspVerbose.withValue(false) {
          val (chiselFFT, scalaFFT) = this.simpleTestFFT(3, testSignal, paramsFixed1)
          val sqnr1 = calc_sqnr(chiselFFT, scalaFFT)
          sum_N_stages_grow = sum_N_stages_grow + sqnr1
          count = count + 1
        }
      }
      sqnr_results_N_stages_grow(log2Up(i)-1) = (sum_N_stages_grow/numSamples)
      }
    }

  // grow all stages but trim at the end
    val paramsFixed2 = FFTParams.fixed(
      numPoints = i,
      dataWidth = dataWidthIn,
      binPoint = binPointIn,
      dataWidthOut = dataWidthOut,
      binPointOut = binPointOut,
      trimEnable = true,
      twiddleWidth = 16,
      trimType = Convergent,
      expandLogic = Array.fill(log2Up(i))(1), // can be simplified for sure
      keepMSBorLSB = Array.fill(log2Up(i))(true),
      sdfRadix = "2^2")

    dsptools.Driver.execute(
    () => new SDFFFT(paramsFixed2), Array("-tbn", "treadle")){ c =>
    new FixedPointSDFFFTTester(c) {
      var count = 0
      while (count < numSamples) {
        val testSignal = genRandSignal(i,i)
        updatableDspVerbose.withValue(false) {
          val (chiselFFT, scalaFFT) = this.simpleTestFFT(3, testSignal, paramsFixed2)
          val sqnr2 = calc_sqnr(chiselFFT, scalaFFT)
          sum_N_stages_grow_with_trim = sum_N_stages_grow_with_trim + sqnr2
          count = count + 1
        }
      }
      sqnr_results_N_stages_grow_with_trim(log2Up(i)-1) = (sum_N_stages_grow_with_trim/numSamples)
      }
    }

    // scale at each stage - optimal for resource utilization
    val paramsFixed3 = FFTParams.fixed(
      numPoints = i,
      dataWidth = dataWidthIn,
      binPoint = binPointIn,
      dataWidthOut = dataWidthOut,
      binPointOut = binPointOut,
      trimEnable = false,
      twiddleWidth = 16,
      trimType = Convergent,
      expandLogic = Array.fill(log2Up(i))(0), // can be simplified for sure
      keepMSBorLSB = Array.fill(log2Up(i))(true),
      sdfRadix = "2^2")

    dsptools.Driver.execute(
    () => new SDFFFT(paramsFixed3), Array("-tbn", "treadle")){ c =>
    new FixedPointSDFFFTTester(c) {
      var count = 0
      while (count < numSamples) {
        val testSignal = genRandSignal(i,i)
        updatableDspVerbose.withValue(false) {
          val (chiselFFT, scalaFFT) = this.simpleTestFFT(3, testSignal, paramsFixed3)
          val sqnr3 = calc_sqnr(chiselFFT, scalaFFT)
          sum_scale = sum_scale + sqnr3
          count = count + 1
        }
      }
      sqnr_results_scale(log2Up(i)-1) = (sum_scale/numSamples)
      }
    }
    // grow at the each stage
    val paramsFixed4 = FFTParams.fixed(
      numPoints = i,
      dataWidth = dataWidthIn,
      binPoint = binPointIn,
      twiddleWidth = 16,
      trimType = Convergent,
      expandLogic = Array.fill(log2Up(i))(1), // can be simplified for sure
      keepMSBorLSB = Array.fill(log2Up(i))(true),
      sdfRadix = "2^2")

    dsptools.Driver.execute(
    () => new SDFFFT(paramsFixed4), Array("-tbn", "treadle")){ c =>
    new FixedPointSDFFFTTester(c) {
      var count = 0
      while (count < numSamples) {
        val testSignal = genRandSignal(i,i)
        updatableDspVerbose.withValue(false) {
          val (chiselFFT, scalaFFT) = this.simpleTestFFT(7, testSignal, paramsFixed4)
          val sqnr4 = calc_sqnr(chiselFFT, scalaFFT)
          sum_grow = sum_grow + sqnr4
          count = count + 1
        }
      }
      sqnr_results_grow(log2Up(i)-1) = (sum_grow/numSamples)
      }
    }
}
  val f1 = Figure()
  val p1 = f1.subplot(0)
  p1.legend_= (true)
  val xaxis = fftSizeSeq.toArray.map(e => log2Up(e).toDouble)
  p1.setXAxisIntegerTickUnits() // this gives 1 2 3 4 5 . . .

  // TODO: Rename those labels
  p1 += plot(xaxis, sqnr_results_N_stages_grow, name = s"Bit growth for first $N_stages stages")
  p1 += plot(xaxis, sqnr_results_N_stages_grow_with_trim, name = "Bit growth for all stages, scale on last")
  p1 += plot(xaxis, sqnr_results_grow, name = "Bit growth for all stages")
  p1 += plot(xaxis, sqnr_results_scale, name = "Scale for all stages")

  p1.ylim(Seq(sqnr_results_N_stages_grow.min,sqnr_results_N_stages_grow_with_trim.min, sqnr_results_grow.min, sqnr_results_scale.min).min - 10.0, Seq(sqnr_results_N_stages_grow.max,sqnr_results_N_stages_grow_with_trim.max, sqnr_results_grow.max, sqnr_results_scale.max).max + 10.0)

  p1.title_= (s"Signal-to-Quantization-Noise Ratio (SQNR)" + "\n" + " radix 2^2 - DIF")

  p1.xlabel = "Number of FFT stages (base 2 logarithm of the FFT window size)"
  p1.ylabel = "SQNR [dB]"
  f1.saveas(s"test_run_dir/sqnr_grow_stages_analysis.pdf")

  val durationTest1 = (System.nanoTime - t1) / 1e9d
  println(s"The execution time of the grow logic analysis sqnr analysis is $durationTest1 s")

}
