// SPDX-License-Identifier: Apache-2.0

package fft

import org.scalatest.{FlatSpec, Matchers}
import breeze.plot._
import breeze.linalg._
import chisel3.util.log2Up

import dsptools._
import dsptools.numbers._ 

/**
 * This test shows how changing fft parameters affect signal to noise quantization ratio (SQNR) in the case of the Radix 2 fft.
 * Analysis is applied on a large random data set. For presentation dif algorithm has been chosen. Other decimation type (dit) gives similar results.
 */

class Radix2SQNRSpec extends FlatSpec with Matchers {
  
  val numSamples = 1
  val fftSizeSeq = Seq(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
  var sqnr_results16_scale : Array[Double] = Array.fill(fftSizeSeq.length)(0)
  var sqnr_results24_scale : Array[Double] = Array.fill(fftSizeSeq.length)(0)
  
  var sqnr_results16_grow : Array[Double] = Array.fill(fftSizeSeq.length)(0)
  var sqnr_results24_grow : Array[Double] = Array.fill(fftSizeSeq.length)(0)
  
  var sum16_scale : Double = 0
  var sum24_scale : Double = 0
  var sum16_grow : Double = 0
  var sum24_grow : Double = 0
  
  val t1 = System.nanoTime
 
 /**
  * Test data width effect on SQNR.
  */
  
  for (i <- fftSizeSeq) {
    sum16_scale = 0
    sum24_scale = 0
    sum16_grow = 0
    sum24_grow = 0
    
    val paramsFixed16s = FFTParams.fixed(
      numPoints = i,
      dataWidth = 16,
      twiddleWidth =16,
      trimType = Convergent,
      binPoint =14,
      expandLogic = Array.fill(log2Up(i))(0),
      keepMSBorLSB = Array.fill(log2Up(i))(true),
      sdfRadix = "2")
      
    dsptools.Driver.execute(
    () => new SDFFFT(paramsFixed16s), Array("-tbn", "treadle")){ c =>
    new FixedPointSDFFFTTester(c) {
      var count = 0
      while (count < numSamples) {
        val testSignal = genRandSignal(i,i)
        updatableDspVerbose.withValue(false) {
          val (chiselFFT16, scalaFFT16) = this.simpleTestFFT(3, testSignal, paramsFixed16s)
          val sqnr16 = calc_sqnr(chiselFFT16, scalaFFT16)
          sum16_scale = sum16_scale + sqnr16
          count = count + 1
        }
      }
      sqnr_results16_scale(log2Up(i)-1) = (sum16_scale/numSamples)
      }
    }
    val paramsFixed24s = FFTParams.fixed(
      numPoints = i,
      dataWidth = 24,
      twiddleWidth = 24,
      binPoint =22, 
      trimType = Convergent,
      expandLogic = Array.fill(log2Up(i))(0),
      keepMSBorLSB = Array.fill(log2Up(i))(true),
      sdfRadix = "2")
    dsptools.Driver.execute(
    () => new SDFFFT(paramsFixed24s), Array("-tbn", "treadle")){ c =>
    new FixedPointSDFFFTTester(c) {
      var count = 0
      while (count < numSamples) {
        val testSignal = genRandSignal(i,i)
        updatableDspVerbose.withValue(false) {
          val (chiselFFT24, scalaFFT24) = this.simpleTestFFT(3, testSignal, paramsFixed24s)
           val sqnr24 = calc_sqnr(chiselFFT24, scalaFFT24)
           sum24_scale = sum24_scale + sqnr24
        }
        count = count + 1
      }
      sqnr_results24_scale(log2Up(i)-1) = (sum24_scale/numSamples)
      }
    }
    val paramsFixed16g = FFTParams.fixed(
      numPoints = i,
      dataWidth = 16,
      twiddleWidth = 16,
      trimType = Convergent,
      binPoint =14, 
      expandLogic = Array.fill(log2Up(i))(1),
      keepMSBorLSB = Array.fill(log2Up(i))(true),
      sdfRadix = "2")
    dsptools.Driver.execute(
    () => new SDFFFT(paramsFixed16g), Array("-tbn", "treadle")){ c => 
    new FixedPointSDFFFTTester(c) {
      var count = 0
      while (count < numSamples) {
        val testSignal = genRandSignal(i,i)
        updatableDspVerbose.withValue(false) {
          val (chiselFFT16, scalaFFT16) = this.simpleTestFFT(7, testSignal, paramsFixed16g)
          val sqnr16 = calc_sqnr(chiselFFT16, scalaFFT16)
          sum16_grow = sum16_grow + sqnr16
          count = count + 1
        }
      }
      sqnr_results16_grow(log2Up(i)-1) = (sum16_grow/numSamples)
      }
    }
    
    val paramsFixed24g = FFTParams.fixed(
      numPoints = i,
      numAddPipes = 1,
      dataWidth = 24,
      twiddleWidth = 24,
      trimType = Convergent,
      binPoint =22, 
      expandLogic = Array.fill(log2Up(i))(1),
      keepMSBorLSB = Array.fill(log2Up(i))(true),
      sdfRadix = "2")
      
    dsptools.Driver.execute(
    () => new SDFFFT(paramsFixed24g), Array("-tbn", "treadle")){ c =>
    new FixedPointSDFFFTTester(c) {
      var count = 0
      while (count < numSamples) {
        val testSignal = genRandSignal(i,i)
        updatableDspVerbose.withValue(false) {
          val (chiselFFT24, scalaFFT24) = this.simpleTestFFT(7, testSignal, paramsFixed24g)
          val sqnr24 = calc_sqnr(chiselFFT24, scalaFFT24)
          sum24_grow = sum24_grow + sqnr24
          count = count + 1
        }
      }
      sqnr_results24_grow(log2Up(i)-1) = (sum24_grow/numSamples)
      }
    }
  }
  
  val f1 = Figure()
  val p1 = f1.subplot(0)
  p1.legend_= (true)
  val xaxis = fftSizeSeq.toArray.map(e => log2Up(e).toDouble)
  p1.setXAxisIntegerTickUnits() // this gives 1 2 3 4 5 . . .
  
  p1 += plot(xaxis, sqnr_results16_scale, name = "datawidth 16b Convergent")
  p1 += plot(xaxis, sqnr_results24_scale, name = "datawidth 24b Convergent")
  p1 += plot(xaxis, sqnr_results16_grow, name = "datawidth 16b grow")
  p1 += plot(xaxis, sqnr_results24_grow, name = "datawidth 24b grow")
  
  p1.ylim(Seq(sqnr_results16_scale.min,sqnr_results24_scale.min).min - 10.0, Seq(sqnr_results16_scale.max, sqnr_results24_scale.max).max + 10.0)
  p1.title_= (s"Signal-to-Quantization-Noise Ratio (SQNR)" + "\n" + " radix 2 - DIF")

  p1.xlabel = "Number of FFT stages (base 2 logarithm of the FFT window size)"
  //p1.ylabel = "SQNR $\pi$ \pi"
  p1.ylabel = "SQNR [dB]"
  f1.saveas(s"test_run_dir/sqnrDataWidthRadix2.pdf")
  
  val durationTest1 = (System.nanoTime - t1) / 1e9d
  println(s"The execution time of the data width sqnr analysis is $durationTest1 s")
  
  
  /**
   * Tests rounding mode effect on SQNR parameter.
   */
  
  val t2 = System.nanoTime
  var sum16_trim : Double = 0
  val roundingModes = Seq(RoundDown, RoundUp, RoundTowardsZero, RoundTowardsInfinity, RoundHalfDown, RoundHalfUp, RoundHalfTowardsZero, RoundHalfTowardsInfinity, RoundHalfToEven, RoundHalfToOdd)
  
  var sqnr_results_trim = Array.ofDim[Double](roundingModes.length, fftSizeSeq.length)
  val numSamplesTrim = 5
  var count = numSamplesTrim
  
  for ((trimType,index) <- roundingModes.zipWithIndex) {
    for (i <- fftSizeSeq) {
      sum16_trim = 0
      count = 0
      val paramsFixed16 = FFTParams.fixed(
        numPoints = i, 
        numAddPipes = 1, 
        dataWidth = 16,
        expandLogic = Array.fill(log2Up(i))(0),
        keepMSBorLSB = Array.fill(log2Up(i))(true),
        sdfRadix = "2",
        trimType = trimType)
      
      dsptools.Driver.execute(
      () => new SDFFFT(paramsFixed16), Array("-tbn", "treadle")){ c =>
      new FixedPointSDFFFTTester(c) {
        var count = 0
        while (count < numSamplesTrim) {
          val testSignal = genRandSignal(i,i)
          updatableDspVerbose.withValue(false) {
            val (chiselFFT16, scalaFFT16) = this.simpleTestFFT(6, testSignal, paramsFixed16)
            val sqnr16 = calc_sqnr(chiselFFT16, scalaFFT16)
            sum16_trim = sum16_trim + sqnr16
            count = count + 1
          }
        }
        sqnr_results_trim(index)(log2Up(i)-1) = (sum16_trim/numSamplesTrim)
        }
      }
    }
  }
  
  val f2 = Figure()
  val p2 = f2.subplot(0)
  p2.legend_= (true)
  val fftSize = fftSizeSeq.toArray.map(e => log2Up(e).toDouble)
  p2.setXAxisIntegerTickUnits()
  sqnr_results_trim.zip(roundingModes).foreach { case (sqnr, round) => p2 += plot(fftSize, sqnr, name = round.toString) }
  
  p2.ylim(sqnr_results_trim(0).min - 10.0, sqnr_results_trim(0).max + 10.0)
  p2.xlabel = "Number of FFT stages (base 2 logarithm of the FFT window size)"
  p2.ylabel = "SQNR [dB]"
  f2.saveas(s"test_run_dir/sqnrRoundingModesRadix2.pdf")
  
  val durationTest2 = (System.nanoTime - t2) / 1e9d
  println(s"The execution time of the rounding mode sqnr analysis is $durationTest2 s")
}
