// SPDX-License-Identifier: Apache-2.0

package fft

import org.scalatest.{FlatSpec, Matchers}
import chisel3.util.log2Up
import dsptools._
import dsptools.numbers._ 

/*
* Test fft with random input data
*/

class Radix2RandomSpec extends FlatSpec with Matchers {

  val numSamples = 2
  val fftSizeSeq = Seq(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
  
  for (i <- fftSizeSeq) {
    for (backendName <- Seq("verilator", "treadle")) {
      val paramsFixed16s = FFTParams.fixed(
        numPoints = i,
        dataWidth = 16,
        twiddleWidth =16,
        trimType = Convergent,
        binPoint =14, 
        expandLogic = Array.fill(log2Up(i))(0),
        keepMSBorLSB = Array.fill(log2Up(i))(true),
        sdfRadix = "2")

      it should f"compute fft for random input data $i fft size, and dataWidth = 16 and $backendName simulator" in {
        dsptools.Driver.execute(
        () => new SDFFFT(paramsFixed16s), Array("-tbn", backendName)){ c =>
        new FixedPointSDFFFTTester(c) {
          var count = 0
          while (count < numSamples) {
            val testSignal = genRandSignal(i,i)
            updatableDspVerbose.withValue(false) {
              val (chiselFFT16, scalaFFT16) = this.simpleTestFFT(3, testSignal, paramsFixed16s)
            }
            count = count + 1
          }
        }
        } should be (true)
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
        
      it should f"compute fft for random input data $i fft size, and dataWidth = 24 and $backendName simulator" in {
        dsptools.Driver.execute(
          () => new SDFFFT(paramsFixed24s), Array("-tbn", backendName)){ c =>
          new FixedPointSDFFFTTester(c) {
            var count = 0
            while (count < numSamples) {
              val testSignal = genRandSignal(i,i)
              updatableDspVerbose.withValue(false) {
                val (chiselFFT24, scalaFFT24) = this.simpleTestFFT(3, testSignal, paramsFixed24s)
                count = count + 1
              }
            }
          }
        } should be (true)
      }
      
      val paramsFixed16g = FFTParams.fixed(
        numPoints = i,
        dataWidth = 16,
        twiddleWidth = 16,
        trimType = Convergent,
        binPoint =14, 
        expandLogic = Array.fill(log2Up(i))(1),
        keepMSBorLSB = Array.fill(log2Up(i))(true),
        sdfRadix = "2^2")
    
      it should f"compute fft for random input data $i fft size, and dataWidth = 16 with growing logic and $backendName simulator" in {
        dsptools.Driver.execute(
        () => new SDFFFT(paramsFixed16g), Array("-tbn", backendName)){ c => 
        new FixedPointSDFFFTTester(c) {
          var count = 0
            while (count < numSamples) {
              val testSignal = genRandSignal(i,i)
              updatableDspVerbose.withValue(false) {
                val (chiselFFT16, scalaFFT16) = this.simpleTestFFT(7, testSignal, paramsFixed16g)
                count = count + 1
              }
            }
          }
        } should be (true)
      }
      
      val paramsFixed24g = FFTParams.fixed(
        numPoints = i,
        numAddPipes = 0,
        dataWidth = 24,
        twiddleWidth = 24,
        trimType = Convergent,
        binPoint =22, 
        expandLogic = Array.fill(log2Up(i))(1),
        keepMSBorLSB = Array.fill(log2Up(i))(true),
        sdfRadix = "2")
    
      it should f"compute fft for random input data $i fft size, and dataWidth = 24 with growing logic and $backendName simulator" in {
        dsptools.Driver.execute(
        () => new SDFFFT(paramsFixed24g), Array("-tbn", backendName)){ c =>
          new FixedPointSDFFFTTester(c) {
            var count = 0
            while (count < numSamples) {
              val testSignal = genRandSignal(i,i)
              updatableDspVerbose.withValue(false) {
                val (chiselFFT24, scalaFFT24) = this.simpleTestFFT(7, testSignal, paramsFixed24g)
              }
              count = count + 1
            }
          }
        } should be (true)
      }
    }
  }
}
