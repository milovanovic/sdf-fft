// SPDX-License-Identifier: Apache-2.0

package fft

import org.scalatest.{FlatSpec, Matchers}
import breeze.math.Complex
import chisel3.util.log2Up

import dsptools._
import dsptools.numbers._

/*
* Test various fft sizes, both decimType and some implementation specific parameters such as pipeline registers and complex multiplier structure
*/
class Radix22Spec extends FlatSpec with Matchers {
  
  val testerSDFFFT = new FixedSDFFFTTester
  
  def getRealTone(numSamples: Int, f1r: Double, f2r: Double = 0): Seq[Complex] = {
    require(f1r != 0, "Digital frequency should not be zero!")
    (0 until numSamples).map(i => Complex((math.sin(2 * math.Pi * f1r * i) + math.sin(2 * math.Pi * f2r * i)), 0.0))
    //Random.nextGaussian
  }
  def genRandSignal(numSamples: Int, scalingFactor: Int): Seq[Complex] = {
    import scala.math.sqrt
    import scala.util.Random
    
    (0 until numSamples).map(x => Complex(Random.nextDouble(), Random.nextDouble()))
    //(0 until numSamples).map(i => Complex((-sqrt(2) + (2*sqrt(2))*Random())/(numSamples/scalingFactor), (-sqrt(2) + (2*sqrt(2))*randomDouble())/(numSamples/scalingFactor))).toVector
  }
  for (i <- Seq(8, 16, 64)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      for (numPipes <- Seq(Seq(1, 1))) {
        val addPipes = numPipes(0)
        val mulPipes = numPipes(1)
        it should f"compute radix 2^2 $decType FFT, size $i with no growing logic and test initial storing logic" in {
          val testSignal = getRealTone(i, 0.36)
          val paramsFixed = FFTParams.fixed(
            numPoints = i,
            numMulPipes = mulPipes,
            numAddPipes = addPipes,
            decimType = decType,
            trimType = Convergent,
            expandLogic = Array.fill(log2Up(i))(0),
            keepMSBorLSB = Array.fill(log2Up(i))(true),
            sdfRadix = "2^2")
          dsptools.Driver.execute(
          () => new SDFFFT(paramsFixed), Array("-tbn", "verilator")){ c =>
          new FixedPointSDFFFTTester(c) {
              updatableDspVerbose.withValue(false) {
                this.testInitialStoring(3, testSignal, paramsFixed)
              }
            }
          } should be (true)
        }
      }
    }
  }
  
  for (i <- Seq(4, 8, 16, 32, 64, 128, 256, 512)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      for(numPipes <- Seq(Seq(0,0), Seq(1,0), Seq(1,1), Seq(3,2))) {
        val addPipes = numPipes(0)
        val mulPipes = numPipes(1)
        it should f"compute radix 2^2 $decType FFT, size $i with no growing logic and numAddPipes = $addPipes and numMulPipes = $mulPipes" in {
          val testSignal = getRealTone(i, (1.0/i).toDouble)
          val paramsFixed = FFTParams.fixed(
            numPoints = i,
            numMulPipes = mulPipes,
            numAddPipes = addPipes,
            decimType = decType,
            trimType = Convergent,
            expandLogic = Array.fill(log2Up(i))(0),
            keepMSBorLSB = Array.fill(log2Up(i))(true),
            sdfRadix = "2^2")
          dsptools.Driver.execute(
          () => new SDFFFT(paramsFixed), Array("-tbn", "verilator")){ c =>
          new FixedPointSDFFFTTester(c) {
              updatableDspVerbose.withValue(false) {
                this.testFFT(3, testSignal, paramsFixed)
              }
            }
          } should be (true)
        }
      }
    }
  }
  
  // use 4 real multipliers for complex multiplication
  for (i <- Seq(8, 32)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix 2^2 $decType FFT, size $i with no growing logic and 4 multiplier structure" in {
        val testSignal = getRealTone(i, (1.0/i).toDouble)
        val paramsFixed = FFTParams.fixed(
          numPoints = i,
          numAddPipes = 1,
          numMulPipes = 1,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          sdfRadix = "2^2",
          use4Muls = true)
        testerSDFFFT.fixedTester(paramsFixed, testSignal) should be (true)
      }
    }
  }
  
  for (i <- Seq(8, 32)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix 2^2 $decType FFT, size $i with no growing logic and included bit-reversal stage" in {
        val testSignal = getRealTone(i, (1.0/i).toDouble)
        val paramsFixed = FFTParams.fixed(
          numPoints = i,
          numAddPipes = 1,
          numMulPipes = 1,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          sdfRadix = "2^2",
          useBitReverse = true)
        testerSDFFFT.fixedTester(paramsFixed, testSignal) should be (true)
      }
    }
  }
  
  // test window functions, use log10 for plotting
  for (window <- Seq(WindowFunctionTypes.Hamming(), WindowFunctionTypes.Hanning(), WindowFunctionTypes.Blackman(), WindowFunctionTypes.Triangular(),  WindowFunctionTypes.None())) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix 2^2, size 256, $decType FFT and applied " ++ window.toString in {
        val testSignal = getRealTone(256, (15.5/256).toDouble)
          val paramsFixed = FFTParams.fixed(
            numPoints = 256,
            numAddPipes = 1,
            numMulPipes = 1,
            decimType = decType,
            expandLogic = Array.fill(log2Up(256))(0),
            keepMSBorLSB = Array.fill(log2Up(256))(true),
            sdfRadix = "2^2",
            useBitReverse = false,
            windowFunc = window)
          testerSDFFFT.fixedTester(paramsFixed, testSignal, win = true) should be (true)
      }
    }
  }
}

