package dspblocks.sdffft

import org.scalatest.flatspec.AnyFlatSpec
import breeze.math.Complex
import chisel3.util.log2Up
import dsptools.numbers._
import chiseltest._
import logger.{LogLevel, LogLevelAnnotation}

/*
 * Test various fft sizes, both decimType and some implementation specific parameters such as pipeline registers and complex multiplier structure
 */
class Radix22Spec extends AnyFlatSpec with ChiselScalatestTester {

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
            sdfRadix = "2^2"
          )
          test(new SDFFFT(paramsFixed))
            .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
            .runPeekPoke(new SDFFFTTester(_) {
              testInitialStoring(0.0001, testSignal, paramsFixed)
            })
        }
      }
    }
  }

  for (i <- Seq(16, 256, 1024)) {
    for (decType <- Seq(DIFDecimType)) {
      for (numPipes <- Seq(Seq(1, 1))) {
        val addPipes = numPipes(0)
        val mulPipes = numPipes(1)
        it should f"compute radix 2^2 $decType FFT, size $i with no growing logic and numAddPipes = $addPipes and numMulPipes = $mulPipes" in {
          val testSignal = getRealTone(i, (1.0 / i).toDouble)
          val paramsFixed = FFTParams.fixed(
            numPoints = i,
            numMulPipes = mulPipes,
            numAddPipes = addPipes,
            decimType = decType,
            trimType = Convergent,
            expandLogic = Array.fill(log2Up(i))(0),
            keepMSBorLSB = Array.fill(log2Up(i))(true),
            sdfRadix = "2^2"
          )
          /*dsptools.Driver.execute(() => new SDFFFT(paramsFixed), Array("-tbn", "verilator")) { c =>
            new FixedPointSDFFFTTester(c) {
              updatableDspVerbose.withValue(false) {
                this.testFFT(3, testSignal, paramsFixed)
              }
            }*/
          test(new SDFFFT(paramsFixed))
            .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
            .runPeekPoke(new SDFFFTTester(_) {
              testRandomizedFFT(0.001, testSignal, paramsFixed)
            })
        }
      }
    }
  }

  for (i <- Seq(4, 8, 32, 64, 128, 512)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      for (numPipes <- Seq(Seq(0, 0), Seq(1, 0), Seq(1, 1), Seq(3, 2))) {
        val addPipes = numPipes(0)
        val mulPipes = numPipes(1)
        it should f"compute radix 2^2 $decType FFT, size $i with no growing logic and numAddPipes = $addPipes and numMulPipes = $mulPipes" in {
          val testSignal = getRealTone(i, (1.0 / i).toDouble)
          val paramsFixed = FFTParams.fixed(
            numPoints = i,
            numMulPipes = mulPipes,
            numAddPipes = addPipes,
            decimType = decType,
            trimType = Convergent,
            expandLogic = Array.fill(log2Up(i))(0),
            keepMSBorLSB = Array.fill(log2Up(i))(true),
            sdfRadix = "2^2"
          )
          test(new SDFFFT(paramsFixed))
            .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
            .runPeekPoke(new SDFFFTTester(_) {
              testFFT(0.0001, testSignal, paramsFixed)
            })
        }
      }
    }
  }

  // use 4 real multipliers for complex multiplication
  for (i <- Seq(8, 32)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix 2^2 $decType FFT, size $i with no growing logic and 4 multiplier structure" in {
        val testSignal = getRealTone(i, (1.0 / i).toDouble)
        val paramsFixed = FFTParams.fixed(
          numPoints = i,
          numAddPipes = 1,
          numMulPipes = 1,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          sdfRadix = "2^2",
          use4Muls = true
        )

        test(new SDFFFT(paramsFixed))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
          .runPeekPoke(new SDFFFTTester(_) {
            testFFT(0.0001, testSignal, paramsFixed)
          })
        // testerSDFFFT.fixedTester(paramsFixed, testSignal) should be(true)
      }
    }
  }

  // test design with single-port/dual-port SRAM used for delay buffers and ping-pong buffers
  for (i <- Seq(32, 128)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      for (singlePortSRAM <- Seq(true, false)) {
        it should f"compute radix 2^2 $decType FFT, size $i, singlePortSRAM = $singlePortSRAM with no growing logic and included bit-reversal stage" in {
          val testSignal = getRealTone(i, (1.0 / i).toDouble)
          val paramsFixed = FFTParams.fixed(
            numPoints = i,
            numAddPipes = 1,
            numMulPipes = 1,
            decimType = decType,
            expandLogic = Array.fill(log2Up(i))(0),
            keepMSBorLSB = Array.fill(log2Up(i))(true),
            sdfRadix = "2^2",
            useBitReverse = true,
            singlePortSRAM = singlePortSRAM
          )
          test(new SDFFFT(paramsFixed))
            .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
            .runPeekPoke(new SDFFFTTester(_) {
              val result = testBitReversal(0.0001, testSignal, paramsFixed)
              //testFFT(0.0008, testSignal, paramsFixed)
            })
          //testerSDFFFT.fixedTester(paramsFixed, testSignal) should be(true)
        }
      }
    }
  }
}
