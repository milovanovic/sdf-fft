package dspblocks.sdffft

import org.scalatest.flatspec.AnyFlatSpec
import chisel3.util.log2Up
import chiseltest.ChiselScalatestTester
import chiseltest._
import logger.{LogLevel, LogLevelAnnotation}

/**
  * This test should run testers for radix 2 module and run time configurable fft size
  */

class Radix2RunTimeSpec extends AnyFlatSpec with ChiselScalatestTester {

  for (i <- Seq(512, 1024)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix 2 $decType FFT, size $i and test run time configurable fft size with no growing logic" in {
        val paramsFixed = FFTParams.fixed(
          numPoints = i,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          runTime = true,
          sdfRadix = "2"
        )
        val paramsDspReal = FFTParams.DSPReal(
          numPoints = i,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          sdfRadix = "2",
          runTime = true
        )
        //   testerRTC.dspRealTester(paramsDspReal) should be (true)
        //testerRTC.fixedTester(paramsFixed) should be(true)

        test(new SDFFFT(paramsFixed))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
          .runPeekPoke(new SDFFFTRunTimeTester(_) {
            testSDFFFTRunTime(0.001, paramsFixed)
          })
      }
    }
  }

  for (i <- Seq(512, 1024)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix-2 $decType FFT, size $i and test run time configurable fft size with appropiate growing logic settings" ignore {
        val exptmp =
          Array.fill(log2Up(i))(
            1
          ) //Array.fill(log2Up(i))(1).tail.zipWithIndex.map { case (e,ind) => if ((ind+1) % 2 == 0) 0 else 1 }
        //val expandLogic = if (decType == DIFDecimType) 1 +: exptmp else 1 +: exptmp.reverse
        val paramsFixed = FFTParams.fixed(
          numPoints = i,
          numAddPipes = 1,
          decimType = decType,
          expandLogic = exptmp,
          keepMSBorLSB = Array.fill(log2Up(i))(true), // stages which do not have grow keep MSB (div 2)
          sdfRadix = "2",
          runTime = true
        )
        test(new SDFFFT(paramsFixed))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
          .runPeekPoke(new SDFFFTRunTimeTester(_) {
            testSDFFFTRunTime(0.02, paramsFixed)
          })
      }
    }
  }
}
