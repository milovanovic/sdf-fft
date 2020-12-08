// SPDX-License-Identifier: Apache-2.0

package fft

import org.scalatest.{FlatSpec, Matchers}
import chisel3.util.log2Up

/**
 * This test should run testers for Radix 2^2 and run time configurable fft size
 */

class Radix22RunTimeSpec extends FlatSpec with Matchers {
   
  /**
   *
   * run time configurable number of points tests for radix 2^2 algorithm, set of various fft sizes, both dit and dif
   * Use default values for dataWidth, binPoint, trimType etc. Check FFTParams.fixed and FFTParams.DSPReal functions inside FFT.scala file for other parameter settings.
   * 512 - 2, 8 32 128 512 - can support only odd number of stages through run time configurability
   * 1024 - 4, 16, 64, 256 1024 - can support only even number of stages through run time configurability
   *
   */
  val testerRTC = new FixedSDFFFTRunTimeTester
  
  //Test both odd and even number of stages
  for (i <- Seq(512, 1024)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix-2^2 $decType FFT, size $i and test run time configurable fft (Module SDFChainRadix22) size with no growing logic" in {
        val paramsFixed = FFTParams.fixed(
          numPoints = i,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          runTime = true)
        val paramsDspReal = FFTParams.DSPReal(
          numPoints = i,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          runTime = true)
      //  testerRTC.dspRealTester(paramsDspReal) should be (true)
        testerRTC.fixedTester(paramsFixed) should be (true)
      }
    }
  }
  
  for (i <- Seq(512, 1024)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix-2^2 $decType FFT, size $i and test run time configurable fft (Module SDFChainRadix22) size with appropiate growing logic settings" in {
        val exptmp = Array.fill(log2Up(i))(1)//Array.fill(log2Up(i))(1).tail.zipWithIndex.map { case (e,ind) => if ((ind+1) % 2 == 0) 0 else 1 }
        val expandLogic = if (decType == DIFDecimType) 1 +: exptmp else 1 +: exptmp.reverse
        val paramsFixed = FFTParams.fixed(
          numPoints = i, 
          numAddPipes = 1,
          decimType = decType,
          expandLogic = expandLogic,
          keepMSBorLSB = Array.fill(log2Up(i))(true), // stages which do not have grow - keep MSB (div 2)
          runTime = true)
        val paramsDspReal = FFTParams.DSPReal(
          numPoints = i,
          numAddPipes = 1,
          decimType = decType,
          expandLogic = expandLogic,
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          runTime = true)
       // testerRTC.dspRealTester(paramsDspReal) should be (true)
        testerRTC.fixedTester(paramsFixed) should be (true)
      }
    }
  }

  /**
   *
   * Highly run time configurable number of points (pow of 2) tests for radix 2^2 algorithm, set of various fftSize, both dit and dif and various scaling options
   * Use default values for dataWidth, binPoint, trimType etc. Check FFTParams.fixed and FFTParams.DSPReal functions inside FFT.scala file for other parameter settings.
   *
   */
  
  //Test both odd and even number of stages. Through run time configurability both even and odd number of stages can be used.
  //for (i<-Seq(512, 1024)) {
  for (i <- Seq(64)) {
    for (decType <- Seq(DIFDecimType)) {//, DITDecimType)) {
      it should f"compute radix-2^2 $decType FFT, size $i and test highly run time configurable fft (Module SDFChainRadix22RunTime) size with no growing logic" in {
        val paramsFixed = FFTParams.fixed(
          numPoints = i, 
          numAddPipes = 1, 
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          runTime = true,
          runTimeR22 = Some(true))
        val paramsDspReal = FFTParams.DSPReal(
          numPoints = i,
          numAddPipes = 1,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          runTime = true,
          runTimeR22 = Some(true))
        //testerRTC.dspRealTester(paramsDspReal) should be (true)
        testerRTC.fixedTester(paramsFixed) should be (true)
      }
    }
  }
  
  // set appropiate growing logic
  for (i <- Seq(512, 1024)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix-2^2 $decType FFT, size $i and test highly run time configurable fft (Module SDFChainRadix22RunTime) size with appropiate growing logic settings" in {
        val exptmp = Array.fill(log2Up(i))(1).tail.zipWithIndex.map { case (e,ind) => if ((ind+1) % 2 == 0) 0 else 1 }
        val expandLogic = if (decType == DIFDecimType) 1 +: exptmp else 1 +: exptmp.reverse
        val paramsFixed = FFTParams.fixed(
           numPoints = i,
           numAddPipes = 1,
           decimType = decType,
           keepMSBorLSB = Array.fill(log2Up(i))(true),
           expandLogic = expandLogic,
           runTime = true,
           runTimeR22 = Some(true))
        val paramsDspReal = FFTParams.DSPReal(
          numPoints = i,
          numAddPipes = 1,
          decimType = decType, 
          expandLogic = expandLogic,
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          runTime = true,
          runTimeR22 = Some(true))
       // testerRTC.dspRealTester(paramsDspReal) should be (true)
        testerRTC.fixedTester(paramsFixed) should be (true)
      }
    }
  }
}


