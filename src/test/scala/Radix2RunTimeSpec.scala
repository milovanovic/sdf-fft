// SPDX-License-Identifier: Apache-2.0

package fft

import org.scalatest.{FlatSpec, Matchers}
import chisel3.util.log2Up

/**
* This test should run testers for radix 2 module and run time configurable fft size
*/

class Radix2RunTimeSpec extends FlatSpec with Matchers {
   
 
  //Test both odd and even number of stages
  val testerRTC = new FixedSDFFFTRunTimeTester
  
  for (i<-Seq(512,1024)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix 2 $decType FFT, size $i and test run time configurable fft size with no growing logic" in {
        val paramsFixed = FFTParams.fixed(
          numPoints = i,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          runTime = true,
          sdfRadix = "2")
        val paramsDspReal = FFTParams.DSPReal(
          numPoints = i,
          decimType = decType,
          expandLogic = Array.fill(log2Up(i))(0),
          keepMSBorLSB = Array.fill(log2Up(i))(true),
          sdfRadix = "2",
          runTime = true)
     //   testerRTC.dspRealTester(paramsDspReal) should be (true)
        testerRTC.fixedTester(paramsFixed) should be (true)
      }
    }
  }
   
  for (i<-Seq(512, 1024)) {
    for (decType <- Seq(DIFDecimType, DITDecimType)) {
      it should f"compute radix-2 $decType FFT, size $i and test run time configurable fft size with appropiate growing logic settings" in {
        val exptmp = Array.fill(log2Up(i))(1)//Array.fill(log2Up(i))(1).tail.zipWithIndex.map { case (e,ind) => if ((ind+1) % 2 == 0) 0 else 1 }
        //val expandLogic = if (decType == DIFDecimType) 1 +: exptmp else 1 +: exptmp.reverse
        val paramsFixed = FFTParams.fixed(
          numPoints = i, 
          numAddPipes = 1,
          decimType = decType,
          expandLogic = exptmp,
          keepMSBorLSB = Array.fill(log2Up(i))(true), // stages which do not have grow keep MSB (div 2)
          sdfRadix = "2",
          runTime = true)
        testerRTC.fixedTester(paramsFixed) should be (true)
      }
    }
  }
}


