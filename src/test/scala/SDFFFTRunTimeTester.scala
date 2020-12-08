// SPDX-License-Identifier: Apache-2.0

package fft

import dsptools.DspTester
import dsptools.numbers._

import chisel3.iotesters.PeekPokeTester
import chisel3.experimental.FixedPoint
import chisel3.util.log2Up

import breeze.math.Complex
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg.DenseVector

import scala.math.pow
import scala.util.Random

/*
* This tester tests run time configurable fft size
* Common tester is used for radix 2, radix 2^2, and radix 2^2 module which provides full run time configurability.
*/

trait SDFFFTRunTimeTester[T <: chisel3.Data] extends HasTesterUtil[SDFFFT[T]] with HasSignalUtils { this: PeekPokeTester[SDFFFT[T]] =>
  def c: SDFFFT[T]
  
  def testSDFFFTRunTime(tol: Int = 3,  params: FFTParams[T]) {
    val numStages = log2Up(params.numPoints)
    var fftSizeColl : Seq[Int] = Seq.empty
    if (params.sdfRadix == "2" || params.runTimeR22.getOrElse(false)) {
      fftSizeColl = (1 to numStages).map(stage => pow(2,stage).toInt)
    }
    else {
      fftSizeColl = if (numStages % 2 == 0) (2 to numStages by 2).map(stage => pow(2,stage).toInt) else (1 to numStages by 2).map(stage => pow(2,stage).toInt)
    }
    var cyclesWait = 0
    var idx = 0
    poke(c.io.out.ready, 1)
    while (idx < 1) { // for shuffling more than once, change this condition
      val fftSizesShuffle = Random.shuffle(fftSizeColl)
      for (size <- fftSizesShuffle) {
        cyclesWait = 0
        var cnt = 0
        var scalingFactor = size
        val testTone = getTone(size, 0.04, 0.1)
        val inp = if (params.decimType == DITDecimType) bitrevorder_data(testTone) else testTone
        val out = if (params.decimType == DITDecimType) fourierTr(DenseVector(testTone.toArray)).toScalaVector else bitrevorder_data(fourierTr(DenseVector(inp.toArray)).toScalaVector)
        if (params.expandLogic.sum != 0)
          if (params.decimType == DIFDecimType) 
            scalingFactor = pow(2, params.expandLogic.drop(numStages - log2Up(size)).filter(_ != 1).size).toInt
          else
            scalingFactor = pow(2, params.expandLogic.take(log2Up(size)).filter(_ != 1).size).toInt
        else
          scalingFactor = size
        
        poke(c.io.in.valid, 0)
        poke(c.io.fftSize.get, log2Up(size).toInt)
        step(5)
        poke(c.io.in.valid, 1)
        val input = inp.iterator
        var output = Seq[Complex]()
        
        while (output.length < size) {
          if (input.hasNext && peek(c.io.in.ready)) {
            poke(c.io.in.bits, input.next())
          }
          if (peek(c.io.out.valid)) {
            params.protoIQ.real match {
              case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, out(output.length)/scalingFactor) }
              case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, out(output.length)/scalingFactor) }
            }
            output = output :+ peek(c.io.out.bits)
          }
          step(1)
        }
        val bitrevorderOutput = if (params.decimType == DIFDecimType) bitrevorder_data(output) else output
        // uncomment this part for ploting diagrams
//         if (size > 256) {
//           val scaledfft =  fourierTr(DenseVector(testTone.toArray)).toScalaVector.map(c => c/scalingFactor)
//           plot_fft(bitrevorderOutput, scaledfft)
//         }
        poke(c.io.in.valid, 0)
        step(2*inp.length)
        reset(2)
      }
      idx += 1
    }
  }
}

//val c!
class FixedPointSDFFFTRunTimeTester(val c: SDFFFT[FixedPoint]) extends DspTester(c) with SDFFFTRunTimeTester[FixedPoint] {
}
class DspRealSDFFFTRunTimeTester(val c: SDFFFT[DspReal]) extends DspTester(c) with SDFFFTRunTimeTester[DspReal] {
}

class FixedSDFFFTRunTimeTester {
  def fixedTester(params: FFTParams[FixedPoint], tolLSBs: Int = 8): Boolean = {
	  dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")){ c => new FixedPointSDFFFTRunTimeTester(c) { this.testSDFFFTRunTime(tolLSBs, params)} }
  }
  def dspRealTester(params: FFTParams[DspReal], tolReal: Int = 12): Boolean = {
	  dsptools.Driver.execute(() => new SDFFFT(params), Array("-tbn", "verilator")){ c => new DspRealSDFFFTRunTimeTester(c) { this.testSDFFFTRunTime(tolReal, params)} }
  }
}
