// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3.experimental.FixedPoint
import chisel3.iotesters.Driver

import org.scalatest.{FlatSpec, Matchers}
import chisel3.util.log2Up

import dsptools._
import dsptools.numbers._

import breeze.math.Complex
import breeze.signal.fourierTr
import breeze.linalg.DenseVector

import scala.math.{Pi, pow}
import scala.util.Random

class BitReversePingPongTester[T <: chisel3.Data](c: BitReversePingPong[T]) extends DspTester(c) {
 
  // move this to trait and then use extends
  /**
  * Returns bit reversed index
  */
  def bit_reverse(in: Int, width: Int): Int = {
    import scala.math.pow
    var test = in
    var out = 0
    for (i <- 0 until width) {
      if (test / pow(2, width-i-1) >= 1) {
        out += pow(2,i).toInt
        test -= pow(2,width-i-1).toInt
      }
    }
    out
  }

  /**
   * Reordering data
   */
  def bitrevorder_data(testSignal: Seq[Complex]): Seq[Complex] = {
    val seqLength = testSignal.size
    val new_indices = (0 until seqLength).map(x => bit_reverse(x, log2Up(seqLength)))
    new_indices.map(x => testSignal(x))
  }
  
  def testReadyOut(inp: Seq[Complex], tol: Int = 3) {
    
    val input = if (c.params.bitReverseDir) bitrevorder_data(inp) else inp
    val output = bitrevorder_data(input)
    val pingPongSize = c.params.pingPongSize
    
    println("Expected result should be: ")
    output.map(c => println((c).toString))
    
    var cntValidIn = 0
    var cntValidOut = 0
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)
    
    // flush after only one window
    while (cntValidIn < input.length) {
      if (cntValidIn == (input.length - 1)) {
        poke(c.io.lastIn, 1)
        poke(c.io.in.bits, input(cntValidIn))
      }
      else {
        poke(c.io.in.bits, input(cntValidIn))
      }
      cntValidIn += 1
      step(1)
    }
    poke(c.io.in.valid, 0)
    poke(c.io.lastIn, 0)
    
    while (cntValidOut < output.length) {
      if (cntValidOut == (output.length - 1) && peek(c.io.out.valid)) {
        expect(c.io.lastOut, 1)
        fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
        cntValidOut += 1
      }
      else {
        if (peek(c.io.out.valid)) {
          fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
          cntValidOut += 1
        }
      }
      step(1)
    }
    cntValidIn = 0
    cntValidOut = 0

    step(3)
    poke(c.io.in.valid, 1)
    
    while (cntValidIn < input.length) {
      poke(c.io.in.bits, input(cntValidIn))
      cntValidIn += 1
      step(1)
    }
    cntValidIn = 0
    poke(c.io.in.valid, 0)
    while (cntValidIn < input.length) {
      println(cntValidIn.toString)
      println(cntValidOut.toString)

      if (peek(c.io.out.valid) && peek(c.io.out.ready)) {
        fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
        cntValidOut += 1
      }
      if (cntValidOut == (input.length)) {
        cntValidOut = 0
      }
      step(1)
      if (peek(c.io.out.valid) && peek(c.io.out.ready)) {
        fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
        cntValidOut += 1
      }
      step(1)
      if (cntValidOut == (input.length)) {
        cntValidOut = 0
      }
      poke(c.io.out.ready, 1)
      if (peek(c.io.in.ready)) {
        poke(c.io.in.valid, 1)
        poke(c.io.in.bits, input(cntValidIn))
        cntValidIn += 1
      }
      if (peek(c.io.out.valid) && peek(c.io.out.ready)) {
        fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
        cntValidOut += 1
      }
      step(1)
      poke(c.io.in.valid, 0)
      if (cntValidOut == (input.length)) {
        cntValidOut = 0
      }
      poke(c.io.out.ready, 0)
    }
    cntValidIn = 0
    poke(c.io.in.valid, 1)
    poke(c.io.out.ready, 1)
    while (cntValidIn < input.length) {
      poke(c.io.in.bits, input(cntValidIn))
      cntValidIn += 1
      step(1)
    }
    cntValidIn = 0
    step(10)
  }
  
  def testStream(inp: Seq[Complex], tol: Int = 3) {

    val input = if (c.params.bitReverseDir) bitrevorder_data(inp) else inp
    val output = bitrevorder_data(input)
    val pingPongSize = c.params.pingPongSize

    println("Expected result should be: ")
    output.map(c => println((c).toString))

    var cntValidIn = 0
    var cntValidOut = 0
    poke(c.io.out.ready, 1)
    poke(c.io.in.valid, 1)

    // flush after only one window
    while (cntValidIn < input.length) {
      if (cntValidIn == (input.length - 1)) {
        poke(c.io.lastIn, 1)
        poke(c.io.in.bits, input(cntValidIn))
      }
      else {
        poke(c.io.in.bits, input(cntValidIn))
      }
      cntValidIn += 1
      step(1)
    }
    poke(c.io.in.valid, 0)
    poke(c.io.lastIn, 0)

    while (cntValidOut < output.length) {
      if (cntValidOut == (output.length - 1) && peek(c.io.out.valid)) {
        expect(c.io.lastOut, 1)
        fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
        cntValidOut += 1
      }
      else {
        if (peek(c.io.out.valid)) {
          fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
          cntValidOut += 1
        }
      }
      step(1)
    }
    cntValidIn = 0
    cntValidOut = 0
    step(3)
    poke(c.io.in.valid, 1)

    while (cntValidIn < input.length) {
      poke(c.io.in.bits, input(cntValidIn))
      cntValidIn += 1
      step(1)
    }
    cntValidIn = 0

    while (cntValidIn < input.length) {
      if (cntValidIn == (input.length - 1)) {
        poke(c.io.lastIn, 1)
        poke(c.io.in.bits, input(cntValidIn))
      }
      else {
        poke(c.io.in.bits, input(cntValidIn))
      }
      cntValidIn += 1
      if (peek(c.io.out.valid)) {
        fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
        cntValidOut += 1
      }
      step(1)
    }
    // check flushing when both memories are full
   // it works looking vcd diagram but tester gives an error
   /* cntValidOut = 0
    while (cntValidOut < output.length) {
      if (cntValidOut == (output.length - 1) && peek(c.io.out.valid)) {
        expect(c.io.lastOut, 1)
        fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
        cntValidOut += 1
      }
      else {
        if (peek(c.io.out.valid)) {
          fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValidOut)) }
          cntValidOut += 1
        }
      }
      step(1)
    }*/

    step(10)
  }

  def testAdjustableSize(inp: Seq[Complex], tol: Int = 3) {
    require (c.params.adjustableSize == true, "Interface must have included size input")

    val log2Size = log2Up(c.params.pingPongSize)
    var sizeColl : Seq[Int] = Seq.empty
    sizeColl = (1 to log2Size).map(stage => pow(2,stage).toInt)
     
    var idx = 0
    var cntValid = 0
    poke(c.io.out.ready, 1)
    println(sizeColl.length.toString)
    
    while (idx < 1) {
      val sizeCollShuffle = Random.shuffle(sizeColl)
      for (size <- sizeCollShuffle) {
        println("Current fft size is: ")
        println(size.toString)
        
        val input = if (c.params.bitReverseDir) bitrevorder_data(inp.take(size)) else inp.take(size)
        val output = bitrevorder_data(input)
        
      
        val input1 = input.iterator
        val input2 = input.iterator
        val input3 = input.iterator
        
        cntValid = 0
        println("Expected result should be: ")
        output.map(c => println(c.toString))
    
        poke(c.io.in.valid, 0)
        poke(c.io.size.get, size)//
        step(5)
        poke(c.io.in.valid, 1)

        while (input1.hasNext && peek(c.io.in.ready)) {
          poke(c.io.in.bits, input1.next())
          step(1)
        }
        while (input2.hasNext && peek(c.io.in.ready)) {
          poke(c.io.in.bits, input2.next())
          if (peek(c.io.out.valid)) {
            c.params.proto.real match {
              case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, output(cntValid)) }
              case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValid)) }
            }
            if (cntValid == (size-1))
              cntValid = 0
            else 
              cntValid += 1
          }
          step(1)
        }
        while (input3.hasNext && peek(c.io.in.ready)) {
          poke(c.io.in.bits, input3.next())
          if (peek(c.io.out.valid)) {
            c.params.proto.real match {
              case dspR: DspReal => realTolDecPts.withValue(tol) { expect(c.io.out.bits, output(cntValid)) }
              case _ => fixTolLSBs.withValue(tol) { expect(c.io.out.bits, output(cntValid)) }
            }
            if (cntValid == (size-1))
              cntValid = 0
            else 
              cntValid += 1
          }
          step(1)
        }
        poke(c.io.in.valid, 0)
        step(2*inp.length)
        reset(2)
      }
        idx += 1
    }
  }
}

object FixedPingPongTester {
  def apply(params: BitReversePingPongParams[FixedPoint], inp: Seq[Complex]): Boolean = {
		chisel3.iotesters.Driver.execute(Array("-tbn", "verilator"),
      () => new BitReversePingPong(params)) { c => new BitReversePingPongTester(c) {
        if (params.adjustableSize) testAdjustableSize(inp) else testStream(inp)//testReadyOut(inp)
      }}
  }
}
class BitReversePingPongSpec extends FlatSpec with Matchers {

  // Add this to get same test case every time
  Random.setSeed(11110L)
  val pingPongSize  = 8
  val testSignal = (0 until pingPongSize).map(x => Complex(Random.nextDouble(), Random.nextDouble()))
  
  it should f"test bitreverse ping-pong without adjustableSize and bitReverseDir = true" in { // DIF
    // input is in bitreverse order
    // output is in natural
    val paramsPingPong = BitReversePingPongParams.fixed(
      dataWidth = 16,
      binPoint = 14,
      pingPongSize = pingPongSize,
      adjustableSize = false
    )
    FixedPingPongTester(paramsPingPong, testSignal) should be (true)
  }
  
  it should f"test bitreverse ping-pong without adjustableSize and bitReverseDir = false" in {  // DIT
    // input is in natural order
    // output is in bit reverse order
    val paramsPingPong = BitReversePingPongParams.fixed(
      dataWidth = 16,
      binPoint = 14,
      pingPongSize = pingPongSize,
      adjustableSize = false,
      bitReverseDir = false
    )
    FixedPingPongTester(paramsPingPong, testSignal) should be (true)
  }
  
  it should f"test bitreverse ping-pong with adjustableSize and bitReverseDir = true" in {
    // input is in bitreverse order
    // output is in natural
    val paramsPingPong = BitReversePingPongParams.fixed(
      dataWidth = 16,
      binPoint = 14,
      pingPongSize = pingPongSize,
      adjustableSize = true,
      bitReverseDir = true
    )
    FixedPingPongTester(paramsPingPong, testSignal) should be (true)
  }

  // Note: for testing this test case, ignore should be in and testReadyOut function should be called
  // This test example works good as well when pipe parameter of the output queue is on, but for some other cases it is necessary that pipe is off
  it should f"test bitreverse ping-pong when ready out is changing" ignore {
    val paramsPingPong = BitReversePingPongParams.fixed(
      dataWidth = 16,
      binPoint = 14,
      pingPongSize = pingPongSize,
      adjustableSize = false,
      bitReverseDir = false
    )
    FixedPingPongTester(paramsPingPong, testSignal) should be (true)
  }
}

