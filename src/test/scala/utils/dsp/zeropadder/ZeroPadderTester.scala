package dspblocks.utils.dsp.zeropadder

import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.misc.PeekPokeDspExtensions
import scala.util.Random
import scala.math._

class ZeroPadderTester[T <: Data](dut: ZeroPadderNative[T], in: Seq[Seq[Double]], tol: Int, packetSizeStart: Int)
    extends PeekPokeTester(dut)
    with PeekPokeDspExtensions {
  // add some checkers
  //require(dut.params.numberOfPackets == in.length)
  //require(in.foreach())
  //Random.setSeed(11110L)

  val inputData = in.flatten // this is total number of data
  val numZeros = dut.params.packetSizeEnd - packetSizeStart //dut.params.packetSizeStart
  var expectedData = in.flatMap { c => c ++: Seq.fill(numZeros)(0.0) }

  val input1 = inputData.iterator

  var inValid = 0

  poke(dut.io.packetSizeStart, packetSizeStart)
  poke(dut.io.packetSizeEnd, dut.params.packetSizeEnd)
  poke(dut.io.numberOfPackets, dut.params.numberOfPackets)
  step(5)

  while (expectedData.nonEmpty) {
    inValid = Random.nextInt(2)
    poke(dut.io.out.ready, Random.nextInt(2))
    if (input1.hasNext) {
      poke(dut.io.in.valid, inValid)
      if (peek(dut.io.in.ready) == 1 && peek(dut.io.in.valid) == 1) {
        poke(dut.io.in.bits, input1.next())
      }
    } else {
      poke(dut.io.in.valid, 0)
    }
    if (peek(dut.io.out.ready) == 1 && peek(dut.io.out.valid) == 1) {
      /*dut.params.proto match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.io.out.bits, expectedData.head) }
          case _ => fixTolLSBs.withValue(tol) { expect(dut.io.out.bits, expectedData.head) }
        }*/
      dut.params.proto match {
        case uInt: UInt => expect(dut.io.out.bits, expectedData.head)
        case sInt: SInt => expect(dut.io.out.bits, expectedData.head)
        case _ =>
          assert(abs(expectedData.head - peek(dut.io.out.bits)) <= tol, "Mismatch!!!")
      }
      //println(lastOut.toString)
      if (expectedData.length == 1) {
        expect(dut.io.lastOut, 1)
      }
      expectedData = expectedData.tail
    }
    step(1)
  }
  poke(dut.io.in.valid, 0)
  step(50)
}
