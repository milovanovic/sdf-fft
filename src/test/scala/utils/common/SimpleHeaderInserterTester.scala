package dspblocks.utils.common

import breeze.numerics.abs
import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.misc.PeekPokeDspExtensions

import scala.util.Random

class SimpleHeaderInserterTester[T <: Data](
  dut:         SimpleHeaderInserter[T],
  in:          Seq[Seq[Double]],
  tol:         Int,
  headerValue: Int,
  packetSize:  Int, // in real scenario if this is set to 1024 numPackets is actually 16 (one data is 2 byte data)
  numPackets:  Int) // the assumption is that there are 2 bytes in one data
    extends PeekPokeTester(dut)
    with PeekPokeDspExtensions {
  //Random.setSeed(11110L)

  val inputData = in.flatten
  inputData.map(c => println(c.toString))

  var expectedData = inputData
  val input1 = inputData.iterator
  var inValid = 0

  poke(dut.io.headerValue, headerValue)
  poke(dut.io.packetSize, packetSize)
  poke(dut.io.includeHeader, 1)
  poke(dut.io.numPacketsInFrame, numPackets)

  step(5)
  var cntValid = 0
  var cntSamplesInFrame = 0

  // + numPackets is if we have insertHeader set to 0
  while (cntValid < (packetSize * numPackets)) {
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
      //println("Output data is:")
      //println(peek(dut.io.out.bits).toString)

      if (cntSamplesInFrame == 0) {
        expect(dut.io.out.bits, headerValue)
      } else {
        dut.dataType match {
          case uInt: UInt => expect(dut.io.out.bits, expectedData(cntValid))
          case sInt: SInt => expect(dut.io.out.bits, expectedData(cntValid))
          case _ =>
            assert(abs(expectedData.head - peek(dut.io.out.bits)) <= tol, "Mismatch!!!")
        }
        cntValid = cntValid + 1
      }
      if (cntSamplesInFrame == packetSize) {
        cntSamplesInFrame = 0
      } else {
        cntSamplesInFrame = cntSamplesInFrame + 1
      }
    }
    step(1)
  }
  poke(dut.io.in.valid, 0)
  step(50)
}
