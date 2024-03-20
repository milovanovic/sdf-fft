package dspblocks.utils.common

import chisel3._
import chiseltest.iotesters.PeekPokeTester
import dsptools.misc.PeekPokeDspExtensions
import scala.util.Random
import scala.math._
import logger.{LogLevel, LogLevelAnnotation}

class PasstroughOrIgnoreStreamTester[T <: Data](
  dut:           PasstroughOrIgnoreStream[T],
  in:            Seq[Seq[Double]],
  tol:           Int,
  numRowsToSend: Int)
    extends PeekPokeTester(dut)
    with PeekPokeDspExtensions {
  //Random.setSeed(11110L)

  val inputData = in.flatten
  inputData.map(c => println(c.toString))

  var expectedData = inputData.take(dut.maxColumns * numRowsToSend)
  val input1 = inputData.iterator
  var inValid = 0

  poke(dut.io.numRows, numRowsToSend)
  poke(dut.io.enStreamCut, 1)

  step(5)
  var cntValid = 0

  while (cntValid < (dut.maxColumns * numRowsToSend)) {
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
      dut.dataType match {
        case uInt: UInt => expect(dut.io.out.bits, expectedData(cntValid))
        case sInt: SInt => expect(dut.io.out.bits, expectedData(cntValid))
        case _ =>
          assert(abs(expectedData.head - peek(dut.io.out.bits)) <= tol, "Mismatch!!!")
      }
      cntValid = cntValid + 1
    }
    step(1)
  }
  poke(dut.io.in.valid, 0)
  step(50)
}
