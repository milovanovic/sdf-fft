package dspblocks.utils.common

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import logger.{LogLevel, LogLevelAnnotation}

class SimpleHeaderInserterSpec extends AnyFlatSpec with ChiselScalatestTester {

  behavior.of("Simple header inserter module")
  for (packetSize <- Seq(12, 6)) {
    for (numPackets <- Seq(8, 5)) {
      for (headerValue <- Seq(4, 5)) {
        it should f"work for data type UInt(8.W), packetSize = $packetSize, numPackets  = $numPackets and headerValue = $headerValue" in {
          val dataType = UInt(8.W)
          Random.setSeed(11110L)
          val in = Seq.fill(packetSize * numPackets)(
            Seq.fill(packetSize * numPackets)(Random.nextInt(1 << (dataType.getWidth)).toDouble)
          )
          test(new SimpleHeaderInserter(dataType, packetSize, headerValue, numPackets))
            .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
            .runPeekPoke(new SimpleHeaderInserterTester(_, in, 0, headerValue, packetSize, numPackets))
        }
      }
    }
  }
}
