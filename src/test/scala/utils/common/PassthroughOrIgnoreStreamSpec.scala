package dspblocks.utils.common

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import logger.{LogLevel, LogLevelAnnotation}

class PassthroughOrIgnoreStreamSpec extends AnyFlatSpec with ChiselScalatestTester {

  behavior.of("PassthroughOrIgnoreStream module")

  for (maxColumns <- Seq(4, 6)) {
    for (maxRows <- Seq(8, 16)) {
      it should f"work for data type UInt(8.W), maxColumns = $maxColumns, maxRows  = $maxRows" in {
        val dataType = UInt(6.W)
        Random.setSeed(11110L)
        val in = Seq.fill(maxRows)(
          Seq.fill(maxColumns)(Random.nextInt(1 << (dataType.getWidth)).toDouble)
        )
        test(new PasstroughOrIgnoreStream(dataType, maxColumns, maxRows))
          .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation, LogLevelAnnotation(LogLevel.Info)))
          .runPeekPoke(new PasstroughOrIgnoreStreamTester(_, in, 0, numRowsToSend = maxRows.toInt))
      }
    }
  }
}
