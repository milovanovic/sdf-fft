package dspblocks.utils.dsp.zeropadder

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class ZeroPadderSpec extends AnyFlatSpec with ChiselScalatestTester {

  behavior.of("Zero padding module")

  // useBlockRam needs to be always on true -> TODO: make it work as well when useBlockRam is not true
  // queue depth is set to be 2*packetSizeEnd + packetSizeEnd/2
  for (numberOfPackets <- Seq(5, 8)) {
    for (packetSizeEnd <- Seq(16, 32)) {
      val packetSizesStart = (2 to packetSizeEnd by 4).toSeq
      for (packetSizeStart <- packetSizesStart) {
        for (useQueue <- Seq(true, false)) {
          for (useBlockRam <- Seq(true)) {
            it should f"work for data type UInt(8.W), ct parameters numberOfPackets = $numberOfPackets, packetSizeEnd = $packetSizeEnd, useBlockRam = $useBlockRam, useQueue = $useQueue , rt parameter packetSizeStart = $packetSizeStart" in {
              val params: ZeroPadderParams[UInt] = ZeroPadderParams(
                proto = UInt(8.W),
                packetSizeStart = packetSizeEnd,
                packetSizeEnd = packetSizeEnd,
                numberOfPackets = numberOfPackets,
                queueDepth = packetSizeEnd * 2 + packetSizeEnd / 2,
                useQueue = useQueue,
                useBlockRam = true
              )
              Random.setSeed(11110L)
              val in = Seq.fill(params.numberOfPackets)(
                Seq.fill(packetSizeStart)(Random.nextInt(1 << (params.proto.getWidth)).toDouble)
              )
              test(new ZeroPadderNative(params))
                .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
                .runPeekPoke(new ZeroPadderTester(_, in, 0, packetSizeStart))
            }
          }
        }
      }
    }
  }
}
