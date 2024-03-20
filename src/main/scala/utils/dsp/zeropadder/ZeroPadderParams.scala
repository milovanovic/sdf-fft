package dspblocks.utils.dsp.zeropadder

import chisel3._
import dsptools.numbers._

case class ZeroPadderParams[T <: Data: Real](
  proto:           T,
  isDataComplex:   Boolean = false,
  packetSizeStart: Int = 32,
  packetSizeEnd:   Int = 32,
  queueDepth:      Int = 64, // make this as an Option[Int]
  numberOfPackets: Int = 4,
  useQueue:        Boolean = false,
  useBlockRam:     Boolean = false // make this as an Option[Boolean]
) {

  def checkNumberOfSamples(): Unit = {
    require(packetSizeStart <= packetSizeEnd)
  }
  def checkQueueDepth(): Unit = {
    if (useQueue)
      require(queueDepth >= packetSizeEnd)
  }
}
// TODO: Generation of last signal after each packet - make it configurable both in compile and run-time
