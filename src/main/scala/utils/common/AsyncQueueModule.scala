package dspblocks.utils.common

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.util._
import dsptools.numbers._

class AsyncQueueModule[T <: Data: Real](gen: T, sync: Int = 3, depth: Int = 32, safe: Boolean = true) extends Module {
  val io = IO(new CrossingIO(gen))

  //val asyncQueue0 = Module(new AsyncQueue(chiselTypeOf(in0.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))

  val asyncQueue = Module(new AsyncQueue(gen, AsyncQueueParams(depth = depth, sync = sync, safe = safe)))
  asyncQueue.io.enq_clock := io.enq_clock
  asyncQueue.io.enq_reset := io.enq_reset
  asyncQueue.io.deq_clock := io.deq_clock
  asyncQueue.io.deq_reset := io.deq_reset

  asyncQueue.io.enq <> io.enq
  io.deq <> asyncQueue.io.deq
}

object AsyncQueueApp extends App {
  (new ChiselStage).execute(
    Array("--target-dir", "verilog/AsyncQueueModule"),
    Seq(ChiselGeneratorAnnotation(() => new AsyncQueueModule(SInt(32.W))))
  )
}
