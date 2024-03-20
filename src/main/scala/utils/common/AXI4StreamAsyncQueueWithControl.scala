package dspblocks.utils.common

import dspblocks._
import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

case class AXI4StreamAsyncQueueWithControlParams(
  ctrlBits: Int,
  // AsyncQueue params
  sync:  Int,
  depth: Int,
  safe:  Boolean) {
  // add some requirements
}

/* AXI4StreamAsyncQueueWithControl Bundle */
class AXI4StreamAsyncQueueWithControlIO(ctrlBits: Int) extends Bundle {
  // Cotrol bits
  val in_ctrl = Input(UInt(ctrlBits.W))
  val out_ctrl = Output(UInt(ctrlBits.W))

  // Clock & reset (Write side)
  val write_clock = Input(Clock())
  val write_reset = Input(Bool())
}

class AXI4StreamAsyncQueueWithControlBlock(params: AXI4StreamAsyncQueueWithControlParams)(implicit p: Parameters)
    extends AsyncQueueWithControlBlock[
      AXI4MasterPortParameters,
      AXI4SlavePortParameters,
      AXI4EdgeParameters,
      AXI4EdgeParameters,
      AXI4Bundle
    ](params)
    with AXI4DspBlock {
  override val mem = None
}

abstract class AsyncQueueWithControlBlock[D, U, E, O, B <: Data](params: AXI4StreamAsyncQueueWithControlParams)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B] {
  val streamNode = AXI4StreamIdentityNode()
  lazy val io = Wire(new AXI4StreamAsyncQueueWithControlIO(params.ctrlBits))

  lazy val module = new LazyModuleImp(this) {
    // IOs
    val (in, _) = streamNode.in.head
    val (out, _) = streamNode.out.head
    // IOs

    // AsyncQueue
    val asyncQueue = Module(
      new AsyncQueueModule(
        chiselTypeOf(Cat(io.in_ctrl, in.bits.data)),
        depth = params.depth,
        sync = params.sync,
        safe = params.safe
      )
    )

    // Connect asyncQueue (Write side)
    asyncQueue.io.enq_clock := io.write_clock
    asyncQueue.io.enq_reset := io.write_reset
    asyncQueue.io.enq.bits := Cat(io.in_ctrl, in.bits.data).asUInt
    asyncQueue.io.enq.valid := in.valid
    in.ready := asyncQueue.io.enq.ready

    // Connect asyncQueue (Read side)
    asyncQueue.io.deq_clock := clock
    asyncQueue.io.deq_reset := reset.asBool
    asyncQueue.io.deq.ready := out.ready
    out.valid := asyncQueue.io.deq.valid
    out.bits.data := asyncQueue.io.deq.bits(in.bits.data.getWidth - 1, 0).asUInt
    io.out_ctrl := asyncQueue.io.deq.bits(in.bits.data.getWidth + io.in_ctrl.getWidth - 1, in.bits.data.getWidth)

  }
}

trait AXI4StreamAsyncQueueWithControlStandalone extends AXI4StreamAsyncQueueWithControlBlock {
  def beatBytes: Int = 4

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(
    AXI4StreamMasterParameters(n = beatBytes)
  ) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }

  def makeCustomIO() = {
    val io2: AXI4StreamAsyncQueueWithControlIO = IO(io.cloneType)
    io2.suggestName("io")
    io2 <> io
    io2
  }

  val pins = InModuleBody {
    makeCustomIO()
  }
}

object AXI4StreamAsyncQueueWithControlApp extends App {
  implicit val p: Parameters = Parameters.empty

  val params: AXI4StreamAsyncQueueWithControlParams = AXI4StreamAsyncQueueWithControlParams(
    ctrlBits = 3,
    sync = 4,
    depth = 2048,
    safe = true
  )
  val lazyDUT = LazyModule(
    new AXI4StreamAsyncQueueWithControlBlock(params) with AXI4StreamAsyncQueueWithControlStandalone
  )
  (new ChiselStage).execute(
    Array("--target-dir", "verilog/AXI4StreamAsyncQueueWithControl"),
    Seq(ChiselGeneratorAnnotation(() => lazyDUT.module))
  )
}
