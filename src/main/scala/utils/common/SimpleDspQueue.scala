package dspblocks.utils.common

import chisel3._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import org.chipsalliance.cde.config.Parameters

case class SimpleDspQueueCustomParams(
  queueDepth:     Int = 16384,
  useSyncReadMem: Boolean = true,
  useBlockRam:    Boolean = true) {}

trait AXI4SimpleDspQueueStandaloneBlock extends SimpleDspQueue {

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

class SimpleDspQueue(val params: SimpleDspQueueCustomParams) extends LazyModule()(Parameters.empty) {
  val streamNode = AXI4StreamIdentityNode()
  val depth = params.queueDepth

  lazy val module = new LazyModuleImp(this) {
    val (streamIn, _) = streamNode.in(0)
    val (streamOut, _) = streamNode.out(0)
    val queuedStream = Module(
      new QueueWithSyncReadMem(
        chiselTypeOf(streamIn.bits),
        entries = depth,
        useSyncReadMem = params.useSyncReadMem,
        useBlockRam = params.useBlockRam,
        flow = false,
        pipe = true
      )
    )

    queuedStream.io.enq.valid := streamIn.valid
    queuedStream.io.enq.bits.data := streamIn.bits.data
    queuedStream.io.enq.bits.strb := DontCare
    queuedStream.io.enq.bits.keep := DontCare
    queuedStream.io.enq.bits.id := DontCare
    queuedStream.io.enq.bits.dest := DontCare
    queuedStream.io.enq.bits.last := streamIn.bits.last
    queuedStream.io.enq.bits.user := DontCare

    streamIn.ready := queuedStream.io.enq.ready
    streamOut.bits := queuedStream.io.deq.bits
    streamOut.valid := queuedStream.io.deq.valid
    queuedStream.io.deq.ready := streamOut.ready
  }
}

object SimpleDspQueueApp extends App {
  val params: SimpleDspQueueCustomParams =
    SimpleDspQueueCustomParams(queueDepth = 16534) //SimpleDspQueueCustomParams(queueDepth = 131074, progFull = true)

  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty
  val queueModule = LazyModule(new SimpleDspQueue(params) with AXI4SimpleDspQueueStandaloneBlock)
  //chisel3.Driver.execute(args, ()=> queueModule.module)
  (new ChiselStage)
    .execute(Array("--target-dir", "verilog/SimpleDspQueue"), Seq(ChiselGeneratorAnnotation(() => queueModule.module)))

}
