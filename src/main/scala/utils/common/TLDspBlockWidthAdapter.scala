package dspblocks.utils.common

import chisel3._
import chisel3.util._
import dspblocks.{DspBlock, TLDspBlock}
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

trait TLSimpleWidthAdapterPins extends TLSimpleWidthAdapter {
  val in: ModuleValue[AXI4StreamBundle] = {
    implicit val valName: ValName = ValName(s"inIO")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
    streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = 2))) := in
    InModuleBody { in.makeIO() }
  }
}

class TLSimpleWidthAdapter(beatBytes: Int, n: Int)(implicit p: Parameters)
    extends SimpleWidthAdapter[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](
      beatBytes,
      n
    )
    with TLDspBlock {
  override val mem: Option[Nothing] = None
}

abstract class SimpleWidthAdapter[D, U, E, O, B <: Data](val beatBytes: Int, n: Int)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B] {
  val streamNode: AXI4StreamNexusNode = AXI4StreamNexusNode(
    masterFn = (ms: Seq[AXI4StreamMasterPortParameters]) =>
      AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters(n = beatBytes * n))),
    slaveFn = ss => { AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _)) }
  )
  lazy val module: LazyModuleImp = new LazyModuleImp(this) {
    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip
    val inWidth: Int = ins.head.bits.data.getWidth
    // require that ins is 1 and out is 1 as well
    val cntValidIn: UInt = RegInit(0.U(log2Up(n).W))
    val inDataRegs: Vec[UInt] = RegInit(VecInit(Seq.fill(n)(0.U(inWidth.W))))
    ins.head.ready := outs.map(c => c.ready).foldLeft(true.B)(_ && _)

    when(ins.head.ready && ins.head.valid) {
      cntValidIn := cntValidIn + 1.U
      inDataRegs(cntValidIn) := ins.head.bits.data
    }
    outs.head.valid := cntValidIn === (n - 1).U && (ins.head.valid && ins.head.ready)
    outs.head.bits.data := inDataRegs
      .take(n - 1)
      .reverse
      .foldRight(ins.head.bits.data)(Cat(_, _)) // check as well foldRight
    outs.head.bits.last := (cntValidIn === (n - 1).U) && ins.head.bits.last && (ins.head.valid && ins.head.ready)
  }
}
