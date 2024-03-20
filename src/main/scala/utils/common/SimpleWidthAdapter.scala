package dspblocks.utils.common

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters

// Fast solution, there is no need to use Nexus node!
class AXI4StreamSimpleWidthAdapter(val beatBytes: Int, n: Int) extends LazyModule()(Parameters.empty) {
  val streamNode = AXI4StreamNexusNode(
    masterFn = (ms: Seq[AXI4StreamMasterPortParameters]) =>
      AXI4StreamMasterPortParameters(
        Seq(
          AXI4StreamMasterParameters(
            n = beatBytes * n // 4 * 8 is equal to 32
          )
        )
      ),
    slaveFn = ss => { AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _)) }
  )
  lazy val module = new LazyModuleImp(this) {
    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip
    val inWidth = ins(0).bits.data.getWidth
    // require that ins is 1 and out is 1 as well
    val cntValidIn = RegInit(0.U(log2Up(n).W))
    val inDataRegs = RegInit(VecInit(Seq.fill(n)(0.U(inWidth.W))))
    ins(0).ready := outs.map(c => c.ready).foldLeft(true.B)(_ && _)

    when(ins(0).ready && ins(0).valid) {
      cntValidIn := cntValidIn + 1.U
      inDataRegs(cntValidIn) := ins(0).bits.data
    }
    //outs(0).valid := cntValidIn === (outs.length-1).U && (ins(0).valid && ins(0).ready)
    outs(0).valid := cntValidIn === (n - 1).U && (ins(0).valid && ins(0).ready)
    outs(0).bits.data := inDataRegs
      .take(n - 1)
      .reverse
      .foldRight(ins(0).bits.data)(Cat(_, _)) // check as well foldRight
    //outs(0).bits.data := inDataRegs.take(n-1).reverse.foldLeft(ins(0).bits.data)(Cat(_, _)) // check as well foldRight
    //outs(0).bits.last := (cntValidIn === (outs.length-1).U) && ins(0).bits.last && (ins(0).valid && ins(0).ready)
    outs(0).bits.last := (cntValidIn === (n - 1).U) && ins(0).bits.last && (ins(0).valid && ins(0).ready)
  }
}

class AXI4StreamSimpleWidthAdapter2(val beatBytes: Int, n: Int) extends LazyModule()(Parameters.empty) {
  val streamNode = AXI4StreamNexusNode(
    masterFn = (ms: Seq[AXI4StreamMasterPortParameters]) =>
      AXI4StreamMasterPortParameters(
        Seq(
          AXI4StreamMasterParameters(
            n = beatBytes * n // 4 * 8 is equal to 32
          )
        )
      ),
    slaveFn = ss => { AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _)) }
  )
  lazy val module = new LazyModuleImp(this) {
    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip
    val inWidth = ins(0).bits.data.getWidth
    // require that ins is 1 and out is 1 as well
    val cntValidIn = RegInit(0.U(log2Up(n).W))
    val inDataRegs = RegInit(VecInit(Seq.fill(n)(0.U(inWidth.W))))
    ins(0).ready := outs.map(c => c.ready).foldLeft(true.B)(_ && _)

    when(ins(0).ready && ins(0).valid) {
      cntValidIn := cntValidIn + 1.U
      inDataRegs(cntValidIn) := ins(0).bits.data
    }
    //outs(0).valid := cntValidIn === (outs.length-1).U && (ins(0).valid && ins(0).ready)
    outs(0).valid := cntValidIn === (n - 1).U && (ins(0).valid && ins(0).ready)
    //outs(0).bits.data := inDataRegs.take(n-1).reverse.foldRight(ins(0).bits.data)(Cat(_, _)) // check as well foldRight
    outs(0).bits.data := inDataRegs.take(n - 1).reverse.foldLeft(ins(0).bits.data)(Cat(_, _)) // check as well foldRight
    //outs(0).bits.last := (cntValidIn === (outs.length-1).U) && ins(0).bits.last && (ins(0).valid && ins(0).ready)
    outs(0).bits.last := (cntValidIn === (n - 1).U) && ins(0).bits.last && (ins(0).valid && ins(0).ready)
  }
}

trait AXI4StreamSimpleWidthAdapterStandaloneBlock extends AXI4StreamSimpleWidthAdapter {

  val nOut = 1
  val outs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until nOut) yield {
    implicit val valName = ValName(s"outIO_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out := AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) := streamNode

    InModuleBody { out.makeIO() }
  }
  val nIn = 1
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    streamNode := BundleBridgeToAXI4Stream(
      AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))
    ) := in
    InModuleBody { in.makeIO() }
  }
}

object AXI4StreamSimpleWidthAdapterApp extends App {
  implicit val p: Parameters = Parameters.empty
  val lazyDut = LazyModule(
    new AXI4StreamSimpleWidthAdapter(beatBytes = 2, n = 4) with AXI4StreamSimpleWidthAdapterStandaloneBlock
  )

  (new ChiselStage).execute(
    Array("--target-dir", "verilog/AXI4StreamSimpleWidthAdapter"),
    Seq(ChiselGeneratorAnnotation(() => lazyDut.module))
  )
}
