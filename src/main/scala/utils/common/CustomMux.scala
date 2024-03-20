package dspblocks.utils.common // rename package name to dsputils

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters

abstract class CustomMux[D, U, E, O, B <: Data](beatBytes: Int)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  val streamNode = AXI4StreamNexusNode(
    masterFn = (ms: Seq[AXI4StreamMasterPortParameters]) =>
      AXI4StreamMasterPortParameters(
        Seq(
          AXI4StreamMasterParameters(
            n = 4
          )
        )
      ),
    slaveFn = ss => { AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _)) }
  )

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.out.length == 1, "Only one output is allowed")

    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip

    val selSignal = RegInit(0.U(log2Up(ins.length + 1).W))

    for ((in, inIdx) <- ins.zipWithIndex) {
      when(selSignal === inIdx.U) {
        outs(0).bits := in.bits
        outs(0).valid := in.valid
        in.ready := outs(0).ready
      }
    }
    for (in <- ins) {
      in.ready := outs.head.ready
    }

    val fields = Seq(
      RegField(
        log2Up(ins.length + 1),
        selSignal,
        RegFieldDesc(name = "selSignal", desc = "Register used to select appropriate input")
      )
    )
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f) }): _*)
  }
}

class AXI4StreamCustomMux(address: AddressSet, val beatBytes: Int = 4)(implicit p: Parameters)
    extends CustomMux[
      AXI4MasterPortParameters,
      AXI4SlavePortParameters,
      AXI4EdgeParameters,
      AXI4EdgeParameters,
      AXI4Bundle
    ](beatBytes)
    with AXI4DspBlock
    with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}

trait AXI4StreamCustomMuxPins extends AXI4StreamCustomMux {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)

  val ioMem = mem.map { m =>
    {
      val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

      m :=
        BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
        ioMemNode

      val ioMem = InModuleBody { ioMemNode.makeIO() }
      ioMem
    }
  }

  val nOut = 1
  val outIO: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until nOut) yield {
    implicit val valName = ValName(s"outIO_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out := AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) := streamNode

    InModuleBody { out.makeIO() }
  }
  val nIn = 16
  val inIO: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    streamNode := BundleBridgeToAXI4Stream(
      AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))
    ) := in
    InModuleBody { in.makeIO() }
  }
}

object CustomMuxApp extends App {
  implicit val p: Parameters = Parameters.empty

  val lazyDut = LazyModule(new AXI4StreamCustomMux(AddressSet(0x00, 0xf), beatBytes = 4) with AXI4StreamCustomMuxPins)
  (new ChiselStage)
    .execute(Array("--target-dir", "verilog/AXI4StreamCustomMux"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
