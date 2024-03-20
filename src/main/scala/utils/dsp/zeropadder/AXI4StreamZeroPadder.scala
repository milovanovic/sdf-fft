package dspblocks.utils.dsp.zeropadder

import chisel3.util._
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.tilelink.{
  TLBundle,
  TLClientPortParameters,
  TLEdgeIn,
  TLEdgeOut,
  TLManagerPortParameters,
  TLRegisterNode
}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import org.chipsalliance.cde.config.Parameters

trait AXI4ZeroPadderStandaloneBlock extends AXI4ZeroPadderBlock[FixedPoint] {
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

abstract class ZeroPadderBlock[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](
  params:    ZeroPadderParams[T],
  beatBytes: Int)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  val streamNode = AXI4StreamIdentityNode()
  lazy val module = new LazyModuleImp(this) {
    val (in, _) = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    //  ZeroPadder module
    val zeropadder = Module(new ZeroPadderNative(params))

    val log2packetSizeStart = log2Ceil(params.packetSizeStart + 1)
    val log2packetSizeEnd = log2Ceil(params.packetSizeEnd + 1)
    val log2numberOfPackets = log2Ceil(params.numberOfPackets + 1)

    val packetSizeStart = RegInit(params.packetSizeStart.U(log2packetSizeStart.W))
    val packetSizeEnd = RegInit(params.packetSizeEnd.U(log2packetSizeEnd.W))
    val numberOfPackets = RegInit(params.numberOfPackets.U(log2numberOfPackets.W))

    val fields = Seq(
      RegField(
        log2packetSizeStart,
        packetSizeStart,
        RegFieldDesc(name = "packetSizeStart", desc = "Defines number of samples inside packet before zero padding")
      ),
      RegField(
        log2packetSizeEnd,
        packetSizeEnd,
        RegFieldDesc(name = "packetSizeEnd", desc = "Defines number of samples inside packet after zero padding")
      ),
      RegField(
        log2numberOfPackets,
        numberOfPackets,
        RegFieldDesc(name = "numberOfPackets", desc = "Defines number of packets")
      )
    )

    regmap(
      fields.zipWithIndex.map({
        case (f, i) =>
          i * beatBytes -> Seq(f)
      }): _*
    )

    // Connect inputs
    zeropadder.io.in.valid := in.valid
    zeropadder.io.packetSizeStart := packetSizeStart
    zeropadder.io.packetSizeEnd := packetSizeEnd
    zeropadder.io.numberOfPackets := numberOfPackets

    if (params.isDataComplex) {
      val inComplex = Wire(DspComplex(params.proto.cloneType))
      inComplex.real := in.bits.data(in.bits.data.getWidth - 1, in.bits.data.getWidth / 2).asTypeOf(params.proto)
      inComplex.imag := in.bits.data(in.bits.data.getWidth / 2 - 1, 0).asTypeOf(params.proto)
      zeropadder.io.in.bits := inComplex
    } else {
      zeropadder.io.in.bits := in.bits.data.asTypeOf(params.proto)
    }
    in.ready := zeropadder.io.in.ready
    zeropadder.io.lastIn := in.bits.last

    out.valid := zeropadder.io.out.valid
    zeropadder.io.out.ready := out.ready
    out.bits.data := zeropadder.io.out.bits.asUInt
    out.bits.last := zeropadder.io.lastOut
  }
}

class AXI4ZeroPadderBlock[T <: Data: Real: BinaryRepresentation](
  params:     ZeroPadderParams[T],
  address:    AddressSet,
  _beatBytes: Int = 4
)(
  implicit p: Parameters)
    extends ZeroPadderBlock[
      T,
      AXI4MasterPortParameters,
      AXI4SlavePortParameters,
      AXI4EdgeParameters,
      AXI4EdgeParameters,
      AXI4Bundle
    ](params, _beatBytes)
    with AXI4DspBlock
    with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

class TLZeroPadderBlock[T <: Data: Real: BinaryRepresentation](
  params:     ZeroPadderParams[T],
  address:    AddressSet,
  _beatBytes: Int = 4
)(
  implicit p: Parameters)
    extends ZeroPadderBlock[T, TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle](
      params,
      _beatBytes
    )
    with TLDspBlock
    with TLHasCSR {
  val devname = "TLZeroPadderBlock"
  val devcompat = Seq("zeropadder", "radardsp")
  val device = new SimpleDevice(devname, devcompat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping)
    }
  }
  // make diplomatic TL node for regmap
  override val mem = Some(TLRegisterNode(address = Seq(address), device = device, beatBytes = _beatBytes))
}

object ZeroPadderDspBlockAXI4 extends App {
  val params: ZeroPadderParams[FixedPoint] = ZeroPadderParams(
    proto = FixedPoint(16.W, 14.BP),
    packetSizeStart = 16,
    packetSizeEnd = 32,
    queueDepth = 64,
    numberOfPackets = 3,
    useQueue = true,
    isDataComplex = true,
    useBlockRam = false
  )
  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty
  val zeropadderModule = LazyModule(
    new AXI4ZeroPadderBlock(params, AddressSet(baseAddress + 0x100, 0xff), _beatBytes = 2)
      with AXI4ZeroPadderStandaloneBlock
  )

  //chisel3.Driver.execute(args, ()=> zeropadderModule.module)
  (new ChiselStage).execute(
    Array("--target-dir", "verilog/AXI4ZeroPadderBlock"),
    Seq(ChiselGeneratorAnnotation(() => zeropadderModule.module))
  )
}
