package dspblocks.utils.common

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import dspblocks.{AXI4DspBlock, AXI4HasCSR, DspBlock, HasCSR}
import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters
import fixedpoint._

// define parameters
case class PassOrIgnoreWithHeaderParams[T <: Data: Real](
  proto:          T,
  maxHeaderValue: Int = 256,
  maxPacketSize:  Int = 32,
  maxNumPackets:  Int = 256,
  maxColumns:     Int = 32) {}

class PassOrIgnoreStreamWithHeaderIO[T <: Data: Real](params: PassOrIgnoreWithHeaderParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.proto.cloneType))
  val lastIn = Input(Bool())

  val enStreamCut = Input(Bool())

  val headerValue = Input(UInt(log2Ceil(params.maxHeaderValue + 1).W))
  val packetSize = Input(UInt(log2Ceil(params.maxPacketSize + 1).W)) // without header value
  val numPacketsInFrame =
    Input(UInt(log2Ceil(params.maxNumPackets + 1).W)) // defined by total number of data that should be sent/packet size

  val includeHeader = Input(Bool())

  val out = Decoupled(params.proto.cloneType)
  val lastOut = Output(Bool())
}

object PassOrIgnoreStreamWithHeaderIO {
  def apply[T <: Data: Real](params: PassOrIgnoreWithHeaderParams[T]): PassOrIgnoreStreamWithHeaderIO[T] =
    new PassOrIgnoreStreamWithHeaderIO(params)
}
class PassOrIgnoreStreamWithHeader[T <: Data: Real: BinaryRepresentation](
  val params: PassOrIgnoreWithHeaderParams[T])
    extends Module {
  val io = IO(PassOrIgnoreStreamWithHeaderIO(params))

  val passOrIgnoreStreamModule = Module(
    new PasstroughOrIgnoreStream(params.proto, params.maxColumns, params.maxHeaderValue)
  )
  val insertHeaderModule = Module(
    new SimpleHeaderInserter(params.proto, params.maxPacketSize, params.maxHeaderValue, params.maxNumPackets)
  )
  // init registers passOrIgnoreStreamModule
  passOrIgnoreStreamModule.io.enStreamCut := io.enStreamCut
  passOrIgnoreStreamModule.io.numRows := io.headerValue

  // init registers insertHeaderValue
  insertHeaderModule.io.headerValue := io.headerValue
  insertHeaderModule.io.packetSize := io.packetSize
  insertHeaderModule.io.numPacketsInFrame := io.numPacketsInFrame
  insertHeaderModule.io.includeHeader := io.includeHeader

  // lastIn and lastOut
  passOrIgnoreStreamModule.io.lastIn := io.lastIn
  insertHeaderModule.io.lastIn := passOrIgnoreStreamModule.io.lastOut
  io.lastOut := insertHeaderModule.io.lastOut

  passOrIgnoreStreamModule.io.in <> io.in
  insertHeaderModule.io.in <> passOrIgnoreStreamModule.io.out
  io.out <> insertHeaderModule.io.out
}

trait AXI4PassOrIgnoreStreamWithHeaderStandaloneBlock extends AXI4PassOrIgnoreStreamWithHeader[FixedPoint] {
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

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

abstract class PassOrIgnoreStreamWithHeaderBlock[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](
  params:    PassOrIgnoreWithHeaderParams[T],
  beatBytes: Int)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  val streamNode = AXI4StreamIdentityNode()
  // same input/output stream
  lazy val module = new LazyModuleImp(this) {
    val (in, _) = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    val passOrIgnoreWithHeader = Module(new PassOrIgnoreStreamWithHeader(params))
    var commonFields = Seq[RegField]()

    // define here all registers
    val enStreamCut = RegInit(false.B)
    val headerValue = RegInit(params.maxHeaderValue.U(log2Ceil(params.maxHeaderValue + 1).W))
    val packetSize = RegInit(params.maxPacketSize.U(log2Ceil(params.maxPacketSize + 1).W))
    val numPacketsInFrame = RegInit(params.maxNumPackets.U(log2Ceil(params.maxNumPackets + 1).W))
    val includeHeader = RegInit(false.B)

    commonFields = commonFields :+ RegField(
      log2Ceil(params.maxHeaderValue),
      headerValue,
      RegFieldDesc(name = "headerValue", desc = "header value that is included inside stream")
    )

    passOrIgnoreWithHeader.io.includeHeader := includeHeader
    passOrIgnoreWithHeader.io.enStreamCut := enStreamCut
    passOrIgnoreWithHeader.io.numPacketsInFrame := numPacketsInFrame
    passOrIgnoreWithHeader.io.packetSize := packetSize
    passOrIgnoreWithHeader.io.packetSize := headerValue

    passOrIgnoreWithHeader.io.in.bits := in.bits.data.asTypeOf(params.proto)
    passOrIgnoreWithHeader.io.in.valid := in.valid
    in.ready := passOrIgnoreWithHeader.io.in.valid
    passOrIgnoreWithHeader.io.lastIn := in.bits.last

    out.bits.data := passOrIgnoreWithHeader.io.out.bits.asUInt
    out.valid := passOrIgnoreWithHeader.io.out.valid
    passOrIgnoreWithHeader.io.out.ready := out.ready
    passOrIgnoreWithHeader.io.headerValue := headerValue

    commonFields = commonFields :+ RegField(
      log2Ceil(params.maxNumPackets),
      numPacketsInFrame,
      RegFieldDesc(name = "maxNumPackets", desc = "maximum number of packets")
    )

    commonFields = commonFields :+ RegField(
      log2Ceil(params.maxPacketSize),
      packetSize,
      RegFieldDesc(name = "packetSize", desc = "packet size")
    )

    commonFields = commonFields :+ RegField(
      1,
      enStreamCut,
      RegFieldDesc(name = "enStreamCut", desc = "enable stream ignore")
    )

    commonFields = commonFields :+ RegField(
      1,
      includeHeader,
      RegFieldDesc(name = "includeHeader", desc = "enable header insertion")
    )

    //define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(
      commonFields.zipWithIndex.map({
        case (f, i) =>
          i * beatBytes -> Seq(f)
      }): _*
    )
  }
}

class AXI4PassOrIgnoreStreamWithHeader[T <: Data: Real: BinaryRepresentation](
  params:     PassOrIgnoreWithHeaderParams[T],
  address:    AddressSet,
  _beatBytes: Int = 4
)(
  implicit p: Parameters)
    extends PassOrIgnoreStreamWithHeaderBlock[
      T,
      AXI4MasterPortParameters,
      AXI4SlavePortParameters,
      AXI4EdgeParameters,
      AXI4EdgeParameters,
      AXI4Bundle
    ](params, _beatBytes)
    with AXI4DspBlock
    with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = _beatBytes))
}

object AXI4PassOrIgnoreStreamWithHeaderBlockApp extends App {
  val params = PassOrIgnoreWithHeaderParams(proto = FixedPoint(16.W, 8.BP))
  val baseAddress = 0x500
  implicit val p: Parameters = Parameters.empty

  val lazyDut = LazyModule(
    new AXI4PassOrIgnoreStreamWithHeader(params, AddressSet(baseAddress + 0x100, 0xff), _beatBytes = 4)
      with AXI4PassOrIgnoreStreamWithHeaderStandaloneBlock {}
  )
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

object PassOrIgnoreStreamWithHeaderApp extends App {
  val params = PassOrIgnoreWithHeaderParams(proto = UInt(8.W))
  val arguments = Array(
    "-X",
    "verilog",
    "--log-level",
    "info",
    "--target-dir",
    "./rtl/PasstroughAndBlockStream"
  )
  (new ChiselStage)
    .execute(arguments, Seq(ChiselGeneratorAnnotation(() => new PassOrIgnoreStreamWithHeader(params))))
}
