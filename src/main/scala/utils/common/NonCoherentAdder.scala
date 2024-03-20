package dspblocks.utils.common

import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dsptools._
import dsptools.numbers._

import fixedpoint._
import chisel3.internal.requireIsChiselType

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}

// NonCoherentAdder parameters
case class NonCoherentAdderParams[T <: Data: Real: BinaryRepresentation](
  proto:       T, // Input/output data type
  numAddPipes: Int = 1,
  trimType:    TrimType = NoTrim, //Convergent, //NoTrim
  verbose:     Boolean = true) {
  requireIsChiselType(proto, s"($proto) must be chisel type")
  require(numAddPipes >= 0, "Number of pipe registers must be greater or equal to zero!")
}

class NonCoherentAdder[T <: Data: Real: BinaryRepresentation](params: NonCoherentAdderParams[T])
    extends LazyModule()(Parameters.empty) {
  val streamNode = AXI4StreamNexusNode(
    masterFn =
      (ms: Seq[AXI4StreamMasterPortParameters]) => AXI4StreamMasterPortParameters(ms.map(_.masters).reduce(_ ++ _)),
    slaveFn = ss => { AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _)) }
  )

  lazy val module = new LazyModuleImp(this) {
    require(streamNode.out.length == 1, "Only one output to non-coherent adder allowed")
    require(streamNode.in.length >= 1, "Module must have one or more inputs!")
    val (ins, _) = streamNode.in.unzip
    val (out, _) = streamNode.out.unzip

    if (params.verbose)
      println(
        s"---------------------------\n" +
          s"ADDER: Number of inputs: ${ins.length}\n" +
          s"---------------------------\n"
      )

    val latency = params.numAddPipes * log2Ceil(ins.length) // latency

    def sumRecursive[T <: Data: Real: BinaryRepresentation](in: Seq[T]): T = {
      if (in.length == 1) in.head
      else
        sumRecursive(
          in.grouped(2)
            .map {
              case data: Seq[T] => {
                DspContext.alter(
                  DspContext.current
                    .copy(numAddPipes = params.numAddPipes, trimType = params.trimType, overflowType = Grow)
                ) {
                  if (data.length > 1) data.head.context_+(data.last) else data.head.context_+(0.U.asTypeOf(data.head))
                }
              }
            }
            .toSeq
        )
    }

    // Perform trimming (if necessary) for FixedPoint
    val sumOfInputs = (params.proto match {
      case fp: FixedPoint => {
        if (fp.binaryPoint.get >= log2Ceil(ins.length)) {
          DspContext.withTrimType(params.trimType) {
            sumRecursive(ins.map { case (in) => in.bits.data.asTypeOf(params.proto) })
              .trimBinary(fp.binaryPoint.get - log2Ceil(ins.length))
          }
        } else {
          if (params.verbose)
            println(
              s"Trimming was not possible, input BP width is: ${fp.binaryPoint.get}, data should be trimmed by ${log2Ceil(ins.length)} bits."
            )
          sumRecursive(ins.map { case (in) => in.bits.data.asTypeOf(params.proto) })
        }
      }
      case _ => sumRecursive(ins.map { case (in) => in.bits.data.asTypeOf(params.proto) })
    })

    // Check if there is pipe registers for addition
    if (params.numAddPipes > 0) {
      val dataQueue = Module(new Queue(out.head.bits.cloneType, latency + 1, pipe = latency == 1))
      val queueCounter = RegInit(0.U(log2Ceil(latency + 1).W))
      queueCounter := queueCounter +& ins.map { case (in) => in.fire() }.reduce((a, b) => a && b) -& out.head.fire()

      out.head <> dataQueue.io.deq

      dataQueue.io.enq.bits.data := BinaryRepresentation[T]
        .shr(sumOfInputs, sumOfInputs.getWidth - params.proto.getWidth)
        .asTypeOf(out.head.bits.data)
      dataQueue.io.enq.bits.last := ShiftRegister(
        ins.map { case (in) => in.bits.last }.reduce((a, b) => a && b),
        latency,
        false.B,
        //en = true.B
        true.B
      )
      dataQueue.io.enq.valid := ShiftRegister(
        ins.map { case (in) => in.fire() }.reduce((a, b) => a && b),
        latency,
        false.B,
        //en = true.B
        true.B
      )
      for ((in, inIdx) <- ins.zipWithIndex) {
        in.ready := dataQueue.io.enq.ready && (queueCounter < latency.U)
      }
      dataQueue.io.enq.bits.id := DontCare
      dataQueue.io.enq.bits.strb := DontCare
      dataQueue.io.enq.bits.keep := DontCare
      dataQueue.io.enq.bits.dest := DontCare
      dataQueue.io.enq.bits.user := DontCare

      // we control in.ready such that the queue can't fill up!
      assert(!dataQueue.io.enq.valid || dataQueue.io.enq.ready)
    } else {
      out.head.bits.data := BinaryRepresentation[T]
        .shr(sumOfInputs, sumOfInputs.getWidth - params.proto.getWidth)
        .asTypeOf(out.head.bits.data)
      out.head.bits.last := ins.map { case (in) => in.bits.last }.reduce((a, b) => a && b)
      out.head.valid := ins.map { case (in) => in.valid }.reduce((a, b) => a && b)
      for ((in, inIdx) <- ins.zipWithIndex) {
        in.ready := out.head.ready
      }
    }

    // input last and valid must be the same!
    assert(
      ins.map { case (in) => ins.head.bits.last === in.bits.last }.reduce((a, b) => a & b),
      "All input last signals must be the same!"
    )
    assert(
      ins.map { case (in) => ins.head.valid === in.valid }.reduce((a, b) => a & b),
      "All input valid signals must be the same!"
    )
  }
}

object NonCoherentAdder {
  def apply[T <: Data: Real: BinaryRepresentation](params: NonCoherentAdderParams[T]) = {
    val adder = LazyModule(new NonCoherentAdder(params))
    adder.streamNode
  }
}

trait NonCoherentAdderInOuts[T <: Data] {
  self: NonCoherentAdder[T] =>
  def nIn:       Int = 4
  def nOut:      Int = 1
  def beatBytes: Int = 2

  val inIO: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until nIn) yield {
    implicit val valName = ValName(s"inIO_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    streamNode := BundleBridgeToAXI4Stream(
      AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = beatBytes))
    ) := in
    InModuleBody { in.makeIO() }
  }
  val outIO: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until nOut) yield {
    implicit val valName = ValName(s"outIO_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out := AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) := streamNode
    InModuleBody { out.makeIO() }
  }
}

object NonCoherentAdderApp extends App {
  val params: NonCoherentAdderParams[FixedPoint] = NonCoherentAdderParams(
    proto = FixedPoint(16.W, 14.BP)
  )
  implicit val p: Parameters = Parameters.empty

  val lazyDut = LazyModule(new NonCoherentAdder(params) with NonCoherentAdderInOuts[FixedPoint] {
    override def nIn = 4
    override def nOut = 1
    override def beatBytes = 2
  })
  (new ChiselStage)
    .execute(Array("--target-dir", "verilog/NonCoherentAdder"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
