package dspblocks.utils.dsp.zeropadder

import chisel3.util._
import dsptools.numbers._
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import dspblocks.utils.common.QueueWithSyncReadMem

class ZeroPadderNative[T <: Data: Real](val params: ZeroPadderParams[T]) extends Module {

  params.checkNumberOfSamples()
  params.checkQueueDepth()

  val io = IO(ZeroPadderIO(params))

  val packetSizeStartReg = RegNext(io.packetSizeStart, params.packetSizeStart.U)
  val packetSizeEndReg = RegNext(io.packetSizeEnd, params.packetSizeEnd.U)
  val numberOfPackets = RegNext(io.numberOfPackets, params.numberOfPackets.U)

  val log2packetSizeStart = log2Ceil(params.packetSizeStart)
  val log2packetSizeEnd = log2Ceil(params.packetSizeEnd)
  val log2numberOfPackets = log2Ceil(params.numberOfPackets)

  val cntOutData = RegInit(0.U(log2packetSizeEnd.W))
  val cntPackets = RegInit(0.U(log2numberOfPackets.W))
  val zeroPaddFlag = Wire(Bool())
  val outFire = io.out.valid && io.out.ready

  when(outFire) {
    cntOutData := cntOutData + 1.U
  }

  when(cntOutData === (packetSizeEndReg - 1.U) && outFire) {
    cntOutData := 0.U
    when(cntPackets === (numberOfPackets - 1.U)) {
      io.lastOut := true.B
      cntPackets := 0.U
    }.otherwise {
      cntPackets := cntPackets + 1.U
      io.lastOut := false.B
    }
  }.otherwise {
    io.lastOut := false.B //&& genLast
  }

  when(cntOutData < packetSizeStartReg) {
    zeroPaddFlag := false.B
  }.otherwise {
    //zeroPaddFlag := true.B && ~io.lastOut
    zeroPaddFlag := true.B
  }

  val rstProtoComplex = Wire(DspComplex(params.proto.cloneType))
  rstProtoComplex.real := Real[T].fromDouble(0.0)
  rstProtoComplex.imag := Real[T].fromDouble(0.0)

  val rstProto = Wire(params.proto.cloneType)
  rstProto := Real[T].fromDouble(0.0)

  //rstProto.real := Real[T].fromDouble(0.0)
  //rstProto.imag := Real[T].fromDouble(0.0)

  if (params.useQueue) {
    val dataQueue = Module(
      new QueueWithSyncReadMem(
        io.in.bits.cloneType,
        entries = params.queueDepth,
        flow = !params.useBlockRam,
        useSyncReadMem = params.useBlockRam,
        useBlockRam = params.useBlockRam
      )
    )
    val inValidReg = RegNext(io.in.valid, init = false.B)
    val inDataReg =
      if (params.isDataComplex) RegNext(io.in.bits, init = rstProtoComplex) else RegNext(io.in.bits, init = rstProto)

    dataQueue.io.enq.bits := inDataReg
    dataQueue.io.enq.valid := inValidReg
    dataQueue.io.deq.ready := ~zeroPaddFlag && io.out.ready
    when(zeroPaddFlag) { // if out_ready is not active then cntOutData will not count at all
      io.out.valid := true.B && io.out.ready
      if (params.isDataComplex) {
        io.out.bits := rstProtoComplex
      } else {
        io.out.bits := rstProto
      }
      io.in.ready := dataQueue.io.enq.ready
    }.otherwise {
      io.out.valid := dataQueue.io.deq.valid
      io.out.bits := dataQueue.io.deq.bits
      io.in.ready := dataQueue.io.enq.ready
    }
  } else {
    when(zeroPaddFlag) {
      io.out.valid := true.B
      if (params.isDataComplex) {
        io.out.bits := rstProtoComplex
      } else {
        io.out.bits := rstProto
      }
      io.in.ready := false.B
    }.otherwise {
      io.out.valid := io.in.valid
      io.out.bits := io.in.bits
      io.in.ready := io.out.ready
    }
  }
}

object ZeroPadderApp extends App {
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

  (new ChiselStage).execute(
    Array("--target-dir", "verilog/ZeroPadder"),
    Seq(ChiselGeneratorAnnotation(() => new ZeroPadderNative(params)))
  )
}
