package dspblocks.utils.common

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import dsptools.numbers._
import fixedpoint._

class SimpleHeaderInserterIO[T <: Data: Real](dataType: T, maxPacketSize: Int, maxHeaderValue: Int, maxNumPackets: Int)
    extends Bundle {
  val in = Flipped(Decoupled(dataType.cloneType))
  val lastIn = Input(Bool())

  // users take care that header value, packet size and number of packets inside frame are appropriately set
  val headerValue = Input(UInt(log2Ceil(maxHeaderValue + 1).W))
  val packetSize = Input(UInt(log2Ceil(maxPacketSize + 1).W)) // without header value
  val numPacketsInFrame =
    Input(UInt(log2Ceil(maxNumPackets + 1).W)) // defined by total number of data that should be sent/packet size

  val includeHeader = Input(Bool())

  val out = Decoupled(dataType.cloneType)
  val lastOut = Output(Bool())
}

object SimpleHeaderInserterIO {
  def apply[T <: Data: Real](
    dataType:       T,
    maxPacketSize:  Int,
    maxHeaderValue: Int,
    maxNumPackets:  Int
  ): SimpleHeaderInserterIO[T] =
    new SimpleHeaderInserterIO(dataType, maxPacketSize, maxHeaderValue, maxNumPackets)
}

class SimpleHeaderInserter[T <: Data: Real: BinaryRepresentation](
  val dataType:       T,
  val maxPacketSize:  Int,
  val maxHeaderValue: Int,
  val maxNumPackets:  Int)
    extends Module {
  val io = IO(SimpleHeaderInserterIO(dataType, maxPacketSize, maxHeaderValue, maxNumPackets))
  require(dataType.getWidth % 8 == 0, "input/output data bitwidth should be multiple of 8 bits")

  val cntSamplesInPacket = RegInit(0.U(log2Ceil(maxPacketSize + 1).W))
  val cntPackets = RegInit(0.U(log2Ceil(maxNumPackets + 1).W))

  object State extends ChiselEnum {
    val sIdle, sSendHeader, sSendInputStream = Value
  }

  val state = RegInit(State.sIdle)
  val last = RegInit(false.B)
  val readyQueue = WireInit(false.B)
  val state_next = Wire(state.cloneType)
  state_next := state

  val inQueue = Module(new Queue(io.in.bits.cloneType, entries = maxNumPackets, pipe = false, flow = false))
  // pipe and flow are maybe not ok check that
  inQueue.io.enq.bits := io.in.bits
  inQueue.io.enq.valid := io.in.fire
  inQueue.io.deq.ready := readyQueue
  readyQueue := Mux(state === State.sSendInputStream && io.out.ready, true.B, false.B)
  //Mux(state_next === State.sSendHeader || state_next === State.sIdle, false.B, io.out.ready)
  val headerValueReg = RegInit(maxHeaderValue.U(log2Ceil(maxHeaderValue + 1).W))

  val resetData = Wire(io.in.bits.cloneType)
  resetData := Real[T].fromDouble(0.0)

  when(io.includeHeader) {
    io.out.bits := Mux(state === State.sSendHeader, headerValueReg.asTypeOf(dataType), inQueue.io.deq.bits)
    io.out.valid := Mux(
      (state === State.sSendHeader && io.out.ready) || (inQueue.io.deq.fire && state === State.sSendInputStream),
      true.B,
      false.B
    )
    io.in.ready := inQueue.io.enq.ready //io.out.ready
    io.lastOut := io.lastIn
    io.in.ready := io.out.ready
    io.lastOut := io.lastIn
  }.otherwise {
    io.out <> io.in
    io.lastOut := io.lastIn
  }
  when(io.out.fire) {
    when(cntSamplesInPacket === io.packetSize) {
      when(cntPackets === io.numPacketsInFrame - 1.U) {
        cntPackets := 0.U
      }.otherwise {
        cntPackets := cntPackets + 1.U
      }
      cntSamplesInPacket := 0.U
    }.otherwise {
      cntSamplesInPacket := cntSamplesInPacket + 1.U
    }
  }
  switch(state) {
    //0
    is(State.sIdle) {
      when(io.in.fire && io.out.ready) {
        state_next := State.sSendHeader
      }
      headerValueReg := io.headerValue
    }
    //1
    is(State.sSendHeader) {
      when(io.out.fire) {
        state_next := State.sSendInputStream
      }
    }
    is(State.sSendInputStream) {
      when(cntSamplesInPacket === io.packetSize && io.out.valid) {
        when(cntPackets === (io.numPacketsInFrame - 1.U)) {
          state_next := State.sIdle
        }.otherwise {
          state_next := State.sSendHeader
        }
      }
    }
  }
  state := state_next
}

object SimpleHeaderInserterApp extends App {
  val dataType = FixedPoint(16.W, 14.BP)
  val arguments = Array(
    "-X",
    "verilog",
    "--log-level",
    "info",
    "--target-dir",
    "./rtl/SimpleHeaderInserter"
  )
  //generate blackbox-es for memories
  (new ChiselStage)
    .execute(
      arguments,
      Seq(ChiselGeneratorAnnotation(() => new SimpleHeaderInserter(dataType, 32, 32, maxNumPackets = 32)))
    )
}
