package dspblocks.utils.common

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import dsptools.numbers._
import fixedpoint._

class PasstroughOrIgnoreStreamIO[T <: Data: Real](dataType: T, maxColumns: Int, maxRows: Int) extends Bundle {
  val in = Flipped(Decoupled(dataType.cloneType))
  val lastIn = Input(Bool())

  val numRows = Input(UInt(log2Ceil(maxRows).W))
  val enStreamCut = Input(Bool())
  val out = Decoupled(dataType.cloneType)
  val lastOut = Output(Bool())
}

object PasstroughOrIgnoreStreamIO {
  def apply[T <: Data: Real](dataType: T, maxColumns: Int, maxRows: Int): PasstroughOrIgnoreStreamIO[T] =
    new PasstroughOrIgnoreStreamIO(dataType, maxColumns, maxRows)
}

class PasstroughOrIgnoreStream[T <: Data: Real: BinaryRepresentation](
  val dataType:   T,
  val maxColumns: Int,
  val maxRows:    Int)
    extends Module {
  val io = IO(PasstroughOrIgnoreStreamIO(dataType, maxColumns, maxRows))

  val cntColumns = RegInit(0.U(log2Ceil(maxColumns).W))
  val cntRows = RegInit(0.U(log2Ceil(maxRows).W))

  object State extends ChiselEnum {
    val sIdle, sSendInStream, sWaitData = Value
  }
  val numRowsReg = RegInit(maxRows.U(log2Ceil(maxRows + 1).W))

  val state = RegInit(State.sIdle)
  val last = RegInit(false.B)
  val readyQueue = WireInit(false.B)
  val state_next = Wire(state.cloneType)
  state_next := state

  /*val inQueue = Module(new Queue(io.in.bits.cloneType, entries = 2, pipe = true, flow = true))
  inQueue.io.enq.bits := io.in.bits
  inQueue.io.enq.valid := io.in.valid
  inQueue.io.deq.ready := readyQueue
  readyQueue := Mux(state === State.sSendNumRows, false.B, io.out.ready)

  io.out.bits := Mux(state_next =/= State.sSendNumRows, inQueue.io.deq.bits, numRowsReg.asTypeOf(dataType))
  io.out.valid := Mux(state_next =/= State.sSendNumRows || inQueue.io.deq.valid, true.B, false.B)
  io.in.ready := io.out.ready*/

  when(io.in.fire) {
    when(cntColumns === maxColumns.U - 1.U) {
      when(cntRows === maxRows.U - 1.U) {
        cntRows := 0.U
      }.otherwise {
        cntRows := cntRows + 1.U
      }
      cntColumns := 0.U
    }.otherwise {
      cntColumns := cntColumns + 1.U
    }
  }
  switch(state) {
    //0
    is(State.sIdle) {
      when(io.in.fire) {
        state_next := State.sSendInStream
      }
      numRowsReg := io.numRows
    }
    //1
    is(State.sSendInStream) {
      when(io.enStreamCut) {
        when(cntRows === (io.numRows - 1) && io.in.fire && cntColumns === (maxColumns - 1).U) {
          when(io.numRows === maxRows.U) {
            state_next := State.sIdle
          }.otherwise {
            state_next := State.sWaitData
          }
        }
      }.otherwise {
        when(cntRows === (maxRows - 1.U) && io.in.fire && cntColumns === (maxColumns - 1).U) {
          state_next := State.sIdle
        }
      }
    }
    is(State.sWaitData) {
      when(cntRows === (maxRows.U - 1.U) && cntColumns === (maxColumns - 1).U && io.in.fire) {
        state_next := State.sIdle
      }
    }
  }

  val resetData = Wire(io.in.bits.cloneType)
  resetData := Real[T].fromDouble(0.0)

  io.out.bits := Mux(state =/= State.sWaitData && state_next =/= State.sIdle, io.in.bits, resetData)
  io.out.valid := Mux(state =/= State.sWaitData && state_next =/= State.sIdle, io.in.valid, false.B)
  io.in.ready := io.out.ready
  io.lastOut := io.lastIn // check is this important!
  state := state_next
}

object PasstroughOrIgnoreStreamApp extends App {
  val dataType = FixedPoint(16.W, 14.BP)

  val arguments = Array(
    "-X",
    "verilog",
    "--log-level",
    "info",
    "--target-dir",
    "./rtl/PasstroughAndBlockStream"
  )

  //generate blackbox-es for memories
  (new ChiselStage)
    .execute(arguments, Seq(ChiselGeneratorAnnotation(() => new PasstroughOrIgnoreStream(dataType, 32, 32))))
}
