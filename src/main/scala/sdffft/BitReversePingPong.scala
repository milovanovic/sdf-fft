package dspblocks.sdffft

import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import chisel3.util._
import fixedpoint._
import chisel3.experimental.{FixedPoint => _, _}
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dsptools.numbers._
import dspblocks.CounterWithReset
import scala.math._

case class BitReversePingPongParams[T <: Data](
  val proto:          DspComplex[T], // data type
  val pingPongSize:   Int, // ping pong size or fft size
  val adjustableSize: Boolean, // include register for determining current ping - pong size
  val bitReverseDir:  Boolean, // determine whether read or write address is bit reversed
  val fftShiftEnable: Boolean = false, // readout is different if fftshift is on
  val singlePortSRAM: Boolean = false) {
  requireIsChiselType(proto)
}

object BitReversePingPongParams {
  def fixed(
    dataWidth:      Int = 16,
    binPoint:       Int = 14,
    pingPongSize:   Int = 16,
    adjustableSize: Boolean = false,
    bitReverseDir:  Boolean = true,
    fftShiftEnable: Boolean = false,
    singlePortSRAM: Boolean = false
  ): BitReversePingPongParams[FixedPoint] = {
    val proto = DspComplex(FixedPoint(dataWidth.W, binPoint.BP))
    BitReversePingPongParams(
      proto = proto,
      pingPongSize = pingPongSize,
      adjustableSize = adjustableSize,
      bitReverseDir = bitReverseDir,
      fftShiftEnable = fftShiftEnable,
      singlePortSRAM = singlePortSRAM
    )
  }
}

class BitReversePingPongIO[T <: Data: Real](val params: BitReversePingPongParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.proto))
  val out = Decoupled(params.proto)
  val size = if (params.adjustableSize) Some(Input(UInt(log2Up(params.pingPongSize + 1).W))) else None

  val lastIn = Input(Bool())
  val lastOut = Output(Bool())
}

class BitReversePingPong[T <: Data: Real](val params: BitReversePingPongParams[T]) extends Module {
  val io = IO(new BitReversePingPongIO(params))

  val size = params.pingPongSize
  val memPing = SyncReadMem(size, params.proto)
  val memPong = SyncReadMem(size, params.proto)
  val totalDataWidth = params.proto.real.getWidth * 2
  memPing.suggestName("SRAM" + "_depth_" + size.toString + "_width_" + totalDataWidth.toString + s"_mem")

  val memPongReal = SyncReadMem(size, params.proto.real)
  val memPongImag = SyncReadMem(size, params.proto.imag)

  object StateFSM extends ChiselEnum {
    val sIdle, sWriteOnly, sReadWrite, sReadOnly = Value
  }
  val state = RegInit(StateFSM.sIdle)
  val state_next = WireInit(StateFSM.sIdle)
  val rstCntWritten = Wire(Bool())
  val rstCntRead = Wire(Bool())
  //val cntWritten = CounterWithReset(io.in.fire, size, rstCntWritten)
  val readyWrite = state =/= StateFSM.sReadOnly //state_next =/= StateFSM.sReadOnly
  val cntWritten = CounterWithReset(io.in.valid && readyWrite, size, rstCntWritten)
  //val cntRead = CounterWithReset(io.out.fire, size, rstCntRead)
  val validRead = state === StateFSM.sReadOnly || state === StateFSM.sReadWrite
  val cntRead = CounterWithReset(io.out.ready && validRead, size, rstCntRead)

  val cntWriteMax = cntWritten._1 === (io.size.getOrElse(size.U) - 1.U)
  val cntReadMax = cntRead._1 === (io.size.getOrElse(size.U) - 1.U)

  dontTouch(cntWritten._1)
  cntWritten._1.suggestName("writeCounter")

  rstCntWritten := cntWriteMax && (io.in.valid && readyWrite) || state_next === StateFSM.sIdle
  rstCntRead := cntReadMax && (io.out.ready && validRead) || state_next === StateFSM.sIdle

  val readPing = RegInit(true.B) // read ping or pong
  val writePing = RegInit(true.B)
  val last = RegInit(false.B)
  val doubleRead = RegInit(false.B)

  /*
  val readAddress =   if (params.bitReverseDir) MuxCase(0.U(log2Size.W), cases) else cntRead._1
  val writeAddress = if (params.bitReverseDir) cntWritten._1 else MuxCase(0.U(log2Size.W), cases)
   */
  val readAddress = Wire(cntRead._1.cloneType)
  val writeAddress = Wire(cntWritten._1.cloneType)

  val log2Size = log2Up(size) // nummber of stages in the FFT case
  val subSizes = (1 to log2Size).map(d => pow(2, d).toInt) // all possible number of stages
  val subSizesWire = subSizes.map(e => e.U)
  val bools = subSizesWire.map(e => e === io.size.getOrElse(size.U)) // create conditions
  val cases = bools.zip(1 to log2Size).map {
    case (bool, numBits) =>
      //bool -> { if (params.bitReverseDir) Reverse(cntRead._1(numBits-1, 0)) else Reverse(cntWritten._1(numBits-1, 0)) }
      bool -> Reverse(cntWritten._1(numBits - 1, 0))
  }

  readAddress := cntRead._1 //Mux(io.out.ready, cntRead._1, RegEnable(cntRead._1 , false.B, io.out.ready))
  dontTouch(readAddress)
  readAddress.suggestName("read_address")
  writeAddress := MuxCase(0.U(log2Size.W), cases)
  //dontTouch(writeAddress)
  //writeAddress.suggestName("write_address")

  /*if (params.bitReverseDir)
    readAddress := MuxCase(0.U(log2Size.W), cases)
  else
    readAddress := cntRead._1

  if (params.bitReverseDir)
    writeAddress := cntWritten._1
  else
    writeAddress := MuxCase(0.U(log2Size.W), cases)*/

  when(io.in.fire && io.lastIn) {
    last := true.B
  }
  val lastRead = cntReadMax && io.out.ready
  val lastWrite = cntWriteMax && io.in.valid
  val readOnly = lastWrite && ~lastRead
  val writeOnly = ~lastWrite && lastRead
  state_next := state // to be defined always

  when(state === StateFSM.sIdle) {
    readPing := true.B
    writePing := true.B
  }.otherwise {
    when(lastRead) {
      readPing := ~readPing
    }
    when(lastWrite) {
      writePing := ~writePing
    }
  }

  val rstProtoIQ = Wire(io.in.bits.cloneType)
  rstProtoIQ.real := Real[T].fromDouble(0.0)
  rstProtoIQ.imag := Real[T].fromDouble(0.0)
  val memPongData = WireDefault(rstProtoIQ)
  val memPingData = WireDefault(rstProtoIQ)

  val readAddressShift = if (params.fftShiftEnable) Some(Wire(readAddress.cloneType)) else None

  if (params.fftShiftEnable) {
    when(cntRead._1 >= io.size.getOrElse(params.pingPongSize.U).div2(1)) {
      readAddressShift.get := cntRead._1 - io.size.getOrElse(params.pingPongSize.U).div2(1)
    }.otherwise {
      readAddressShift.get := cntRead._1 + io.size.getOrElse(params.pingPongSize.U).div2(1)
    }
    readAddressShift.get.suggestName("readAddressShift")
    dontTouch(readAddressShift.get)
  }

  if (params.singlePortSRAM) {

    val addressPong = Mux(io.in.fire && (writePing === false.B), writeAddress, readAddressShift.getOrElse(readAddress))
    val addressPing = Mux(io.in.fire && (writePing === true.B), writeAddress, readAddressShift.getOrElse(readAddress))

    val rdWrPortPong = memPong(addressPong)
    when(io.in.fire && ~writePing) {
      rdWrPortPong := io.in.bits
    }
    memPongData := rdWrPortPong

    val rdWrPortPing = memPing(addressPing)
    when(io.in.fire && writePing) {
      rdWrPortPing := io.in.bits
    }
    memPingData := rdWrPortPing
  } else {
    when(io.in.fire && writePing) {
      //when(io.in.ready && readyWrite && writePing) {
      memPing(writeAddress) := io.in.bits
    }
    when(io.in.fire && ~writePing) {
      // when (io.in.ready && readyWrite && ~writePing) {
      memPong(writeAddress) := io.in.bits
    }
    memPingData := memPing(readAddressShift.getOrElse(readAddress))
    memPongData := memPong(readAddressShift.getOrElse(readAddress))
  }

  when(state === StateFSM.sIdle) {
    when(io.in.fire) {
      state_next := StateFSM.sWriteOnly
    }
  }
    .elsewhen(state === StateFSM.sWriteOnly) {
      when(lastWrite) {
        when(io.lastIn) { // this one should be checked
          state_next := StateFSM.sReadOnly
        }.otherwise {
          /*.elsewhen (io.out.ready) {
        state_next := StateFSM.sReadWrite
      }
      .otherwise {*/
          //state_next := StateFSM.sWriteOnly
          state_next := StateFSM.sReadWrite
        }
        //}
      }
    }
    .elsewhen(state === StateFSM.sReadWrite) {
      when(readOnly) {
        when(io.lastIn) {
          doubleRead := true.B
        }
        state_next := StateFSM.sReadOnly
      }
      when(writeOnly) {
        state_next := StateFSM.sWriteOnly
      }
      when(~writeOnly && ~readOnly && io.lastIn) {
        state_next := StateFSM.sReadOnly
      }
    }
    .elsewhen(state === StateFSM.sReadOnly) {
      when(lastRead) {
        when(~doubleRead) { // not tested
          when(last) {
            state_next := StateFSM.sIdle
            last := false.B
          }.otherwise {
            state_next := StateFSM.sReadWrite
          }
        }.otherwise {
          doubleRead := false.B
        }
      }
    }

  state := state_next
  // naming ping/pong!

//   io.out.bits  := Mux(RegNext(readPing), memPingData, memPongData)
//   io.in.ready  := state =/= StateFSM.sReadOnly
//   io.out.valid := RegNext(state === StateFSM.sReadOnly || state === StateFSM.sReadWrite)
//   io.lastOut   := RegNext(last && state_next === StateFSM.sIdle)

  val outQueue =
    Module(
      new Queue(chiselTypeOf(io.out.bits), entries = 1, pipe = false, flow = true)
    ) // prev flow on true and pipe on true
  outQueue.io.enq.bits := Mux(
    RegNext(readPing),
    memPingData,
    memPongData
  ) //Mux(RegNext(readPing), memPingData, memPongData)
  outQueue.io.enq.valid := RegNext(
    (state === StateFSM.sReadOnly || state === StateFSM.sReadWrite) && io.out.ready,
    false.B
  )
  outQueue.io.deq.ready := io.out.ready

  io.out.bits := outQueue.io.deq.bits
  io.in.ready := state =/= StateFSM.sReadOnly
  io.out.valid := outQueue.io.deq.valid

  val outQueueLast = Module(new Queue(chiselTypeOf(io.lastIn), entries = 1, pipe = true, flow = true))
  outQueueLast.io.enq.bits := RegNext(last && state_next === StateFSM.sIdle, false.B)
  outQueueLast.io.enq.valid := RegNext(
    state === StateFSM.sReadOnly || state === StateFSM.sReadWrite && io.out.ready,
    false.B
  )
  outQueueLast.io.deq.ready := io.out.ready

  // not logical, but temporary solution that works fine
  io.lastOut := state === StateFSM.sIdle && io.out.fire //outQueueLast.io.deq.bits
  dontTouch(io.lastOut)
}

object BitReversePingPongApp extends App {

  val params = BitReversePingPongParams.fixed(
    dataWidth = 24,
    binPoint = 9,
    pingPongSize = 1024,
    bitReverseDir = true,
    adjustableSize = true
  )
//   (new ChiselStage).execute(
//     Array("--target-dir", "verilog/BitReversePingPong"),
//     Seq(ChiselGeneratorAnnotation(() => new BitReversePingPong(params)))
//   )
  val arguments = Array(
    "-X",
    "verilog",
    "--repl-seq-mem",
    "-c:BitReversePingPong:-o:mem.conf",
    "--log-level",
    "info"
  )
  (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() => new BitReversePingPong(params))))
}
