// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint

import dsptools._
import dsptools.numbers._

import breeze.numerics.{cos, sin}
import scala.math.{Pi, pow}
import dspblocks.CounterWithReset
import craft.ShiftRegisterMem

class SDFChainRadix2[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  require(Seq(DITDecimType, DIFDecimType).contains(params.decimType), s"""Decimation type must either be dit or dif""")
  val io = IO(FFTIO(params))
  
  // Calculation of constants
  val numPointsDiv2     = params.numPoints / 2
  val numStages         = log2Up(params.numPoints)
  // log2(delay) of each nth stage
  val delayLog2s        = if (params.decimType == DITDecimType) (0 until numStages) else (0 until numStages).reverse
  val logn = log2Ceil(params.numPoints)
 
  // cumulative delays
  // actual delay of each nth stage
  val delays            = delayLog2s.map(d => pow(2, d).toInt)
  val cumulative_delays = delays.scanLeft(0)(_ + _)
  //TODO: try with MixedVec
  val cumulativeDelaysWires = Wire(Vec(cumulative_delays.length, UInt(cumulative_delays.last.U.getWidth.W)))
  cumulativeDelaysWires := cumulative_delays.map(e => e.U)
  
  val cumulativeDelayWire = Wire(UInt(cumulativeDelaysWires.last.getWidth.W))
  val enableVector = Wire(Vec(numStages, Bool()))
  val outputWires = Wire(Vec(numStages, io.out.bits.cloneType))
  
  val regNumStages = RegInit(numStages.U(logn.W))
  val scaleBflyReg = RegInit(VecInit(Seq.fill(numStages)(false.B)))
  val overflowReg = RegInit(VecInit(Seq.fill(numStages)(false.B)))

  val fftOrifft = RegInit(true.B)
  
  val numPoints = Wire(UInt((log2Up(params.numPoints + 1)).W)) // it can be log2Up(params.numPoints) as well
  val activeStages = Wire(Vec(numStages, Bool()))
  val cntr_wires = Wire(Vec(numStages, UInt(log2Up(params.numPoints).W)))
  
  val sIdle :: sProcess :: sFlush :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val state_next = Wire(state.cloneType)
    
  val initialOutDone = RegInit(false.B)
  val cnt = RegInit(0.U((logn + 1).W))
  
  val fft_in_bits  = Wire(io.in.bits.cloneType)
  val fft_out_bits = Wire(io.out.bits.cloneType)
  
  if (params.runTime == true)
    numPoints := (2.U << (regNumStages-1.U)) // dynamic shift
  else 
    numPoints := params.numPoints.U
  
  state_next := state
  val fireLast = io.lastIn && io.in.fire()

  val lastStageCnt = cntr_wires(regNumStages-1.U)
  val lastStageEn = enableVector(regNumStages-1.U)

  switch (state) {
    is (sIdle) {
      // init registers here
      regNumStages := io.fftSize.getOrElse(numStages.U) // number of stages
      scaleBflyReg := io.keepMSBorLSBReg.getOrElse(VecInit(Seq.fill(numStages)(false.B)))
      fftOrifft := io.fftDirReg.getOrElse(params.fftDir.B)
      when (io.in.fire()) { state_next := sProcess }
    }
    is (sProcess) {
      when (fireLast) {
        state_next := sFlush
      }
    }
    is (sFlush) {
      when (io.lastOut) { // frame is processed or window is processed
        state_next := sIdle
      }
    }
  }
  state := state_next
  
  val cntValidOut = RegInit(0.U(log2Up(params.numPoints).W))
  val lastWait = RegInit(false.B)
  val lastIndeed = RegInit(false.B)
  val initialInDone = RegInit(false.B)
  val initialInDonePrev = RegInit(false.B)
  val pktEnd = (cntValidOut === (numPoints - 1.U)) && io.out.fire()
  
  when (state_next === sIdle) {
    initialInDone := false.B
  }
  .elsewhen (cnt === (numPoints - 1.U) && io.in.fire()) {
    initialInDone := true.B
  }
  initialInDonePrev := initialInDone
  
  when (state_next === sIdle) {
    lastWait := false.B
  }
  // be carefull with those changes
  .elsewhen (fireLast && (initialInDone && initialInDonePrev) && lastStageCnt =/= (numPoints - 1.U)) { //&& cntValidOut =/= (numPoints  - 1.U)) {
    lastWait := true.B
  }

  when ((state_next === sIdle && pktEnd) || pktEnd) { //reset cntValid out
    cntValidOut := 0.U
  }
  .elsewhen (io.out.fire()) {
    cntValidOut := cntValidOut + 1.U
  }
    
  when (state_next === sIdle) {
    lastIndeed := false.B
  }
  .elsewhen (lastWait && pktEnd) {
    lastIndeed := true.B
  }

  val lastOut = if (params.numAddPipes == 0 && params.numMulPipes == 0) pktEnd && (state === sFlush) else Mux(lastWait, lastIndeed && pktEnd, pktEnd && state === sFlush)

  if (params.decimType == DIFDecimType)
    cumulativeDelayWire := cumulativeDelaysWires(numStages.U - regNumStages)
  else
    cumulativeDelayWire := 0.U
  
  // dif decimType uses specific number of last stages for calculation when run time configurability is enabled
  // dit decimType uses specific number of first stages for calculation when run time configurability is enabled
  activeStages.zipWithIndex.map { case (active, index) => {
      val ind = if (params.decimType == DIFDecimType) index.U else (numStages - index - 1).U
      active := Mux(numStages.U - regNumStages <= ind, true.B, false.B)
    }
  }
  // instatiate sdf-stages and connect relevant control and status register outputs to sdf-stage in/out signals
  val sdf_stages = delayLog2s.zip(delays).zipWithIndex.map {
     case ((delayLog2, delay), ind) => {
       val stageparams = params.copy(protoIQ = params.protoIQstages(ind))
       val useGrow = if (stageparams.expandLogic(ind) == 1) true else false
       val stage = Module(new SDFStageRadix2(stageparams, delay=delay, useGrow, params.keepMSBorLSB(ind)))
       if (params.keepMSBorLSBReg) {
         stage.io.scale.get := scaleBflyReg(ind)
       }
       stage.io.cntr := cntr_wires(ind)
       overflowReg(ind) := stage.io.overflow.getOrElse(false.B)
       stage
     }
  }

  val enableInit = io.in.fire() || (state === sFlush && io.out.ready)
  
  when (state_next === sIdle) {
    cnt := 0.U
  }
  .elsewhen (enableInit) {
    cnt := cnt +% 1.U
  }
  
  val complexMulLatency = if (params.use4Muls) {
    params.numAddPipes + params.numMulPipes
  } else {
    2 * params.numAddPipes + params.numMulPipes
  }
  val outputLatency = params.numAddPipes + complexMulLatency
  
  (0 until numStages).foldLeft(((enableInit, cnt))) {
    case ((stageEn, stageCnt), currStage) => {
      enableVector(currStage) := stageEn //collects all enable signals
      cntr_wires(currStage) := stageCnt //collects all shifted counter values
      (
        ShiftRegisterWithReset(stageEn, outputLatency, resetData = false.B, reset = state === sIdle, en = true.B),
        ShiftRegisterWithReset(stageCnt, outputLatency, resetData = 0.U, reset = state === sIdle, en = true.B)
      )
    }
  }
  

  
  when (lastStageCnt === (numPoints - 2.U) && lastStageEn) {
    initialOutDone := true.B
  }
  .elsewhen (state_next === sIdle) {
    initialOutDone := false.B
  }
  
  val validOutBeforePipes = Mux(numPoints === 2.U, RegNext(enableInit) && io.in.fire(), lastStageEn && initialOutDone)
  
  val rstProtoIQ = Wire(io.in.bits.cloneType)
  rstProtoIQ.real := Real[T].fromDouble(0.0)
  rstProtoIQ.imag := Real[T].fromDouble(0.0)
  
  when (fftOrifft === true.B) {
    fft_in_bits := io.in.bits
  }
  .otherwise {
    fft_in_bits.real := io.in.bits.imag
    fft_in_bits.imag := io.in.bits.real
  }
  
  val input_data = Mux(activeStages(0), fft_in_bits, rstProtoIQ)
  
  // connect control signals for the each sdf-stage
  sdf_stages.zipWithIndex.map { case (e, idx) => {
      val condition = if (params.decimType == DIFDecimType) (idx.U < (numStages.U - (regNumStages))) else (idx.U >= regNumStages)
      val index = if (params.decimType == DIFDecimType)  idx.U - (numStages.U - regNumStages) else idx.U
      e.io.en := Mux(condition, false.B, enableVector(index))
      e.io.cntr := (cntr_wires(index) - (cumulativeDelaysWires(idx) - cumulativeDelayWire)) (delayLog2s(idx),0)
    }
  }
  
  // connect sdf stages to chain
  sdf_stages.map(_.io).zipWithIndex.foldLeft(input_data) {
    case (stg_in, (stg_io, index)) => {
      val input = Wire(stg_io.in.cloneType)
      if (params.decimType == DIFDecimType)
        when (index.U === (numStages.U-regNumStages)) {
          input := fft_in_bits //io.in.bits
        }
        .otherwise {
          input := stg_in
        }
      else
        input := stg_in
        when (activeStages(index)) {
          stg_io.in := input
        }
        .otherwise {
          stg_io.in := rstProtoIQ
        }
        outputWires(index) := stg_io.out
        stg_io.out
    }
  }
  
  val output = if (params.decimType == DIFDecimType) outputWires.last else outputWires(regNumStages-1.U)
  val latency = (params.numAddPipes + complexMulLatency) * log2Up(params.numPoints)
  
  val outValid = ShiftRegisterWithReset(validOutBeforePipes, outputLatency, resetData = false.B, reset = state_next === sIdle, en = true.B)
  
  val outQueue =  Module(new Queue(chiselTypeOf(sdf_stages.last.io.out), entries = latency + 1, pipe = true, flow = true))
  outQueue.io.enq.bits := output
  outQueue.io.enq.valid := outValid
  outQueue.io.deq.ready := io.out.ready 
  val scalar = if (params.expandLogic.sum == 0 && !params.keepMSBorLSBReg) ConvertableTo[T].fromDouble(1.0) else ConvertableTo[T].fromDouble(1.0 / params.numPoints.toDouble)
  io.in.ready := ShiftRegister((~initialOutDone), outputLatency, resetData = false.B, en = true.B) || (io.out.ready && (state =/= sFlush))
  
  if (latency == 0) {
    when (fftOrifft === true.B) {
      fft_out_bits := output
    }
    .otherwise {
      fft_out_bits.real := output.imag * scalar
      fft_out_bits.imag := output.real * scalar
    }
    io.out.bits := fft_out_bits
    io.out.valid := outValid 
  }
  else {
    when (fftOrifft === true.B) {
      fft_out_bits := outQueue.io.deq.bits
    }
    .otherwise {
      fft_out_bits.real := outQueue.io.deq.bits.imag * scalar
      fft_out_bits.imag := outQueue.io.deq.bits.real * scalar
    }
    io.out.bits := fft_out_bits
    io.out.valid := outQueue.io.deq.valid
  }
  
  if (params.overflowReg) {
    io.overflow.get := overflowReg
  }
  
  io.lastOut := lastOut
  //io.busy := state === sFlush
  io.busy := state =/= sIdle
}

class SDFStageRadix2IO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in  = Input(params.protoIQ)
  val out = Output(params.protoIQ)
  val scale = if (params.keepMSBorLSBReg) Some(Input(Bool())) else None
  val overflow     =  if (params.overflowReg) Some(Output(Bool())) else None
 // control signals
  val cntr         = Input(UInt(log2Up(params.numPoints).W))
  val en           = Input(Bool())

  override def cloneType: this.type = SDFStageRadix2IO(params).asInstanceOf[this.type]
}
object SDFStageRadix2IO {
  def apply[T <: Data : Ring](params: FFTParams[T]): SDFStageRadix2IO[T] = new SDFStageRadix2IO(params)
}

class SDFStageRadix2[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T], val delay: Int, val useGrow: Boolean, val keepMSBOrLSB: Boolean) extends Module {
  params.checkNumPointsPow2()
  require(isPow2(delay) && delay >= 1, "delay must be a power of 2 greater than or equal to 1")

  val io = IO(SDFStageRadix2IO(params))
  val inp = Wire(params.protoIQ.cloneType)
  val out = Wire(params.protoIQ.cloneType)
  
  val gen = params.protoIQ.real
  val bpos = (gen match {
    case fp: FixedPoint => fp.binaryPoint.get
    case _ => 0
  })

  val totalDataWidth = params.protoIQ.real.getWidth*2
  val twiddles_rom = Wire(Vec(delay, params.protoTwiddle.cloneType))
  val tw = if (params.decimType == DIFDecimType) ShiftRegister(twiddles_rom(io.cntr), params.numAddPipes, en = true.B) else twiddles_rom(io.cntr)

  DspContext.withTrimType(Convergent) {
     for (k <- 0 until delay) {
        twiddles_rom(k).real := params.protoTwiddle.real.fromDoubleWithFixedWidth(cos(2*Pi*(k.toDouble)/(2*delay)))
        twiddles_rom(k).imag := params.protoTwiddle.real.fromDoubleWithFixedWidth(-sin(2*Pi*(k.toDouble)/(2*delay)))
     }
  }
  val complexMulLatency = if (params.use4Muls) {
    params.numAddPipes + params.numMulPipes
  } else {
    2 * params.numAddPipes + params.numMulPipes
  }
  // Apply twiddle factor at the input or output, depending on whether it's DIT or DIF decim type
  if (params.decimType == DITDecimType) {
    val inpOuttw = Wire(io.in.cloneType)
    val inpOut = Wire(io.in.cloneType)
    
    DspContext.alter(DspContext.current.copy(
      numAddPipes = params.numAddPipes,
      numMulPipes = params.numMulPipes
    )) {
      val mulres = DspContext.alter(DspContext.current.copy(trimType = NoTrim, overflowType = Grow, complexUse4Muls = params.use4Muls)) { io.in context_* tw }
      inpOuttw := DspContext.withTrimType(params.trimType) { mulres.trimBinary(bpos) }
    }
    inpOut := ShiftRegister(io.in, complexMulLatency, en = true.B)
    when (ShiftRegister(io.cntr > delay, complexMulLatency, en = true.B)) {
      inp := inpOuttw
    }
    .otherwise {
      inp := inpOut
    }
    io.out := out
  } else {
    inp := io.in
    val multipleSignal = ShiftRegister(io.cntr < delay.U && io.cntr =/= 0.U, params.numAddPipes, en = true.B)
    val ioOuttw = Wire(io.out.cloneType)
    val ioOut = Wire(io.out.cloneType)

    DspContext.alter(DspContext.current.copy(
      numAddPipes = params.numAddPipes,
      numMulPipes = params.numMulPipes
    )) {
       val mulres = DspContext.alter(DspContext.current.copy(trimType = NoTrim, overflowType = Grow, complexUse4Muls = params.use4Muls)) { out context_* tw }
       ioOuttw := DspContext.withTrimType(params.trimType) { mulres.trimBinary(bpos) }
    }
    ioOut := ShiftRegister(out, complexMulLatency, en = true.B)
    when (ShiftRegister(multipleSignal, complexMulLatency, en = true.B)) {
      io.out := ioOuttw
     }.otherwise { 
      io.out := ioOut
    }
  }
  val butterfly_outputs = Seq.fill(2)(Wire(params.protoIQ.cloneType))
  
  // for both DIT and DIF algorithm condition is io.cntr < delay
  val load_input = Wire(Bool())
  if (params.decimType == DIFDecimType) {
    load_input :=  io.cntr < delay.U
  }
  else {
    load_input := ShiftRegister(io.cntr < delay.U, complexMulLatency, resetData = false.B, en = true.B)
  }
  val shift_in = Wire(inp.cloneType)
  shift_in := Mux(load_input, inp, butterfly_outputs(1))
  val shift_out = Wire(shift_in.cloneType)
  
  if (params.decimType == DIFDecimType) {
    if (params.minSRAMdepth < delay) {
      shift_out := ShiftRegisterMem(shift_in, delay, en = io.en, name = "SRAM" + "_depth_" + delay.toString + "_width_" + totalDataWidth.toString + s"_mem")
    }
    else {
      shift_out := ShiftRegister(shift_in, delay, en = io.en)
    }
  }
  else {
    if (params.minSRAMdepth < delay) {
      shift_out := ShiftRegisterMem(shift_in, delay, en = ShiftRegister(io.en, complexMulLatency, true.B), name = "SRAM" + "_depth_" + delay.toString + "_width_" + totalDataWidth.toString + s"_mem")
    }
    else {
      shift_out := ShiftRegister(shift_in, delay, en = ShiftRegister(io.en, complexMulLatency, true.B))
    }
  }
  //val shift_out = if (params.decimType == DIFDecimType) ShiftRegisterMem(shift_in, delay, en = io.en) else ShiftRegisterMem(shift_in, delay, en = ShiftRegister(io.en, complexMulLatency, true.B))
  
  val butt_outputs = Butterfly[T](Seq(shift_out, inp))
  val overflow = Wire(Bool())
  
  if (useGrow) {
    butt_outputs.zip(butterfly_outputs).foreach { case(out_val, out_wire) => out_wire := out_val }
    overflow := false.B
  }
  else {
    val sumWithDiv2 = Seq(DspContext.alter(DspContext.current.copy(
                      trimType = params.trimType, binaryPointGrowth = 0))
                      { butt_outputs(0).div2(1) },
                    DspContext.alter(DspContext.current.copy(
                      trimType = params.trimType, binaryPointGrowth = 0))
                      { butt_outputs(1).div2(1)})
    
    // val tooBig = !butt_outputs(width) && butt_outputs(width - 1)
    // val tooSmall = butt_outputs(width) && !butt_outputs(width - 1)
    // asTypeOf doesnt't work in this case
    val check_overflow = params.protoIQ.real match {
      case fp: FixedPoint => Seq(butt_outputs(0).real,
                              butt_outputs(0).imag,
                              butt_outputs(1).real,
                              butt_outputs(1).imag).map(sGrow => {
                                val width = sGrow.getWidth
                                val binaryPoint = fp.binaryPoint.get
                                val tooBig = ~sGrow.isSignNegative && (BinaryRepresentation[T].shr(sGrow, width - 2) === Real[T].fromDouble(1/math.pow(2,binaryPoint)))
                                val tooSmall = sGrow.isSignNegative && (BinaryRepresentation[T].shr(sGrow, width - 2) === Real[T].fromDouble(0.0))
                                tooBig || tooSmall
                              })
      case _ => Seq(false.B, false.B, false.B, false.B)
    }
    
    overflow := check_overflow.foldLeft(false.B)(_ || _)
    val keepLSB0 = Wire(params.protoIQ)
    val keepLSB1 = Wire(params.protoIQ)
    keepLSB0 := butt_outputs(0) // or BinaryRepresentation.shl on real and imag part
    keepLSB1 := butt_outputs(1) // or BinaryRepresantation.shl on real and imag part
    butterfly_outputs(0) := Mux(io.scale.getOrElse(keepMSBOrLSB.B), sumWithDiv2(0), keepLSB0)
    butterfly_outputs(1) := Mux(io.scale.getOrElse(keepMSBOrLSB.B), sumWithDiv2(1), keepLSB1)
  }
  
  if (params.overflowReg) {
    io.overflow.get := overflow
  }
  val feedback = ShiftRegister(shift_out, params.numAddPipes, en = true.B)
  val butt_out_0 = ShiftRegister(butterfly_outputs(0), params.numAddPipes, en = true.B)
  val load_output = if (params.decimType == DIFDecimType) ShiftRegister(load_input, params.numAddPipes, resetData = false.B, en = true.B) else ShiftRegister(io.cntr < delay.U, params.numAddPipes + complexMulLatency, resetData = false.B, en = true.B)

  out := Mux(load_output, feedback, butt_out_0)
}

object SDFChainRadix2SimpleApp extends App
{
  val params = FFTParams.fixed (
    dataWidth = 16,
    twiddleWidth = 16,
    numPoints = 8 ,
    decimType = DITDecimType,
    numAddPipes = 1,
    numMulPipes = 1,
    runTime = true,
    use4Muls = false,
    trimType = RoundDown, //Floor,
    expandLogic = Array.fill(log2Up(8))(1),
    keepMSBorLSB = Array.fill(log2Up(8))(true),
    binPoint = 0
  )
  chisel3.Driver.execute(args,()=>new SDFChainRadix2(params))
}



