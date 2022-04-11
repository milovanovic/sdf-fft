// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint

import dsptools._
import dsptools.numbers._

import breeze.numerics.{cos, sin}
import scala.math.{Pi, pow}
import craft.ShiftRegisterMem
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}


class SDFChainRadix22[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  require(Seq(DITDecimType, DIFDecimType).contains(params.decimType), s"""Decimation type must either be dit or dif""")
  val io = IO(FFTIO(params))
  
  // Calculation of constants
  val numPointsDiv2     = params.numPoints / 2
  val numStages         = log2Up(params.numPoints)
  // log2(delay) of each nth stage
  val delayLog2s        = if (params.decimType == DITDecimType) (0 until numStages) else (0 until numStages).reverse
  val logn = log2Ceil(params.numPoints)
  val retiming = if (params.runTime) 1 else 0
  
  // cumulative delays
  // actual delay of each nth stage
  val delays            = delayLog2s.map(d => pow(2, d).toInt)
  val cumulative_delays = delays.scanLeft(0)(_ + _)
  //TODO: try with MixedVec
  val cumulativeDelaysWires = Wire(Vec(cumulative_delays.length, UInt(cumulative_delays.last.U.getWidth.W)))
  cumulativeDelaysWires := cumulative_delays.map(e => e.U)
  
  val cumulativeDelayWire = Wire(UInt(cumulativeDelaysWires.last.getWidth.W))
  val enableVector = Wire(Vec(numStages, Bool()))

  //val outputWires = Wire(Vec(numStages, io.out.bits.cloneType))
  val outputWires = if (params.trimEnable) Wire(Vec(numStages, params.protoIQstages.last.cloneType)) else Wire(Vec(numStages, io.out.bits.cloneType))

  val regNumStages = RegInit(numStages.U((logn).W))
  val scaleBflyReg = RegInit(VecInit(Seq.fill(numStages)(false.B)))
  val overflowReg = RegInit(VecInit(Seq.fill(numStages)(false.B)))
  val fftOrifft = RegInit(true.B)
  
  val numPoints = Wire(UInt((log2Up(params.numPoints + 1)).W))
  val activeStages = Wire(Vec(numStages, Bool()))
  val cntr_wires = Wire(Vec(numStages, UInt(log2Up(params.numPoints).W)))
  val twiddles = Wire(Vec(numStages, params.protoTwiddle))
  
  val sIdle :: sProcess :: sFlush :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val state_next = Wire(state.cloneType)
    
  val initialOutDone = RegInit(false.B)
  val cnt = RegInit(0.U((logn + 1).W))
  
  val fft_in_bits  = Wire(io.in.bits.cloneType)
  val fft_out_bits = Wire(io.out.bits.cloneType)
  
  val complexMulLatency = if (params.use4Muls) {
    params.numAddPipes + params.numMulPipes
  } else {
    2 * params.numAddPipes + params.numMulPipes
  }
  val outputLatency = params.numAddPipes + complexMulLatency
  
  if (params.runTime == true)
    numPoints := (2.U << (regNumStages-1.U))
  else
    numPoints := params.numPoints.U

  // simple state machine
  state_next := state
  val fireLast = io.lastIn && io.in.fire()

  val lastStageCnt = cntr_wires(regNumStages-1.U)
  val lastStageEn = enableVector(regNumStages-1.U)

  switch (state) {
    is (sIdle) {
      // register initialization
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
      when (io.lastOut) { //frame is processed
        state_next := sIdle
      }
    }
  }
  state := state_next
  // logic for last out signal generation
  
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
  // lastStageCnt =/= (numPoints - 1.U) is added
  .elsewhen (fireLast && (initialInDone && initialInDonePrev) && lastStageCnt =/= (numPoints - 1.U)) {
    lastWait := true.B
  }

  when ((state_next === sIdle && pktEnd) || pktEnd) {
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
  
  // dif decimType uses certain number of last stages for calculation when run time configurability is enabled
  // dit decimType uses certain number of first stages for calculation when run time configurability is enabled
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
      val stage = Module(new SDFStageRadix22(stageparams, delay=delay, useGrow, params.keepMSBorLSB(ind)))
      if (params.keepMSBorLSBReg) {
        stage.io.scale.get := scaleBflyReg(ind)
      }
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
  
  (0 until numStages).foldLeft(((enableInit, cnt))) {
    case ((stageEn, stageCnt), currStage) => {
      enableVector(currStage) := stageEn  // collects all shifted enable values
      cntr_wires(currStage) := stageCnt  //collects all shifted counter values
      (
        ShiftRegisterWithReset(stageEn, outputLatency, resetData = false.B, reset = state_next === sIdle, en = true.B),
        ShiftRegisterWithReset(stageCnt, outputLatency, resetData = 0.U, reset = state_next === sIdle, en = true.B)
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
  val rstProtoTwiddle = Wire(params.protoTwiddle.cloneType)
  rstProtoTwiddle.real := Real[T].fromDouble(0.0)
  rstProtoTwiddle.imag := Real[T].fromDouble(0.0)
  
  // if ifft is used swap real and imaginary part
  when (fftOrifft === true.B) {
    fft_in_bits := io.in.bits
  }
  .otherwise {
    fft_in_bits.real := io.in.bits.imag
    fft_in_bits.imag := io.in.bits.real
  }
  
  val input_data = Mux(activeStages(0), fft_in_bits, rstProtoIQ)
  
  sdf_stages.zipWithIndex.map { case (e, idx) => {
     val condition = if (params.decimType == DIFDecimType) (idx.U < (numStages.U - (regNumStages))) else (idx.U >= regNumStages)
     val index = if (params.decimType == DIFDecimType)  idx.U - (numStages.U - regNumStages) else idx.U
     e.io.en := Mux(condition, false.B, enableVector(index))
     e.io.cntr := (cntr_wires(index) - (cumulativeDelaysWires(idx) - cumulativeDelayWire)) (delayLog2s(idx),0)
    }
  }
  // multiplication logic which follows radix 2^2 sdf-fft architecture
  // considers run time configurability also
  sdf_stages.map(_.io).zipWithIndex.foldLeft(input_data) {
    case (stg_in, (stg_io, index)) => {
      val input = Wire(stg_io.in.cloneType)
      val doNothing = if (params.decimType == DIFDecimType) (numStages-1) == index else index == 0
      val stageN = if (params.decimType == DIFDecimType) (2 << (numStages - index)) else (2 << (index+1))
      val isMulTw = if (params.decimType == DIFDecimType) ((index + 1) % 2 == 0 && !doNothing) else (((index + 1) % 2 == 0) && (numStages % 2 == 1)) || (((index + 1) % 2 == 1) && (numStages % 2 == 0))
      val gen = params.protoIQstages(index).real.cloneType
      val bpos = (gen match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _ => 0
      })
      if (params.decimType == DIFDecimType)
        when (index.U === (numStages.U-regNumStages)) {
          input := fft_in_bits
        }
        .otherwise {
          input := stg_in
        }
      else
        input := stg_in
        
      if (params.decimType == DIFDecimType) {
        when (activeStages(index)) {
          stg_io.in := input
        }
       .otherwise {
          stg_io.in := rstProtoIQ
        }
      }
      // sdf stage with multiplier
      if (isMulTw && !doNothing) {
        val twiddleIdxs =
          Seq.fill(stageN / 4)(0) ++
          Seq.tabulate(stageN / 4)(i => i * 2) ++
          Seq.tabulate(stageN / 4)(i => i) ++
          Seq.tabulate(stageN / 4)(i => i * 3) // ++
        val uniqueTwiddleIdxs = twiddleIdxs.distinct.sorted
        
        val uniqueTwiddleTable = VecInit(uniqueTwiddleIdxs.map(t =>
          DspContext.withTrimType(Convergent) {
            DspComplex.wire(
              real = ConvertableTo[T].fromDoubleWithFixedWidth( math.cos(2 * math.Pi * t / stageN), params.protoTwiddle.real),
              imag = ConvertableTo[T].fromDoubleWithFixedWidth(-math.sin(2 * math.Pi * t / stageN), params.protoTwiddle.imag)
            )}))
        
        val twiddleIdxTable = VecInit(twiddleIdxs.map(i => {
          ((uniqueTwiddleIdxs.indexOf(i))).U
        }))
        
        if (params.decimType == DIFDecimType) {
          val currIndex = index.U - (numStages.U - regNumStages)
          val address = cntr_wires(currIndex) - (cumulativeDelaysWires(index + 1) - cumulativeDelayWire)
          address.suggestName("addressOrig")

          val addressRetime = if (params.runTime) RegEnable(cntr_wires(currIndex) + 1.U - (cumulativeDelaysWires(index + 1) - cumulativeDelayWire), enableVector(currIndex)) else cntr_wires(currIndex) - (cumulativeDelaysWires(index + 1) - cumulativeDelayWire)
          
          twiddles(index) := ShiftRegister(uniqueTwiddleTable(twiddleIdxTable(addressRetime)), params.numAddPipes, en = true.B)
        }
        else {
          val address = (cntr_wires(index.U) - (cumulativeDelaysWires(index) - cumulativeDelayWire))
          address.suggestName("addressOrig")
          //if (params.runTime == true) Mux(state === sIdle, 0.U, RegEnable(cntr_wires(index.U) + 1.U - delay, enableVector(index.U))) else (cntr_wires(index.U) - delay)
          twiddles(index) := uniqueTwiddleTable(twiddleIdxTable(address))
        }
        val ioOuttw = Wire(stg_in.cloneType)
        DspContext.alter(DspContext.current.copy(
          numAddPipes = params.numAddPipes,
          numMulPipes = params.numMulPipes
        )) {

//           ORIGINAL CONFIGURATION
//           val mulres = if (params.decimType == DIFDecimType) DspContext.alter(DspContext.current.copy(trimType = NoTrim, overflowType = Grow, complexUse4Muls = params.use4Muls)) { stg_io.out context_* twiddles(index) } else
//           DspContext.alter(DspContext.current.copy(trimType = NoTrim, overflowType = Grow, complexUse4Muls = params.use4Muls)) { stg_in context_* twiddles(index) }

          val mulres = if (params.decimType == DIFDecimType) 
          DspContext.alter(DspContext.current.copy(
            trimType = NoTrim,// params.trimType,//Floor,
            overflowType = Grow,
            complexUse4Muls = params.use4Muls)){ stg_io.out context_* twiddles(index) } else
          DspContext.alter(DspContext.current.copy(
            trimType = NoTrim, //params.trimType,
            overflowType = Grow,
            complexUse4Muls = params.use4Muls)){ stg_in context_* twiddles(index) }

          ioOuttw := DspContext.withTrimType(params.trimType) { mulres.trimBinary(bpos) }
        }
        if (params.decimType == DIFDecimType) {
          outputWires(index) := ioOuttw
        }
        else {
          stg_io.in := ioOuttw
          outputWires(index) := stg_io.out
        }
      }
      else {
        // sdf-stage with trivial *j multiplication
        if (!doNothing) {
          twiddles(index) := rstProtoTwiddle
          val invertSignal = if (params.decimType == DIFDecimType) ShiftRegister(Mux(stg_io.cntr < (delays(index).U), Mux(stg_io.cntr < ((delays(index).U)  >> 1), false.B, true.B), false.B), params.numAddPipes, en = true.B)
          else Mux(stg_io.cntr < (delays(index).U), false.B, Mux(stg_io.cntr < ((delays(index)*3/2).U), false.B, true.B))
          val invertData = Wire(stg_io.out.cloneType)

          if (params.decimType == DIFDecimType) {
            invertData.real := stg_io.out.imag
            invertData.imag := -stg_io.out.real
            outputWires(index) := ShiftRegister(Mux(invertSignal, invertData, stg_io.out), complexMulLatency, en = true.B)
          }
          else {
            invertData.real := stg_in.imag
            invertData.imag := -stg_in.real
            outputWires(index) := stg_io.out
            stg_io.in := ShiftRegister(Mux(invertSignal, invertData, input), complexMulLatency, en = true.B)
          }
        }
        else {
         // no multiplication, just bypassing data
          twiddles(index) := rstProtoTwiddle
          if (params.decimType == DITDecimType) {
            stg_io.in := ShiftRegister(input, complexMulLatency, en = true.B)
            outputWires(index) := stg_io.out
          }
          else {
            outputWires(index) := ShiftRegister(stg_io.out, complexMulLatency, en = true.B)
          }
        }
     }
     outputWires(index)
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
  
  val dataWidthIn = fft_in_bits.real.getWidth
  val dataWidthOut= fft_out_bits.real.getWidth
  val div2Num = numStages - (dataWidthOut - dataWidthIn)

  if (latency == 0) {
    when (fftOrifft === true.B) {
      //fft_out_bits := output
      if (params.trimEnable && div2Num > 0) {
        fft_out_bits.real := DspContext.alter(DspContext.current.copy(
                      trimType = Convergent, binaryPointGrowth = 0)) { output.real.div2(div2Num) }
        fft_out_bits.imag := DspContext.alter(DspContext.current.copy(
                      trimType = Convergent, binaryPointGrowth = 0)) { output.imag.div2(div2Num) }
      }
      else {
        fft_out_bits := output
      }
    }
    .otherwise {
      // not tested
      if (params.trimEnable && div2Num > 0) {
        fft_out_bits.real := DspContext.alter(DspContext.current.copy(
                      trimType = Convergent, binaryPointGrowth = 0)) { output.real.div2(div2Num) } * scalar
        fft_out_bits.imag := DspContext.alter(DspContext.current.copy(
                      trimType = Convergent, binaryPointGrowth = 0)) { output.imag.div2(div2Num) } * scalar
      }
      else {
        fft_out_bits.real := output.imag * scalar
        fft_out_bits.imag := output.real * scalar
      }
    }
    io.out.bits := fft_out_bits
    io.out.valid := outValid 
  }
  else {
    when (fftOrifft === true.B) {
      if (params.trimEnable && div2Num > 0) {
        // here apply convergent rounding
        fft_out_bits.real := DspContext.alter(DspContext.current.copy(
                      trimType = Convergent, binaryPointGrowth = 0)) { outQueue.io.deq.bits.real.div2(div2Num) }
        fft_out_bits.imag := DspContext.alter(DspContext.current.copy(
                      trimType = Convergent, binaryPointGrowth = 0)) { outQueue.io.deq.bits.imag.div2(div2Num) }
      }
      else {
        fft_out_bits := outQueue.io.deq.bits
      }
    }
    .otherwise {
      // not tested
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

class SDFStageRadix22IO[T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in  = Input(params.protoIQ)
  val out = Output(params.protoIQ)
  val scale = if (params.keepMSBorLSBReg) Some(Input(Bool())) else None
  val overflow     =  if (params.overflowReg) Some(Output(Bool())) else None
  // control signals
  val cntr         = Input(UInt(log2Up(params.numPoints).W))
  val en           = Input(Bool())
  
  override def cloneType: this.type = SDFStageRadix22IO(params).asInstanceOf[this.type]
}
object SDFStageRadix22IO {
  def apply[T <: Data : Ring](params: FFTParams[T]): SDFStageRadix22IO[T] = new SDFStageRadix22IO(params)
}

class SDFStageRadix22[T <: Data : Real : Ring : BinaryRepresentation](val params: FFTParams[T], val delay: Int, val useGrow: Boolean, val keepMSBOrLSB: Boolean) extends Module {
  params.checkNumPointsPow2()
  require(isPow2(delay) && delay >= 1, "delay must be a power of 2 greater than or equal to 1")
  
  val io = IO(SDFStageRadix22IO(params))
  val inp = Wire(params.protoIQ.cloneType)
  val out = Wire(params.protoIQ.cloneType)

  val totalDataWidth = params.protoIQ.real.getWidth*2
  val complexMulLatency = if (params.use4Muls) {
    params.numAddPipes + params.numMulPipes
  } else {
    2 * params.numAddPipes + params.numMulPipes
  }
  inp := io.in
  io.out := out
  val butterfly_outputs = Seq.fill(2)(Wire(params.protoIQ.cloneType))
  
  //for both DIT and DIF algorithm condition is io.cntr < delay
  val load_input = if (params.decimType == DIFDecimType) io.cntr < delay.U else ShiftRegister(io.cntr < delay, complexMulLatency, resetData = false.B, en = true.B) 
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
      //shift_out := ShiftRegisterMem(shift_in, delay, en = ShiftRegister(io.en, complexMulLatency, true.B), name = this.name + s"_mem")
      shift_out := ShiftRegisterMem(shift_in, delay, en = ShiftRegister(io.en, complexMulLatency, true.B), name = "SRAM" + "_depth_" + delay.toString + "_width_" + totalDataWidth.toString + s"_mem")
    }
    else {
      shift_out := ShiftRegister(shift_in, delay, en = ShiftRegister(io.en, complexMulLatency, true.B))
    }
  }

  val butt_outputs = Butterfly[T](Seq(shift_out, inp))
  
  val overflow = Wire(Bool())
  if (useGrow) {
    butt_outputs.zip(butterfly_outputs).foreach { case(out_val, out_wire) => out_wire := out_val }
    // assign overflow output
    overflow := false.B
  }
  else {
    val sumWithDiv2 = Seq(DspContext.alter(DspContext.current.copy(
                      trimType = params.trimType, binaryPointGrowth = 0))
                      { butt_outputs(0).div2(1) },
                    DspContext.alter(DspContext.current.copy(
                      trimType = params.trimType, binaryPointGrowth = 0))
                      { butt_outputs(1).div2(1) })
        
    // val overflow = params.protoIQ.match { for final design
    // This solution is not elegant. Perhaps somehow overflow function can be added to all type classes and then use simple indexing for 
    // checking specific bits.
    // e.g. 
    // val tooBig = !butt_outputs(cWidth) && butt_outputs(cWidth - 1)
    // val tooSmall = butt_outputs(cWidth) && !butt_outputs(cWidth - 1)
        
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

object SDFRadix22App extends App
{
  val params = FFTParams.fixed(
    dataWidth = 16,
    twiddleWidth = 16,
    numPoints = 1024,
    decimType = DITDecimType,
    runTime = true,
    numAddPipes = 2,
    numMulPipes = 2,
    expandLogic = Array.fill(log2Up(1024))(0),
    keepMSBorLSB = Array.fill(log2Up(1024))(true),
    binPoint = 1
  )
  chisel3.Driver.execute(args,()=>new SDFChainRadix22(params))
  
//   val arguments = Array(
//     "-X", "verilog",
//     "--repl-seq-mem", "-c:SDFChainRadix22:-o:mem.conf",
//     "--log-level", "info"
//   )
  // generate blackbox-es for memories
//   (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() =>new SDFChainRadix22(params))))
}

