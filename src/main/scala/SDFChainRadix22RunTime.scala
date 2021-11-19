// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._
import chisel3.experimental.FixedPoint
import breeze.numerics.{cos, sin}
import scala.math.{Pi, pow}
import dspblocks.CounterWithReset
import dsptools.{DspContext}


object TwiddleRadix22LookUp {
  def apply[T <: Data : Real](address: UInt, stageN : Int, protoTwiddle: DspComplex[T]): DspComplex[T] = {
    require(stageN >= 4, "Parameter stageN must be at least 4 or larger")
    val twiddleIdxs =
      Seq.fill(stageN / 4)(0) ++
      Seq.tabulate(stageN / 4)(i => i * 2) ++
      Seq.tabulate(stageN / 4)(i => i) ++
      Seq.tabulate(stageN / 4)(i => i * 3) // ++
   // println(twiddleIdxs)
    println(stageN)
    val uniqueTwiddleIdxs = twiddleIdxs.distinct.sorted
    val uniqueTwiddleTable = VecInit(uniqueTwiddleIdxs.map(t =>
      DspContext.withTrimType(Convergent) { //is this visible - check it!
      DspComplex.wire(
        real = ConvertableTo[T].fromDoubleWithFixedWidth( math.cos(2 * math.Pi * t / stageN), protoTwiddle.real),
        imag = ConvertableTo[T].fromDoubleWithFixedWidth(-math.sin(2 * math.Pi * t / stageN), protoTwiddle.imag)
      )}))
    val twiddleIdxTable = VecInit(twiddleIdxs.map(i => {
      ((uniqueTwiddleIdxs.indexOf(i))).U
    }))
    /*uniqueTwiddleIdxs.map(t =>
      println(math.cos(2 * math.Pi * t / stageN).toString ++ " + j" ++ (-math.sin(2 * math.Pi * t / stageN)).toString)
    )*/
    uniqueTwiddleTable(twiddleIdxTable(address))
  }
}


/**
 * Radix-2^2 module which can provide full run time configurability
 * Instantiates and connects SDF FFT stages in series and provides necessary control signals for each stage
 */

class SDFChainRadix22RunTime[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  params.checkNumPointsPow2()
  params.checkExpandLogic()
  params.checkPipeline()
  
  require(Seq(DITDecimType, DIFDecimType).contains(params.decimType), s"""Decimation type must either be dit or dif""")
  val io = IO(FFTIO(params))
  
  // Calculation of constants
  val numPointsDiv2     = params.numPoints / 2                                                                // FFT size / 2
  val numStages         = log2Up(params.numPoints)
  val delayLog2s        = if (params.decimType == DITDecimType) (0 until numStages) else (0 until numStages).reverse // log2(delay) of each nth stage
  val logn = log2Ceil(params.numPoints)
 
  // cumulative delays
  val delays            = delayLog2s.map(d => pow(2, d).toInt)                                                // actual delay of each nth stage
  val cumulative_delays = delays.scanLeft(0)(_ + _) // 
  val cumulativeDelaysWires = Wire(Vec(cumulative_delays.length, UInt(cumulative_delays.last.U.getWidth.W))) //
  cumulativeDelaysWires := cumulative_delays.map(e => e.U)
  val cumulativeDelayWire = Wire(UInt(cumulativeDelaysWires.last.getWidth.W))
  val oddEvenStages = Wire(Vec(numStages, Bool()))
  val invertSignals = Wire(Vec(numStages, Bool()))
  val enableVector = Wire(Vec(numStages, Bool()))
  val outputWires = Wire(Vec(numStages, io.out.bits.cloneType))
  //val outputWires = Wire(MixedVec((0 to numStages) map { i => params.protoIQstages(i) }))
  //can not be addressed with Chisel type, index must be Scala Int type
  val regNumStages = RegInit(numStages.U((log2Up(numStages)+1).W))
  val numPoints = Wire(UInt((log2Up(params.numPoints + 1)).W))
  val activeStages = Wire(Vec(numStages, Bool()))
    
  val cntr_wires = Wire(Vec(numStages, UInt(log2Up(params.numPoints).W)))
  val numTw = if (params.numPoints > 2) (numStages % 2) + (numStages/2-1) else 0
  val twiddles = Wire(Vec(numTw,params.protoTwiddle))
  val twiddlesVector = Wire(Vec(numStages, params.protoTwiddle))
  val twAddr = Wire(Vec(numTw, UInt(log2Up(params.numPoints).W)))
  val twiddleAddr = Wire(Vec(numStages, UInt(log2Up(params.numPoints).W)))
  val cnt = RegInit(0.U((logn + 1).W))  

  val muxOutputTw = Wire(MixedVec((0 to numStages-1) map { i => params.protoIQstages(i) }))//Wire(Vec(numStages, params.protoIQstages(numStages - 1)))

  // try to reduce this condition
  val isShiftAddr = !((((numStages % 2) == 0).B  && (regNumStages % 2.U === 0.U)) || (((numStages % 2) == 1).B  && (regNumStages % 2.U === 1.U)))
  
  val fftOrifft = RegInit(true.B)
  val fft_in_bits  = Wire(io.in.bits.cloneType)
  val fft_out_bits = Wire(io.out.bits.cloneType)

  if (params.runTime == true)
    numPoints := (2.U << (regNumStages-1.U))
  else 
    numPoints := params.numPoints.U

  if (params.decimType == DIFDecimType)
    cumulativeDelayWire := cumulativeDelaysWires(numStages.U - regNumStages)
  else
    cumulativeDelayWire := 0.U

  activeStages.zip(oddEvenStages).zipWithIndex.map { case ((active, oddEven), index) => {
      val ind = if (params.decimType == DIFDecimType) index.U else (numStages - index - 1).U
      active := Mux(numStages.U - regNumStages <= ind, true.B, false.B)
      oddEven :=  Mux((numStages.U - regNumStages) <= ind, ((ind - (numStages.U - regNumStages)) % 2.U), false.B)
    }
  }
  
  (0 until numStages by 2).zipWithIndex.map { case (logStage, index) => {
    val stageN = 1 << (numStages - logStage) 
    val lastStage = if (numStages % 2 == 0) stageN == 4 else stageN == 2
    if (!lastStage) {
      if (params.decimType == DIFDecimType) {
        twiddles(index) := TwiddleRadix22LookUp[T](twAddr(index), stageN, params.protoTwiddle)
      }
      else {
        twiddles(numTw-index-1) := TwiddleRadix22LookUp[T](twAddr(numTw-index-1), stageN, params.protoTwiddle)
      }
     }
    }
  }

  twAddr.zipWithIndex.map {
    case (address, ind) => {
       /*val ind = if (params.decimType == DIFDecimType) index else numTw - index - 1
       address := Mux(isShiftAddr, twiddleAddr(ind*2+2), twiddleAddr(ind*2+1))*/
      if (params.decimType == DIFDecimType) {
        address := Mux(isShiftAddr, twiddleAddr(ind*2+2), twiddleAddr(ind*2+1))
      }
      else {
        if (numStages % 2 == 1)
          address := Mux(isShiftAddr, twiddleAddr(ind*2), twiddleAddr(ind*2+1))
        else
          address := Mux(isShiftAddr, twiddleAddr(ind*2+1), twiddleAddr(ind*2+2))
      }
    }
  }
  
  val rstProtoIQ = Wire(io.in.bits.cloneType)
  rstProtoIQ.real := Real[T].fromDouble(0.0)
  rstProtoIQ.imag := Real[T].fromDouble(0.0)
  
  val rstProtoTwiddle = Wire(params.protoTwiddle.cloneType)
  rstProtoTwiddle.real := Real[T].fromDouble(0.0)
  rstProtoTwiddle.imag := Real[T].fromDouble(0.0)
  
  val sIdle :: sProcess :: sFlush :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val state_next = Wire(state.cloneType)
  
  val scaleBflyReg = RegInit(VecInit(Seq.fill(numStages)(false.B)))
  val overflowReg = RegInit(VecInit(Seq.fill(numStages)(false.B)))

  state_next := state
  val fireLast = io.lastIn && io.in.fire()
  // simple state machine
  switch (state) {
    is (sIdle) {
      // register initialization
      regNumStages := io.fftSize.getOrElse(numStages.U) // number of stages
      scaleBflyReg := io.keepMSBorLSBReg.getOrElse(VecInit(Seq.fill(numStages)(false.B)))
      fftOrifft := io.fftDirReg.getOrElse(params.fftDir.B)
      when (io.in.fire()) { state_next := sProcess }
    }
    is (sProcess) {
      when (io.lastIn && io.in.fire()) {
        state_next := sFlush
      }
    }
    is (sFlush) {
      when (io.lastOut) { // frame is processed
        state_next := sIdle
      }
    }
  }
  state := state_next
  
  val lastStageCnt = cntr_wires(regNumStages-1.U)
  val lastStageEn = enableVector(regNumStages-1.U)

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
  // added lastStageCnt =/= (numPoints - 1.U)
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
  
  
  when (fftOrifft === true.B) {
    fft_in_bits := io.in.bits
  }
  .otherwise {
    fft_in_bits.real := io.in.bits.imag
    fft_in_bits.imag := io.in.bits.real
  }

  val sdf_stages = delayLog2s.zip(delays).zipWithIndex.map {
    case ((delayLog2, delay), ind) => {
      val stageparams = params.copy(protoIQ = params.protoIQstages(ind))
      val useGrow = if (stageparams.expandLogic(ind) == 1) true else false
      val stage = Module(new SDFStageRadix22(stageparams, delay=delay, useGrow, params.keepMSBorLSB(ind)))
      if (params.keepMSBorLSBReg) {
        stage.io.scale.get := io.keepMSBorLSBReg.get(ind)
      }
      overflowReg(ind) := stage.io.overflow.getOrElse(false.B)
      
      val doNothing = if (params.decimType == DIFDecimType) ((numStages-1).U === ind.U) else ind.U === 0.U
      when (oddEvenStages(ind) && !doNothing && activeStages(ind)) {
        invertSignals(ind) := false.B
        if (params.decimType == DIFDecimType) {
          val currIndex = ind.U - (numStages.U - regNumStages)
          val address = (cntr_wires(currIndex) - (cumulativeDelaysWires(ind+1) - cumulativeDelayWire))
          twiddleAddr(ind) := Mux(isShiftAddr, address<<1, address)
          val indexToTwiddles = Mux(isShiftAddr, ((ind.U >> 1.U) - 1.U), (ind.U >> 1.U))
          twiddlesVector(ind) := ShiftRegister(twiddles(indexToTwiddles), params.numAddPipes, en = true.B)
        }
        else {
          val address = (cntr_wires(ind.U) - (cumulativeDelaysWires(ind) - cumulativeDelayWire))
          val indexToTwiddles = if (numStages % 2 == 0) ((ind.U - 1.U) >> 1.U) else (ind.U >> 1.U)
          twiddleAddr(ind) := Mux(isShiftAddr, address<<1, address)
          twiddlesVector(ind) := twiddles(indexToTwiddles)
        }
      }
      .elsewhen (!doNothing && activeStages(ind)) {
        if (params.decimType == DIFDecimType)
          invertSignals(ind) := ShiftRegister(Mux(stage.io.cntr < (delay.U), Mux(stage.io.cntr < ((delay.U)  >> 1), false.B, true.B), false.B), params.numAddPipes, en = true.B)
        else
          invertSignals(ind) := Mux(stage.io.cntr < (delay.U), false.B, Mux(stage.io.cntr < ((delay*3/2).U), false.B, true.B))
        twiddleAddr(ind) := 0.U
        twiddlesVector(ind) := rstProtoTwiddle
      }
      .otherwise {
        invertSignals(ind) := false.B
        twiddleAddr(ind) := 0.U
        twiddlesVector(ind) := rstProtoTwiddle
      }
      stage
    }
  }
  val initialOutDone = RegInit(false.B)
  /*
  val stopEnable = RegInit(false.B)
  when (state === sIdle) {
    stopEnable := false.B
  }
  .elsewhen (state === sFlush && cntr_wires(0) === numPoints-1.U) {
    stopEnable := true.B
  }*/
  val enableInit = io.in.fire() || (state === sFlush && io.out.ready)// && stopEnable === false.B)
  
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
  val initialDelay = params.numPoints
  
  val input_data = Mux(activeStages(0), fft_in_bits, rstProtoIQ)

  //(0 until numStages).foldLeft(((io.in.fire(), cnt))) {
  (0 until numStages).foldLeft(((enableInit, cnt))) {
    case ((stageEn, stageCnt), currStage) => {
      enableVector(currStage) := stageEn //collect all enable signals
      cntr_wires(currStage) := stageCnt //collect all shifted counter values
      (
        ShiftRegisterWithReset(stageEn, outputLatency, resetData = false.B, reset = state_next === sIdle, en = true.B),
        ShiftRegisterWithReset(stageCnt, outputLatency, resetData = 0.U, reset = state_next === sIdle, en = true.B)
      )
    }
  }

  sdf_stages.zipWithIndex.map { case (e, idx) => {
      val condition = if (params.decimType == DIFDecimType) (idx.U < (numStages.U - (regNumStages))) else (idx.U >= regNumStages)
      val index = if (params.decimType == DIFDecimType)  idx.U - (numStages.U - regNumStages) else idx.U
      e.io.en := Mux(condition, false.B, enableVector(index))
      e.io.cntr := (cntr_wires(index) - (cumulativeDelaysWires(idx) - cumulativeDelayWire)) (delayLog2s(idx),0)
    }
  }
  
  //TODO: reuse DIF logic (code) for DIT input and output signal manipulation
  sdf_stages.map(_.io).zipWithIndex.foldLeft(input_data) {
    case (stg_in, (stg_io, index)) => {
      val input = Wire(stg_io.in.cloneType)
      //val tmpInput = Wire(rstProtoIQ.cloneType)
      val tmpInput = Wire(stg_io.in.cloneType)
      val rstProtoStg = Wire(stg_io.in.cloneType)
      rstProtoStg.real := Real[T].fromDouble(0.0)
      rstProtoStg.imag := Real[T].fromDouble(0.0)
      
      val invertData = Wire(stg_io.out.cloneType)
      val gen = params.protoIQstages(index).real.cloneType
      val bpos = (gen match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _ => 0
      })
      val isMulStage = ((params.decimType == DIFDecimType) && (index % 2 == 1)) || (params.decimType == DITDecimType && ((numStages % 2 == 0 && index % 2 == 0) || (numStages % 2 == 1 && index % 2 == 1)))
      
      if (params.decimType == DIFDecimType) {
        when (index.U === (numStages.U-regNumStages)) {
          input := fft_in_bits
        }
        .otherwise {
          input := stg_in
        }
        when (activeStages(index)) {
          stg_io.in := input
        }
       .otherwise {
          stg_io.in := rstProtoStg //be careful when expandLogic is used!
        }
        invertData.real := Mux(invertSignals(index), stg_io.out.imag, stg_io.out.real)
        invertData.imag := Mux(invertSignals(index), -stg_io.out.real, stg_io.out.imag)
      }
      else {
        stg_io.in := tmpInput
        input := stg_in
        invertData.real :=  Mux(invertSignals(index), stg_in.imag, stg_in.real)
        invertData.imag :=  Mux(invertSignals(index), -stg_in.real, stg_in.imag)
      }
      if ((index == 0 && params.decimType == DIFDecimType) || (index == (numStages - 1) && params.decimType == DITDecimType)) {
          when (activeStages(index) === true.B) {
            muxOutputTw(index) := invertData
          }
          .otherwise {
            muxOutputTw(index) := rstProtoStg
          }
          tmpInput := ShiftRegister(muxOutputTw(index), complexMulLatency, en = true.B)
      }
      else {
        if ((index == (numStages - 1) && params.decimType == DIFDecimType) || ((index == 0) && params.decimType == DITDecimType)) {
          if (params.decimType == DIFDecimType) {
            tmpInput := ShiftRegister(stg_io.out, complexMulLatency, en = true.B)
            muxOutputTw(index) := stg_io.out
          }
          else {
            tmpInput := ShiftRegister(input, complexMulLatency, en = true.B)
            muxOutputTw(index) := input
          }
        }
      else if (isMulStage) {
          val inputToMul = Wire(stg_io.out.cloneType)
          val twiddle =  Wire(params.protoTwiddle)
          if (params.decimType == DIFDecimType) {
            //inputToMul := Mux(activeStages(index) && oddEvenStages(index), stg_io.out, Mux(activeStages(index) , outputWires(index + 1).asTypeOf(stg_io.out), rstProtoStg))
            val outNextStage = Wire(stg_io.out.cloneType)
            outNextStage := outputWires(index+1)
            inputToMul := Mux(activeStages(index) && oddEvenStages(index), stg_io.out, Mux(activeStages(index), outNextStage, rstProtoStg))
            twiddle := Mux(activeStages(index) && oddEvenStages(index), twiddlesVector(index), Mux(activeStages(index), twiddlesVector(index + 1), rstProtoTwiddle))
          }
          else {
            val outputVal = Wire(stg_io.in.cloneType)
            if (numStages % 2 == 0)
              outputVal := outputWires(index - 2) // no problem
            else
              outputVal := outputWires(index)

            inputToMul := Mux(activeStages(index) && oddEvenStages(index), stg_in.asTypeOf(stg_io.in), Mux(activeStages(index), outputVal, rstProtoStg))
            val twiddleVal = if (numStages % 2 == 0) twiddlesVector(index - 1) else twiddlesVector(index + 1)
            twiddle := Mux(activeStages(index) && oddEvenStages(index), twiddlesVector(index), Mux(activeStages(index), twiddleVal, rstProtoTwiddle))
          }
          DspContext.alter(DspContext.current.copy(
            numAddPipes = params.numAddPipes,
            numMulPipes = params.numMulPipes
          )) {
            val mulres =  DspContext.alter(DspContext.current.copy(trimType = params.trimType, overflowType = Grow, complexUse4Muls = params.use4Muls)) { inputToMul context_* twiddle }
            muxOutputTw(index) := DspContext.withTrimType(params.trimType) { mulres.trimBinary(bpos) }
          }
          tmpInput := Mux(~(activeStages(index) && oddEvenStages(index)), ShiftRegister(invertData, complexMulLatency, en = true.B), muxOutputTw(index))
        }
        else {
          if (params.decimType == DITDecimType) {
            if (numStages % 2 == 0)
              muxOutputTw(index) := muxOutputTw(index+1)
            else
              muxOutputTw(index) := muxOutputTw(index-1)
          }
          else {
             muxOutputTw(index) := muxOutputTw(index-1).asTypeOf(muxOutputTw(index))
          }
          tmpInput := Mux(~(activeStages(index) && oddEvenStages(index)), ShiftRegister(invertData, complexMulLatency, en = true.B), muxOutputTw(index))
        }
      }
        val out = Wire(stg_io.out.cloneType)
        outputWires(index) := stg_io.out
        if (params.decimType == DIFDecimType) {
          out := tmpInput
        }
        else {
          out := outputWires(index)
        }
        out
      }
  }
//  val lastStageCnt = cntr_wires(regNumStages-1.U) //generate Mux in verilog code
//  val lastStageEn =  enableVector(regNumStages - 1)
  
 // Note: error for numPoints = 2 run vs compile time
  when (lastStageCnt === (numPoints - 2.U) && lastStageEn) {
    initialOutDone := true.B
  }
  .elsewhen (state_next === sIdle) {
    initialOutDone := false.B
  }
  
  val validOutBeforePipes = Mux(numPoints === 2.U, RegNext(enableInit) && io.in.fire(), lastStageEn && initialOutDone)
  
  val output = if (params.decimType == DIFDecimType) ShiftRegister(outputWires.last, complexMulLatency, en = true.B) else outputWires(regNumStages-1.U)
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

object SDFRadix22AppRunTime extends App
{
  val params = FFTParams.fixed(
    dataWidth = 16,
    twiddleWidth = 16,
    numPoints = 16,
    decimType = DIFDecimType,
    numAddPipes = 1,
    numMulPipes = 0,
    runTime = false,
    expandLogic = Array.fill(log2Up(16))(0),
    keepMSBorLSB = Array.fill(log2Up(16))(true),
    binPoint = 0
  )
  chisel3.Driver.execute(args,()=>new SDFChainRadix22RunTime(params))
}


