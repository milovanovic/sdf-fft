// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dsptools._
import dsptools.numbers._

import scala.math.pow
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}


/**
  * Interface of the sdf-fft
  */
class FFTIO [T <: Data : Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.protoIQ))
    val out = if (params.trimEnable) Decoupled(params.protoIQOut) else Decoupled(params.protoIQstages(log2Up(params.numPoints)-1))

  val lastOut = Output(Bool())
  val lastIn = Input(Bool())
  // control registers
  val fftSize = if (params.runTime) Some(Input(UInt((log2Up(params.numPoints)).W))) else None
  val keepMSBorLSBReg = if (params.keepMSBorLSBReg) Some(Input(Vec(log2Up(params.numPoints),Bool()))) else None
  val fftDirReg = if (params.fftDirReg) Some(Input(Bool())) else None
  //val flushDataOut = Input(Bool())
  
  // status registers
  val busy = Output(Bool())
  val overflow = if (params.overflowReg) Some(Output(Vec(log2Up(params.numPoints),Bool()))) else None
  
  override def cloneType: this.type = FFTIO(params).asInstanceOf[this.type]
}

object FFTIO {
  def apply[T <: Data : Ring](params: FFTParams[T]): FFTIO[T] = new FFTIO(params)
}


/**
  * Top level sdf core
  */
class SDFFFT[T <: Data : Real : BinaryRepresentation](val params: FFTParams[T]) extends Module {
  val io = IO(FFTIO(params))
  params.checkFftType()
  params.checkSDFRadix()
  
  val bitreverse_flag = if (params.useBitReverse) 1 else 0
  override def desiredName = "SDFFFT" + "_size_" + params.numPoints.toString + "_width_" + params.protoIQ.real.getWidth.toString + "_radix_" + params.sdfRadix + "_bitreverse_" + bitreverse_flag.toString
  
  // generate rom for window coefficients
  val windowSeq = params.windowFunc match {
    case WindowFunctionTypes.Hamming(_, alpha, beta, _) => WindowFunctions.hammingWindow(params.numPoints, alpha, beta)
    case WindowFunctionTypes.Hanning(_, _) => WindowFunctions.hanningWindow(params.numPoints)
    case WindowFunctionTypes.Blackman(_, a0, a1, a2, _) => WindowFunctions.blackmanWindow(params.numPoints, a0, a1, a2)
    case WindowFunctionTypes.Triangular(_, _) => WindowFunctions.triangularWindow(params.numPoints)
    case WindowFunctionTypes.User(_, userWindow) => {
      require(userWindow.length == params.numPoints, "Length of specified window function is not the same as fft size")
      userWindow
    }
    case WindowFunctionTypes.None(_) => Seq.fill(params.numPoints)(1.0)
  }
  val window = VecInit(windowSeq.map(t => {
    val coeff = ConvertableTo[T].fromDoubleWithFixedWidth(t, params.protoWin)
    coeff
  }))

  val cntWin = RegInit(0.U((log2Ceil(params.numPoints)).W))
  val numPoints = Wire(UInt((log2Ceil(params.numPoints)).W))

  if (params.runTime == true)
    numPoints := (2.U << (io.fftSize.get-1.U))
  else 
    numPoints := params.numPoints.U

  when (io.in.fire()) {
    cntWin := cntWin + 1.U
  }
  when (io.lastIn || cntWin === (numPoints - 1.U)) {
    cntWin := 0.U
  }
  dontTouch(cntWin)
  cntWin.suggestName("cntWin")
  
  val log2Size = log2Ceil(params.numPoints)                  // nummber of stages
  val subSizes = (1 to log2Size).map(d => pow(2, d).toInt)   // all possible number of stages
  val subSizesWire = subSizes.map(e => (e.U).asTypeOf(numPoints))
  val bools = subSizesWire.map(e => e === numPoints)         // create conditions - if run time is off this logic is not going to be generated
  val cases = bools.zip(1 to log2Size).map { case (bool, numBits) =>
    bool -> Reverse(cntWin(numBits-1, 0))
  }
/*  
  bools.map{ c => dontTouch(c) }
  subSizesWire.map { c => dontTouch(c) }*/
    // runTime configurability here is not checked
  val addrWin = if (params.decimType == DITDecimType && !params.useBitReverse) MuxCase(0.U(log2Size.W), cases) else cntWin
  
  dontTouch(addrWin)
  addrWin.suggestName("addrWin")
  
  params.sdfRadix match {
    case "2" => {
      val fft = Module(new SDFChainRadix2(params))
      if (params.useBitReverse) {
        if (params.decimType == DIFDecimType) {
          val paramsBR = BitReversePingPongParams(
            proto = params.protoIQstages.last,
            pingPongSize = params.numPoints,
            adjustableSize = params.runTime,
            bitReverseDir =  true
          )
          val bitReversal = Module(new BitReversePingPong(paramsBR))
          if (params.runTime) {
            bitReversal.io.size.get := 1.U << io.fftSize.get
          }
          when (io.fftDirReg.getOrElse(params.fftDir.B)) {
            /**** do windowing *****/
            fft.io.in.bits.real := io.in.bits.real * window(addrWin)
            fft.io.in.bits.imag := io.in.bits.imag * window(addrWin)
            /**********************/
            fft.io.in.valid := io.in.valid
            io.in.ready := fft.io.in.ready
            io.out <> bitReversal.io.out
          }
          .otherwise {
            fft.io.in <> io.in
            /**** do inverse windowing ? ****/
            io.out.bits := bitReversal.io.out.bits
            /**********************/
            io.out.valid := bitReversal.io.out.valid
            bitReversal.io.out.ready := io.out.ready
          }
          fft.io.lastIn := io.lastIn
          bitReversal.io.in <> fft.io.out
          bitReversal.io.lastIn := fft.io.lastOut
          io.lastOut := bitReversal.io.lastOut
          dontTouch(io.lastOut)
        }
        else {
        // DIT
          val paramsBR = BitReversePingPongParams(
            proto = params.protoIQstages.last,
            pingPongSize = params.numPoints,
            adjustableSize = params.runTime,
            bitReverseDir =  false
          )
          val bitReversal = Module(new BitReversePingPong(paramsBR))
          if (params.runTime) {
            bitReversal.io.size.get := 1.U << io.fftSize.get
          }
          bitReversal.io.in <> io.in
          bitReversal.io.lastIn := io.lastIn
          
          when (io.fftDirReg.getOrElse(params.fftDir.B)) {
            /***** do windowing **********/
            //fft.io.in <> bitReversal.io.out
            fft.io.in.bits.real := bitReversal.io.out.bits.real * window(addrWin)
            fft.io.in.bits.imag := bitReversal.io.out.bits.imag * window(addrWin)
            /*****************************/
            fft.io.in.valid := bitReversal.io.out.valid
            //io.in.ready := bitReversal.io.out.ready
            bitReversal.io.out.ready := fft.io.in.ready
            io.out <> fft.io.out
          }
          .otherwise {
            fft.io.in <> bitReversal.io.out
            /***** do inverse windowing ? **********/
            io.out.bits := fft.io.out.bits
            /*****************************/
            io.out.valid := fft.io.out.valid
            fft.io.out.ready := io.out.ready
          }
          fft.io.lastIn := bitReversal.io.lastOut
          io.lastOut := fft.io.lastOut
        }
      }
      else {
        // cntWin for dif natural, dit bit reversed
        // check fft/ifft
        when (io.fftDirReg.getOrElse(params.fftDir.B)) {
        //   fft.io.in <> io.in
          /******** do windowing ****************/
          fft.io.in.bits.real := (io.in.bits.real * window(addrWin))
          fft.io.in.bits.imag := (io.in.bits.imag * window(addrWin))
          /**************************************/
          fft.io.in.valid := io.in.valid
          io.in.ready := fft.io.in.ready
          io.out <> fft.io.out
        }
       .otherwise {
          fft.io.in <> io.in
          /********* do inverse windowing ? ******/
          io.out.bits := fft.io.out.bits
          /***************************************/
          io.out.valid := fft.io.out.valid
          fft.io.out.ready := io.out.ready
        }
        fft.io.lastIn := io.lastIn
        io.lastOut := fft.io.lastOut
      }
      io.busy := fft.io.busy
      if (params.runTime) {
        fft.io.fftSize.get := io.fftSize.get
      }
      if (params.overflowReg) {
        io.overflow.get := fft.io.overflow.get
      }
      if (params.keepMSBorLSBReg) {
        fft.io.keepMSBorLSBReg.get := io.keepMSBorLSBReg.get
      }
      if (params.fftDirReg) {
        fft.io.fftDirReg.get := io.fftDirReg.get
      }
    }
    case "2^2" => {
      val fft = if (params.runTimeR22.getOrElse(false) == true) Module(new SDFChainRadix22RunTime(params)) else Module(new SDFChainRadix22(params))
      if (params.useBitReverse) {
        if (params.decimType == DIFDecimType) {
          val paramsBR = BitReversePingPongParams(
            proto = params.protoIQstages.last,
            pingPongSize = params.numPoints,
            adjustableSize = params.runTime,
            bitReverseDir =  true
          )
          val bitReversal = Module(new BitReversePingPong(paramsBR))
          if (params.runTime) {
            bitReversal.io.size.get := 1.U << io.fftSize.get
          }
          when (io.fftDirReg.getOrElse(params.fftDir.B)) {
            // do multiplication on input
             //fft.io.in <> io.in
            /**** do windowing *****/
            fft.io.in.bits.real := io.in.bits.real * window(addrWin)
            fft.io.in.bits.imag := io.in.bits.imag * window(addrWin)
            /**********************/
            fft.io.in.valid := io.in.valid
            io.in.ready := fft.io.in.ready
            io.out <> bitReversal.io.out
          }
          .otherwise {
            fft.io.in <> io.in
            /**** do inverse windowing? ****/
            io.out.bits := bitReversal.io.out.bits
            /*******************************/
            io.out.valid := bitReversal.io.out.valid
            bitReversal.io.out.ready := io.out.ready
          }
          fft.io.lastIn := io.lastIn
          bitReversal.io.in <> fft.io.out
          bitReversal.io.lastIn := fft.io.lastOut
          io.lastOut := bitReversal.io.lastOut
        }
        else {
        // DIT
          val paramsBR = BitReversePingPongParams(
            proto = params.protoIQstages.last,
            pingPongSize = params.numPoints,
            adjustableSize = params.runTime,
            bitReverseDir =  false
          )
          val bitReversal = Module(new BitReversePingPong(paramsBR))
          if (params.runTime) {
            bitReversal.io.size.get := 1.U << io.fftSize.get
          }
          bitReversal.io.in <> io.in
          bitReversal.io.lastIn := io.lastIn
          
          when (io.fftDirReg.getOrElse(params.fftDir.B)) {
            /***** do windowing **********/
            //fft.io.in <> bitReversal.io.out
            bitReversal.io.out.ready := fft.io.in.ready
            fft.io.in.bits.real := bitReversal.io.out.bits.real * window(addrWin)
            fft.io.in.bits.imag := bitReversal.io.out.bits.imag * window(addrWin)
            /****************************/
            fft.io.in.valid := bitReversal.io.out.valid
            //io.in.ready := bitReversal.io.out.ready
            bitReversal.io.out.ready := fft.io.in.ready
            io.out <> fft.io.out
          }
          .otherwise {
            fft.io.in <> bitReversal.io.out
            /***** do inverse windowing? *********/
            io.out.bits := fft.io.out.bits
            /*************************************/
            io.out.valid := fft.io.out.valid
            fft.io.out.ready := io.out.ready
          }
          fft.io.lastIn := bitReversal.io.lastOut
          io.lastOut := fft.io.lastOut
        }
      }
      else {
        // cntWin for dif natural, dit bit reversed
        // check fft/ifft
        when (io.fftDirReg.getOrElse(params.fftDir.B)) {
        //   fft.io.in <> io.in
          /******** do windowing ******************/
          fft.io.in.bits.real := (io.in.bits.real * window(addrWin))
          fft.io.in.bits.imag := (io.in.bits.imag * window(addrWin))
          /****************************************/
          fft.io.in.valid := io.in.valid
          io.in.ready := fft.io.in.ready
          io.out <> fft.io.out
        }
       .otherwise {
          fft.io.in <> io.in
          /********* do inverse windowing? *******/
          io.out.bits := fft.io.out.bits
          /***************************************/
          io.out.valid := fft.io.out.valid
          fft.io.out.ready := io.out.ready
        }
        fft.io.lastIn := io.lastIn
        io.lastOut := fft.io.lastOut
      }
      
      io.busy := fft.io.busy
      // optional ports/registers
      if (params.runTime) {
        fft.io.fftSize.get := io.fftSize.get
      }
      if (params.overflowReg) {
        io.overflow.get := fft.io.overflow.get
      }
      if (params.keepMSBorLSBReg) {
        fft.io.keepMSBorLSBReg.get := io.keepMSBorLSBReg.get
      }
      if (params.fftDirReg) {
        fft.io.fftDirReg.get := io.fftDirReg.get
      }
    }
  }
}

object SDFFFTApp extends App
{
  implicit def int2bool(b: Int) = if (b == 1) true else false
  if (args.length < 5) {
    println("This application requires at least 5 arguments")
  }
  val buildDirName = args(0).toString
  val wordSize = args(1).toInt
  val fftSize = args(2).toInt
  val isBitReverse = int2bool(args(3).toInt)
  val radix = args(4).toString
  val separateVerilog = int2bool(args(5).toInt)

  val params = FFTParams.fixed(
    dataWidth = wordSize,
    binPoint = 0,
    twiddleWidth = 16,
    numPoints = fftSize,
    decimType = DIFDecimType,
    useBitReverse = isBitReverse,
    numAddPipes = 1,
    numMulPipes = 1,
    sdfRadix = radix,
    runTime = true,
    expandLogic = Array.fill(log2Up(fftSize))(0),
    keepMSBorLSB = Array.fill(log2Up(fftSize))(true),
    minSRAMdepth = 8
  )
  print(radix)
  if (separateVerilog == true) {
    val arguments = Array(
      "--target-dir", buildDirName,
      "-e", "verilog",
      "-X", "verilog",
      "--repl-seq-mem", "-c:SDFChainRadix22:-o:mem.conf",
      "--log-level", "info"
    )
    (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() =>new SDFFFT(params))))
  }
  else {
    val arguments = Array(
      "--target-dir", buildDirName,
      "-X", "verilog",
      "--repl-seq-mem", "-c:SDFChainRadix22:-o:mem.conf",
      "--log-level", "info"
    )
    (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() =>new SDFFFT(params))))
  }
}
