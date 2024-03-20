package dspblocks.sdffft

import breeze.numerics.pow
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import dspblocks.utils.dsp.zeropadder._
import dspblocks.utils.dsp.windowing._
import dsptools.numbers._
//import scala.math.pow

//import firrtl.transforms.{Flatten, FlattenAnnotation, NoDedupAnnotation}
//import chisel3.util.experimental.{FlattenInstance, InlineInstance}

abstract trait HasIO extends Module {
  val io: Bundle
}

/**
  * Interface of the sdf-fft
  */
class FFTIO[T <: Data: Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.protoIQ))
  val out =
    if (params.trimEnable) Decoupled(params.protoIQOut)
    else Decoupled(params.protoIQstages(log2Up(params.numPoints) - 1))

  val lastOut = Output(Bool())
  val lastIn = Input(Bool())
  // control registers
  val fftSize = if (params.runTime) Some(Input(UInt((log2Up(params.numPoints)).W))) else None
  val keepMSBorLSBReg = if (params.keepMSBorLSBReg) Some(Input(Vec(log2Up(params.numPoints), Bool()))) else None
  val fftDirReg = if (params.fftDirReg) Some(Input(Bool())) else None
  //val flushDataOut = Input(Bool())

  // status registers
  val busy = Output(Bool())
  val overflow = if (params.overflowReg) Some(Output(Vec(log2Up(params.numPoints), Bool()))) else None
}

object FFTIO {
  def apply[T <: Data: Ring](params: FFTParams[T]): FFTIO[T] = new FFTIO(params)
}

class FFTTopIO[T <: Data: Ring](params: FFTParams[T]) extends Bundle {
  val in = Flipped(Decoupled(params.protoIQ))
  val out =
    if (params.trimEnable) Decoupled(params.protoIQOut)
    else Decoupled(params.protoIQstages(log2Up(params.numPoints) - 1))

  val lastOut = Output(Bool())
  val lastIn = Input(Bool())
  // control registers
  val fftSize = if (params.runTime) Some(Input(UInt((log2Up(params.numPoints)).W))) else None
  val keepMSBorLSBReg = if (params.keepMSBorLSBReg) Some(Input(Vec(log2Up(params.numPoints), Bool()))) else None
  val fftDirReg = if (params.fftDirReg) Some(Input(Bool())) else None
  //val flushDataOut = Input(Bool())

  // status registers
  val busy = Output(Bool())
  val overflow = if (params.overflowReg) Some(Output(Vec(log2Up(params.numPoints), Bool()))) else None

  // control registers that are connected to zeropadding logic
  val packetSizeStart = if (params.zeroPadderParams.isDefined) Some(Input(UInt(log2Up(params.numPoints).W))) else None
  val numberOfPackets =
    if (params.zeroPadderParams.isDefined) Some(Input(UInt(log2Up(params.zeroPadderParams.get.numberOfPackets).W)))
    else None
  // control register for enabling windowing
  val enWind = if (params.windowFunc != WindowFunctionTypes.None()) Some(Input(Bool())) else None
  //override def cloneType: this.type = FFTIO(params).asInstanceOf[this.type]
}

object FFTTopIO {
  def apply[T <: Data: Ring](params: FFTParams[T]): FFTTopIO[T] = new FFTTopIO(params)
}

/**
  * Top level sdf-fft core
  */
class SDFFFT[T <: Data: Real: BinaryRepresentation](val params: FFTParams[T]) extends Module {
  //val io = IO(FFTIO(params))
  val io = IO(FFTTopIO(params))

  params.checkFftType()
  params.checkSDFRadix()
  params.checkZeroPadder()

  val bitreverse_flag = if (params.useBitReverse) 1 else 0
  override def desiredName =
    "SDFFFT" + "_size_" + params.numPoints.toString + "_width_" + params.protoIQ.real.getWidth.toString + "_radix_" + params.sdfRadix + "_bitreverse_" + bitreverse_flag.toString

  val numPoints = Wire(UInt((log2Ceil(params.numPoints)).W))

  if (params.runTime)
    numPoints := (2.U << (io.fftSize.get - 1.U))
  else
    numPoints := params.numPoints.U

  // generate rom for window coefficients
  val windowSeq = params.windowFunc match {
    case WindowFunctionTypes.Hamming(_, alpha, beta, _) => WindowFunctions.hammingWindow(params.numPoints, alpha, beta)
    case WindowFunctionTypes.Hanning(_, _)              => WindowFunctions.hanningWindow(params.numPoints)
    case WindowFunctionTypes.Blackman(_, a0, a1, a2, _) => WindowFunctions.blackmanWindow(params.numPoints, a0, a1, a2)
    case WindowFunctionTypes.Triangular(_, _)           => WindowFunctions.triangularWindow(params.numPoints)
    case WindowFunctionTypes.User(_, userWindow) => {
      require(userWindow.length == params.numPoints, "Length of specified window function is not the same as fft size")
      userWindow
    }
    case WindowFunctionTypes.None(_) => Seq.fill(params.numPoints)(1.0)
  }

  val window =
    if (params.windowFunc != WindowFunctionTypes.None()) Some(VecInit(windowSeq.map(t => {
      val coeff = ConvertableTo[T].fromDoubleWithFixedWidth(t, params.protoWin.get)
      coeff
    })))
    else None

  val cntWin = RegInit(0.U((log2Ceil(params.numPoints)).W))
  val cntEn = WireDefault(false.B)
  val lastCnt = WireDefault(false.B)

  // no zeropadder for DIT variant
  when(cntEn) {
    cntWin := cntWin + 1.U
  }
  when((lastCnt || cntWin === (numPoints - 1.U)) && cntEn) {
    cntWin := 0.U
  }

  val log2Size = log2Ceil(params.numPoints) // nummber of stages
  val subSizes = (1 to log2Size).map(d => pow(2, d).toInt) // all possible number of stages
  val subSizesWire = subSizes.map(e => (e.U).asTypeOf(numPoints))
  val bools =
    subSizesWire.map(e =>
      e === numPoints
    ) // create conditions - if run time is off this logic is not going to be generated
  val cases = bools.zip(1 to log2Size).map {
    case (bool, numBits) =>
      bool -> Reverse(cntWin(numBits - 1, 0))
  }
  /*
    bools.map{ c => dontTouch(c) }
    subSizesWire.map { c => dontTouch(c) }*/
  // runTime configurability here is not checked
  val addrWin =
    if (params.decimType == DITDecimType && !params.useBitReverse) MuxCase(0.U(log2Size.W), cases) else cntWin

  params.sdfRadix match {
    case "2" => {
      val fft = Module(new SDFChainRadix2(params))
      if (params.useBitReverse) {
        val paramsBR = BitReversePingPongParams(
          proto = params.protoIQstages.last,
          pingPongSize = params.numPoints,
          adjustableSize = params.runTime,
          fftShiftEnable = params.fftShiftEnable.getOrElse(false),
          bitReverseDir = true,
          singlePortSRAM = params.singlePortSRAM
        )
        val bitReversal = Module(new BitReversePingPong(paramsBR))
        if (params.runTime) {
          bitReversal.io.size.get := 1.U << io.fftSize.get
        }
        if (params.zeroPadderParams.isDefined) {
          val zeropadder = Module(new ZeroPadderNative(params.zeroPadderParams.get))
          // Connect inputs
          zeropadder.io.in.valid := io.in.valid
          zeropadder.io.packetSizeStart := io.packetSizeStart.get
          zeropadder.io.packetSizeEnd := io.fftSize.get //OrElse(params.numPoints.U)
          zeropadder.io.numberOfPackets := io.numberOfPackets.get //OrElse(params.zeroPadderParams.get.numberOfPackets.U)
          zeropadder.io.in.bits := io.in.bits
          io.in.ready := zeropadder.io.in.ready
          zeropadder.io.lastIn := io.lastIn

          if (params.decimType == DIFDecimType) {
            // if windowing
            // windowing goes after zeropadder
            if (params.windowFunc != WindowFunctionTypes.None()) {
              cntEn := zeropadder.io.out.fire && io.enWind.get
              lastCnt := zeropadder.io.lastOut && cntEn
              val inComplex = Wire(DspComplex(params.protoIQ.real.cloneType))
              inComplex := zeropadder.io.out.bits
              dontTouch(addrWin)
              dontTouch(window.get(addrWin))
              fft.io.in.bits.real := inComplex.real * window.get(addrWin)
              fft.io.in.bits.imag := inComplex.imag * window.get(addrWin)
            } else {
              fft.io.in.bits := zeropadder.io.out.bits
            }
            fft.io.in.valid := zeropadder.io.out.valid
            zeropadder.io.out.ready := fft.io.in.ready
            io.in.ready := zeropadder.io.in.ready
            io.out <> bitReversal.io.out

            fft.io.lastIn := zeropadder.io.lastOut
            bitReversal.io.in <> fft.io.out
            bitReversal.io.lastIn := fft.io.lastOut
            io.lastOut := bitReversal.io.lastOut
            dontTouch(io.lastOut)
          } else {
            // DIT
            zeropadder.io.lastIn := io.lastIn
            zeropadder.io.in <> io.in
            zeropadder.io.lastIn := io.lastIn

            bitReversal.io.in <> zeropadder.io.out
            bitReversal.io.lastIn := zeropadder.io.lastOut
            fft.io.in <> bitReversal.io.out
            io.out.bits := fft.io.out.bits
            io.out.valid := fft.io.out.valid
            fft.io.out.ready := io.out.ready
            fft.io.lastIn := bitReversal.io.lastOut
            io.lastOut := fft.io.lastOut
          }
        } else {
          if (params.decimType == DIFDecimType) {
            when(io.fftDirReg.getOrElse(params.fftDir.B)) {
              // if windowing
              if (params.windowFunc != WindowFunctionTypes.None()) {
                cntEn := io.in.fire && io.enWind.get
                lastCnt := io.lastIn && cntEn
                fft.io.in.bits.real := io.in.bits.real * window.get(addrWin)
                fft.io.in.bits.imag := io.in.bits.imag * window.get(addrWin)
              } else {
                fft.io.in.bits.real := io.in.bits.real //* window(addrWin)
                fft.io.in.bits.imag := io.in.bits.imag //* window(addrWin)
              }
              fft.io.in.valid := io.in.valid
              io.in.ready := fft.io.in.ready
              io.out <> bitReversal.io.out
            }.otherwise {
              fft.io.in <> io.in
              io.out.bits := bitReversal.io.out.bits

              io.out.valid := bitReversal.io.out.valid
              bitReversal.io.out.ready := io.out.ready
            }
            fft.io.lastIn := io.lastIn
            bitReversal.io.in <> fft.io.out
            bitReversal.io.lastIn := fft.io.lastOut
            io.lastOut := bitReversal.io.lastOut
            dontTouch(io.lastOut)
          } else {
            bitReversal.io.in <> io.in
            bitReversal.io.lastIn := io.lastIn
            when(io.fftDirReg.getOrElse(params.fftDir.B)) {
              if (params.windowFunc != WindowFunctionTypes.None()) {
                cntEn := bitReversal.io.in.fire && io.enWind.get
                lastCnt := bitReversal.io.lastIn && cntEn
                fft.io.in.bits.real := bitReversal.io.out.bits.real * window.get(addrWin)
                fft.io.in.bits.imag := bitReversal.io.out.bits.imag * window.get(addrWin)
              } else {
                fft.io.in.bits.real := bitReversal.io.out.bits.real
                fft.io.in.bits.imag := bitReversal.io.out.bits.imag
              }
              fft.io.in.valid := bitReversal.io.out.valid
              bitReversal.io.out.ready := fft.io.in.ready
              io.out <> fft.io.out
            }.otherwise {
              fft.io.in <> bitReversal.io.out
              io.out.bits := fft.io.out.bits
              io.out.valid := fft.io.out.valid
              fft.io.out.ready := io.out.ready
            }
            fft.io.lastIn := bitReversal.io.lastOut
            io.lastOut := fft.io.lastOut
          }
        }
      } else {
        // cntWin for dif natural, dit bit reversed
        if (params.zeroPadderParams.isDefined && params.decimType == DIFDecimType) {
          val zeropadder = Module(new ZeroPadderNative(params.zeroPadderParams.get))
          // Connect inputs
          zeropadder.io.in.valid := io.in.valid
          zeropadder.io.packetSizeStart := io.packetSizeStart.get
          zeropadder.io.packetSizeEnd := io.fftSize.get //OrElse(params.numPoints.U)
          zeropadder.io.numberOfPackets := io.numberOfPackets.get //OrElse(params.zeroPadderParams.get.numberOfPackets.U)
          zeropadder.io.in.bits := io.in.bits
          io.in.ready := zeropadder.io.in.ready
          zeropadder.io.lastIn := io.lastIn
          fft.io.in <> zeropadder.io.in
          io.out.bits := fft.io.out.bits
          io.out.valid := fft.io.out.valid
          fft.io.out.ready := io.out.ready

          fft.io.lastIn := zeropadder.io.lastIn
          io.lastOut := fft.io.lastOut
        } else {
          when(io.fftDirReg.getOrElse(params.fftDir.B)) {
            if (params.windowFunc != WindowFunctionTypes.None()) {
              cntEn := io.in.fire && io.enWind.get
              lastCnt := io.lastIn && cntEn
              fft.io.in.bits.real := io.in.bits.real * window.get(addrWin)
              fft.io.in.bits.imag := io.in.bits.imag * window.get(addrWin)
            } else {
              fft.io.in.bits.real := io.in.bits.real //* window(addrWin))
              fft.io.in.bits.imag := io.in.bits.imag //* window(addrWin))
            }
            fft.io.in.valid := io.in.valid
            io.in.ready := fft.io.in.ready
            io.out <> fft.io.out
          }.otherwise {
            fft.io.in <> io.in
            io.out.bits := fft.io.out.bits
            io.out.valid := fft.io.out.valid
            fft.io.out.ready := io.out.ready
          }
          fft.io.lastIn := io.lastIn
          io.lastOut := fft.io.lastOut
        }
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
      val fft =
        if (params.runTimeR22.getOrElse(false)) Module(new SDFChainRadix22RunTime(params))
        else
          Module(
            new SDFChainRadix22(params)
          ) //Module(new SDFChainRadix22RunTime(params)  with FlattenInstance) else Module(new SDFChainRadix22(params) with FlattenInstance)

      if (params.useBitReverse) {
        val paramsBR = BitReversePingPongParams(
          proto = params.protoIQstages.last,
          pingPongSize = params.numPoints,
          adjustableSize = params.runTime,
          fftShiftEnable = params.fftShiftEnable.getOrElse(false),
          bitReverseDir = true,
          singlePortSRAM = params.singlePortSRAM
        )
        val bitReversal = Module(new BitReversePingPong(paramsBR))
        if (params.runTime) {
          bitReversal.io.size.get := 1.U << io.fftSize.get
        }
        if (params.zeroPadderParams.isDefined) {
          val zeropadder = Module(new ZeroPadderNative(params.zeroPadderParams.get))
          // Connect inputs
          zeropadder.io.in.valid := io.in.valid
          zeropadder.io.packetSizeStart := io.packetSizeStart.get
          zeropadder.io.packetSizeEnd := io.fftSize.get //OrElse(params.numPoints.U)
          zeropadder.io.numberOfPackets := io.numberOfPackets.get //OrElse(params.zeroPadderParams.get.numberOfPackets.U)
          zeropadder.io.in.bits := io.in.bits
          io.in.ready := zeropadder.io.in.ready
          zeropadder.io.lastIn := io.lastIn

          if (params.decimType == DIFDecimType) {
            // if windowing!
            fft.io.in.bits := zeropadder.io.out.bits
            fft.io.in.valid := zeropadder.io.out.valid
            zeropadder.io.out.ready := fft.io.in.ready
            io.in.ready := zeropadder.io.in.ready
            io.out <> bitReversal.io.out

            fft.io.lastIn := zeropadder.io.lastOut
            bitReversal.io.in <> fft.io.out
            bitReversal.io.lastIn := fft.io.lastOut
            io.lastOut := bitReversal.io.lastOut
            dontTouch(io.lastOut)
          } else {
            // DIT
            zeropadder.io.lastIn := io.lastIn
            zeropadder.io.in <> io.in
            zeropadder.io.lastIn := io.lastIn

            bitReversal.io.in <> zeropadder.io.out
            bitReversal.io.lastIn := zeropadder.io.lastOut
            fft.io.in <> bitReversal.io.out
            io.out.bits := fft.io.out.bits
            io.out.valid := fft.io.out.valid
            fft.io.out.ready := io.out.ready
            fft.io.lastIn := bitReversal.io.lastOut
            io.lastOut := fft.io.lastOut
          }
        } else {
          if (params.decimType == DIFDecimType) {
            when(io.fftDirReg.getOrElse(params.fftDir.B)) {
              if (params.windowFunc != WindowFunctionTypes.None()) {
                cntEn := io.in.fire && io.enWind.get
                lastCnt := io.lastIn && cntEn
                fft.io.in.bits.real := io.in.bits.real * window.get(addrWin)
                fft.io.in.bits.imag := io.in.bits.imag * window.get(addrWin)
              } else {
                fft.io.in.bits.real := io.in.bits.real //* window(addrWin)
                fft.io.in.bits.imag := io.in.bits.imag //* window(addrWin)
              }
              fft.io.in.valid := io.in.valid
              io.in.ready := fft.io.in.ready
              io.out <> bitReversal.io.out
            }.otherwise {
              fft.io.in <> io.in
              io.out.bits := bitReversal.io.out.bits

              io.out.valid := bitReversal.io.out.valid
              bitReversal.io.out.ready := io.out.ready
            }
            fft.io.lastIn := io.lastIn
            bitReversal.io.in <> fft.io.out
            bitReversal.io.lastIn := fft.io.lastOut
            io.lastOut := bitReversal.io.lastOut
            dontTouch(io.lastOut)
          } else {
            bitReversal.io.in <> io.in
            bitReversal.io.lastIn := io.lastIn
            when(io.fftDirReg.getOrElse(params.fftDir.B)) {
              if (params.windowFunc != WindowFunctionTypes.None()) {
                cntEn := bitReversal.io.in.fire && io.enWind.get
                lastCnt := bitReversal.io.lastIn && cntEn
                fft.io.in.bits.real := bitReversal.io.out.bits.real * window.get(addrWin)
                fft.io.in.bits.imag := bitReversal.io.out.bits.imag * window.get(addrWin)
              } else {
                fft.io.in.bits.real := bitReversal.io.out.bits.real
                fft.io.in.bits.imag := bitReversal.io.out.bits.imag
              }
              fft.io.in.valid := bitReversal.io.out.valid
              bitReversal.io.out.ready := fft.io.in.ready
              io.out <> fft.io.out
            }.otherwise {
              fft.io.in <> bitReversal.io.out
              io.out.bits := fft.io.out.bits
              io.out.valid := fft.io.out.valid
              fft.io.out.ready := io.out.ready
            }
            fft.io.lastIn := bitReversal.io.lastOut
            io.lastOut := fft.io.lastOut
          }
        }
      } else {
        // cntWin for dif natural, dit bit reversed
        if (params.zeroPadderParams.isDefined && params.decimType == DIFDecimType) {
          val zeropadder = Module(new ZeroPadderNative(params.zeroPadderParams.get))
          // Connect inputs
          zeropadder.io.in.valid := io.in.valid
          zeropadder.io.packetSizeStart := io.packetSizeStart.get
          zeropadder.io.packetSizeEnd := io.fftSize.get //OrElse(params.numPoints.U)
          zeropadder.io.numberOfPackets := io.numberOfPackets.get //OrElse(params.zeroPadderParams.get.numberOfPackets.U)
          zeropadder.io.in.bits := io.in.bits
          io.in.ready := zeropadder.io.in.ready
          zeropadder.io.lastIn := io.lastIn
          fft.io.in <> zeropadder.io.in
          io.out.bits := fft.io.out.bits
          io.out.valid := fft.io.out.valid
          fft.io.out.ready := io.out.ready

          fft.io.lastIn := zeropadder.io.lastIn
          io.lastOut := fft.io.lastOut
        } else {
          when(io.fftDirReg.getOrElse(params.fftDir.B)) {
            if (params.windowFunc != WindowFunctionTypes.None()) {
              cntEn := io.in.fire && io.enWind.get
              lastCnt := io.lastIn && cntEn
              fft.io.in.bits.real := io.in.bits.real * window.get(addrWin)
              fft.io.in.bits.imag := io.in.bits.imag * window.get(addrWin)
            } else {
              fft.io.in.bits.real := io.in.bits.real //* window(addrWin))
              fft.io.in.bits.imag := io.in.bits.imag //* window(addrWin))
            }
            fft.io.in.valid := io.in.valid
            io.in.ready := fft.io.in.ready
            io.out <> fft.io.out
          }.otherwise {
            fft.io.in <> io.in
            io.out.bits := fft.io.out.bits
            io.out.valid := fft.io.out.valid
            fft.io.out.ready := io.out.ready
          }
          fft.io.lastIn := io.lastIn
          io.lastOut := fft.io.lastOut
        }
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
  }
}

object SDFFFTSimpleApp extends App {
  val buildDirName = "verilog"
  val wordSize = 16
  val fftSize = 256
  val isBitReverse = true
  val radix = "2"
  val separateVerilog = true

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
      "--target-dir",
      buildDirName,
      "-e",
      "verilog",
      "-X",
      "verilog",
      "--repl-seq-mem",
      "-c:SDFChainRadix22:-o:mem.conf",
      "--log-level",
      "info"
    )
    //(new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() =>new SDFFFT(params) with FlattenInstance)))
    (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() => new SDFFFT(params))))
  } else {
    val arguments = Array(
      "--target-dir",
      buildDirName,
      "-X",
      "verilog",
      "--repl-seq-mem",
      "-c:SDFChainRadix22:-o:mem.conf",
      "--log-level",
      "info"
    )
    //(new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() =>new SDFFFT(params) with FlattenInstance)))
    (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() => new SDFFFT(params))))
  }
}

object SDFFFTApp extends App {
  implicit def int2bool(b: Int) = if (b == 1) true else false
  if (args.length < 5) {
    println("This application requires at least 5 arguments, check Makefile")
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
  if (separateVerilog == true) {
    val arguments = Array(
      "--target-dir",
      buildDirName,
      "-e",
      "verilog",
      "-X",
      "verilog",
      "--repl-seq-mem",
      "-c:SDFChainRadix22:-o:mem.conf",
      "--log-level",
      "info"
    )
    //(new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() =>new SDFFFT(params) with FlattenInstance)))
    (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() => new SDFFFT(params))))
  } else {
    val arguments = Array(
      "--target-dir",
      buildDirName,
      "-X",
      "verilog",
      "--repl-seq-mem",
      "-c:SDFChainRadix22:-o:mem.conf",
      "--log-level",
      "info"
    )
    (new ChiselStage).execute(arguments, Seq(ChiselGeneratorAnnotation(() => new SDFFFT(params))))
  }
}
