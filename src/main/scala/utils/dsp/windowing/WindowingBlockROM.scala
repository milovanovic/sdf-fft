package dspblocks.utils.dsp.windowing

import chisel3.util._
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._
//import chisel3.util.experimental.loadMemoryFromFile
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters
import java.io._

trait WindowingROMStandaloneBlock extends AXI4WindowingBlockROM[FixedPoint] {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m =>
    {
      val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

      m :=
        BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
        ioMemNode

      val ioMem = InModuleBody { ioMemNode.makeIO() }
      ioMem
    }
  }

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

abstract class WindowingBlockROM[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](
  val params: WindowingParams[T],
  beatBytes:  Int)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {
  val streamNode = AXI4StreamIdentityNode()
  val numMulPipes = params.numMulPipes

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    val address_rom = RegInit(0.U((log2Ceil(params.numPoints)).W)) // implement with Option
    val numPoints = Wire(UInt((log2Ceil(params.numPoints)).W)) // be careful not saved in register

    val fftSize =
      RegInit(
        params.numPoints.U(log2Ceil(params.numPoints + 1).W)
      ) // default value is equal to compile time parameter for fft size
    val fftDir = RegInit(true.B)
    val enableWind = RegInit(false.B) // just check does this enableWind works as it should
    val selFunc =
      if (params.windFuncSeq.isDefined) Some(RegInit(0.U(log2Ceil(params.windFuncSeq.get.size).W))) else None

    // generate simple state machine to handle address generation for window ram
    val sIdle :: sProcess :: Nil = Enum(2)
    val state = RegInit(sIdle)
    val state_next = Wire(state.cloneType)
    state_next := state
    val winCoeff = Wire(params.protoWin)

    if (params.windFuncSeq.isEmpty) {
      val windowSeq = params.windowFunc match {
        case WindowFunctionTypes.Hamming(_, alpha, beta, _) =>
          WindowFunctions.hammingWindow(params.numPoints, alpha, beta)
        case WindowFunctionTypes.Hanning(_, _) => WindowFunctions.hanningWindow(params.numPoints)
        case WindowFunctionTypes.Blackman(_, a0, a1, a2, _) =>
          WindowFunctions.blackmanWindow(params.numPoints, a0, a1, a2)
        case WindowFunctionTypes.Triangular(_, _) => WindowFunctions.triangularWindow(params.numPoints)
        case WindowFunctionTypes.User(_, userWindow) => {
          require(
            userWindow.length == params.numPoints,
            "Length of specified window function is not the same as fft size"
          )
          userWindow
        }
        case WindowFunctionTypes.None(_) => Seq.fill(params.numPoints)(1.0)
      }

      val windowROM = VecInit(windowSeq.map(t => {
        DspContext.withTrimType(Convergent) {
          val coeff = params.protoWin.fromDoubleWithFixedWidth(t)
          coeff
        }
      }))
      val bposWin = (params.protoWin match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _ => 0
      })

      val dirName = params.dirName
      val dir = new File(dirName)
      if (!(dir.exists() && dir.isDirectory))
        dir.mkdir()
      val file = new File(params.memoryFile)
      val w = new BufferedWriter(new FileWriter(file))
      val windowSeqShifted = windowSeq.map(c => c * scala.math.pow(2, bposWin).toInt)
      for (i <- windowSeq.indices) {
        w.write(f"${windowSeqShifted(i).toInt}%02x" + "\n")
      }
      w.close()
    } else {
      val winCoeffs = Wire(Vec(params.windFuncSeq.get.size, params.protoWin.cloneType))
      for ((func, index) <- params.windFuncSeq.get.zipWithIndex) {
        val windowSeq = func match {
          case WindowFunctionTypes.Hamming(_, alpha, beta, _) =>
            WindowFunctions.hammingWindow(params.numPoints, alpha, beta)
          case WindowFunctionTypes.Hanning(_, _) => WindowFunctions.hanningWindow(params.numPoints)
          case WindowFunctionTypes.Blackman(_, a0, a1, a2, _) =>
            WindowFunctions.blackmanWindow(params.numPoints, a0, a1, a2)
          case WindowFunctionTypes.Triangular(_, _) => WindowFunctions.triangularWindow(params.numPoints)
          case WindowFunctionTypes.User(_, userWindow) => {
            require(
              userWindow.length == params.numPoints,
              "Length of specified window function is not the same as fft size"
            )
            userWindow
          }
          case WindowFunctionTypes.None(_) => Seq.fill(params.numPoints)(1.0)
        }

        val windowROM = VecInit(windowSeq.map(t => {
          DspContext.withTrimType(Convergent) {
            val coeff = params.protoWin.fromDoubleWithFixedWidth(t)
            coeff
          }
        }))
        winCoeffs(index) := windowROM(address_rom)
      }
      winCoeff := winCoeffs(selFunc.get)
    }

    switch(state) {
      is(sIdle) {
        when(in.fire) {
          state_next := sProcess
        }
      }
      is(sProcess) {
        when(in.bits.last) {
          state_next := sIdle
        }
      }
    }
    state := state_next

    when(in.fire) {
      address_rom := address_rom + 1.U
    }
    when(address_rom === (numPoints - 1.U) && in.fire) {
      address_rom := 0.U
    }

    //val winCoeff = windowROM(address_rom)

    if (params.runTime)
      numPoints := fftSize
    else
      numPoints := params.numPoints.U

    var fields = Seq[RegField]()

    // Define register fields
    fields = fields ++ Seq(
      // settable registers
      RegField(
        log2Ceil(params.numPoints),
        fftSize,
        RegFieldDesc(name = "fftSize", desc = "contains fft size which is used for run time configurability control")
      ),
      RegField(1, enableWind, RegFieldDesc(name = "enableWin", desc = "enable or disable windowing")),
      RegField(1, fftDir, RegFieldDesc(name = "fftDir", desc = "transform direction: fft or ifft"))
    )
    if (params.windFuncSeq.isDefined) {
      fields = fields :+ RegField(
        selFunc.get.getWidth,
        selFunc.get,
        RegFieldDesc(name = "selFunc", desc = "Defines windowing function")
      )
    }
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f) }): _*)

    //val inComplex = if (params.constWindow) in.bits.data.asTypeOf(params.protoIQ) else RegNext(in.bits.data.asTypeOf(params.protoIQ))
    val inComplex = Wire(params.protoIQ.cloneType)
    inComplex.real := in.bits.data(in.bits.data.getWidth / 2 - 1, 0).asTypeOf(params.protoIQ.imag)
    inComplex.imag := in.bits.data(in.bits.data.getWidth - 1, in.bits.data.getWidth / 2).asTypeOf(params.protoIQ.real)

    val windowedInput = Wire(params.protoIQ.cloneType)

    when(enableWind) {
      DspContext.alter(
        DspContext.current.copy(
          trimType = params.trimType,
          numMulPipes = params.numMulPipes,
          binaryPointGrowth = 0
        )
      ) {
        windowedInput.real := inComplex.real.context_*(winCoeff)
        windowedInput.imag := inComplex.imag.context_*(winCoeff)
      }
    }.otherwise {
      windowedInput := ShiftRegister(inComplex, numMulPipes, true.B)
    }

    if (params.constWindow && numMulPipes == 0) {
      out.valid := in.valid
      out.bits.data := windowedInput.asUInt
      out.bits.last := in.bits.last
    } else {
      val queueDelay = numMulPipes + 1
      val inputsDelay = numMulPipes
      val queueData = Module(new Queue(params.protoIQ.cloneType, queueDelay, flow = true)) // + 1 for input delaying
      queueData.io.enq.bits := windowedInput
      queueData.io.enq.valid := ShiftRegister(in.valid && in.ready, inputsDelay, true.B)
      queueData.io.deq.ready := out.ready

      val queueLast = Module(new Queue(Bool(), queueDelay, flow = true)) // +1 for input delaying
      queueLast.io.enq.valid := ShiftRegister(in.valid && in.ready, inputsDelay, true.B) // +1 for input delaying
      queueLast.io.enq.bits := ShiftRegister(in.bits.last, inputsDelay, true.B) // +1 for input delaying
      queueLast.io.deq.ready := out.ready

      // Connect output
      out.valid := queueData.io.deq.valid
      out.bits.data := queueData.io.deq.bits.asUInt
      out.bits.last := queueLast.io.deq.bits
    }
    in.ready := out.ready
  }
}

class AXI4WindowingBlockROM[T <: Data: Real: BinaryRepresentation](
  params:    WindowingParams[T],
  address:   AddressSet,
  beatBytes: Int = 4
)(
  implicit p: Parameters)
    extends WindowingBlockROM[
      T,
      AXI4MasterPortParameters,
      AXI4SlavePortParameters,
      AXI4EdgeParameters,
      AXI4EdgeParameters,
      AXI4Bundle
    ](params, beatBytes)
    with AXI4DspBlock
    with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes)) // use AXI4 memory mapped
}

object WindowingBlockROMApp extends App {
  val paramsWindowing = WindowingParams.fixed(
    dataWidth = 16,
    binPoint = 14,
    numMulPipes = 1,
    trimType = Convergent,
    constWindow = true,
    dirName = "test_run_dir",
    memoryFile = "./test_run_dir/blacman.txt",
    windowFunc = WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
  )
  implicit val p: Parameters = Parameters.empty

  val testModule = LazyModule(
    new AXI4WindowingBlockROM(paramsWindowing, address = AddressSet(0x010000, 0xff), beatBytes = 4)
      with WindowingROMStandaloneBlock {
      override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    }
  )
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => testModule.module)))
}
