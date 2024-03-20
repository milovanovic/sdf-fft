package dspblocks.utils.dsp.windowing

import chisel3.util._
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._
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

trait WindowingROMMultipleInOutsStandaloneBlock extends AXI4WindowingBlockROMMultipleInOuts[FixedPoint] {
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
  val numIns = 4
  val ins: Seq[ModuleValue[AXI4StreamBundle]] = for (i <- 0 until numIns) yield {
    implicit val valName = ValName(s"in_$i")
    val in = BundleBridgeSource[AXI4StreamBundle](() => AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
    streamNode :=
      BundleBridgeToAXI4Stream(AXI4StreamMasterPortParameters(AXI4StreamMasterParameters(n = 4))) :=
      in
    InModuleBody { in.makeIO() }
  }
  val outs: Seq[ModuleValue[AXI4StreamBundle]] = for (o <- 0 until numIns) yield {
    implicit val valName = ValName(s"out_$o")
    val out = BundleBridgeSink[AXI4StreamBundle]()
    out :=
      AXI4StreamToBundleBridge(AXI4StreamSlavePortParameters(AXI4StreamSlaveParameters())) :=
      streamNode
    InModuleBody { out.makeIO() }
  }
}

abstract class WindowingBlockROMMultipleInOuts[T <: Data: Real: BinaryRepresentation, D, U, E, O, B <: Data](
  val params: WindowingParams[T],
  beatBytes:  Int)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  val streamNode = AXI4StreamNexusNode(
    masterFn =
      (ms: Seq[AXI4StreamMasterPortParameters]) => AXI4StreamMasterPortParameters(ms.map(_.masters).reduce(_ ++ _)),
    slaveFn = ss => {
      AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _))
    }
  )
  val numMulPipes = params.numMulPipes

  lazy val module = new LazyModuleImp(this) {
    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip

    val address_rom = RegInit(0.U((log2Ceil(params.numPoints)).W)) // implement with Option
    val numPoints = Wire(UInt((log2Ceil(params.numPoints)).W)) // be careful not saved in register

    // Control registers
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
      winCoeff := windowROM(address_rom)
      ///////////////////////////////////////////////////
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

      for (i <- 0 until windowSeq.length) {
        w.write(f"${windowSeqShifted(i).toInt}%02x" + "\n")
      }
      w.close()
      //////////////////////////////////////////////////////
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
        when(ins(0).fire) {
          state_next := sProcess
        }
      }
      is(sProcess) {
        when(ins(0).bits.last) {
          state_next := sIdle
        }
      }
    }
    state := state_next

    when(ins(0).fire) {
      address_rom := address_rom + 1.U
    }
    when(address_rom === (numPoints - 1.U) && ins(0).fire) {
      address_rom := 0.U
    }

    val tmpCoeff = Wire(params.protoWin.cloneType)
    tmpCoeff := winCoeff

    dontTouch(tmpCoeff)
    tmpCoeff.suggestName("tmpCoeff")

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

    for ((in, inIdx) <- ins.zipWithIndex) {
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
        outs(inIdx).valid := in.valid
        //outs(inIdx).bits.data    := windowedInput.asUInt
        val realTmp = WireDefault(0.S((in.bits.data.getWidth / 2).W))
        val imagTmp = WireDefault(0.S((in.bits.data.getWidth / 2).W))
        realTmp := windowedInput.real.asTypeOf(realTmp)
        imagTmp := windowedInput.imag.asTypeOf(imagTmp)
        outs(inIdx).bits.data := Cat(imagTmp, realTmp).asUInt
        outs(inIdx).bits.last := in.bits.last
      } else {
        val queueDelay = numMulPipes + 1
        val inputsDelay = numMulPipes
        val queueData = Module(new Queue(params.protoIQ.cloneType, queueDelay, flow = true)) // + 1 for input delaying
        queueData.io.enq.bits := windowedInput
        queueData.io.enq.valid := ShiftRegister(in.valid && in.ready, inputsDelay, true.B)
        queueData.io.deq.ready := outs(inIdx).ready

        val queueLast = Module(new Queue(Bool(), queueDelay, flow = true))
        queueLast.io.enq.valid := ShiftRegister(in.valid && in.ready, inputsDelay, true.B)
        queueLast.io.enq.bits := ShiftRegister(in.bits.last, inputsDelay, true.B)
        queueLast.io.deq.ready := outs(inIdx).ready
        val realTmp = WireDefault(0.S((in.bits.data.getWidth / 2).W))
        val imagTmp = WireDefault(0.S((in.bits.data.getWidth / 2).W))
        realTmp := queueData.io.deq.bits.real.asTypeOf(realTmp)
        imagTmp := queueData.io.deq.bits.imag.asTypeOf(imagTmp)
        dontTouch(realTmp)
        dontTouch(imagTmp)
        // Connect output
        outs(inIdx).valid := queueData.io.deq.valid
        outs(inIdx).bits.data := Cat(imagTmp, realTmp).asUInt
        //outs(inIdx).bits.data    := queueData.io.deq.bits.asUInt
        outs(inIdx).bits.last := queueLast.io.deq.bits
      }
      in.ready := outs(inIdx).ready
    }
  }
}

class AXI4WindowingBlockROMMultipleInOuts[T <: Data: Real: BinaryRepresentation](
  params:    WindowingParams[T],
  address:   AddressSet,
  beatBytes: Int = 4
)(
  implicit p: Parameters)
    extends WindowingBlockROMMultipleInOuts[
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

object WindowingBlockROMMultipleInOutsApp extends App {
  val paramsWindowing = WindowingParams.fixed(
    dataWidth = 16,
    binPoint = 14,
    numMulPipes = 1,
    numPoints = 256,
    trimType = Convergent,
    constWindow = true,
    dirName = "test_run_dir",
    memoryFile = "./test_run_dir/blacman.txt",
    windowFunc = WindowFunctionTypes.Blackman(dataWidth_tmp = 16),
    windFuncSeq = Some(
      Seq(
        WindowFunctionTypes.Blackman(dataWidth_tmp = 16),
        WindowFunctionTypes.Hamming(dataWidth_tmp = 16),
        WindowFunctionTypes.Hanning(dataWidth_tmp = 16)
      )
    )
  )
  implicit val p: Parameters = Parameters.empty

  val testModule = LazyModule(
    new AXI4WindowingBlockROMMultipleInOuts(paramsWindowing, address = AddressSet(0x010000, 0xff), beatBytes = 4)
      with WindowingROMMultipleInOutsStandaloneBlock {
      override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    }
  )
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => testModule.module)))
}
