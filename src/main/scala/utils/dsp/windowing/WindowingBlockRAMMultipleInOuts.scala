package dspblocks.utils.dsp.windowing

import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._
//import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.util.experimental.loadMemoryFromFile
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

trait WindowingRAMMultipleInOutsStandaloneBlock extends AXI4WindowingBlockRAMMultipleInOuts[FixedPoint] {
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

class AXI4WindowingBlockRAMMultipleInOuts[T <: Data: Real: BinaryRepresentation](
  csrAddress: AddressSet,
  ramAddress: AddressSet,
  val params: WindowingParams[T],
  beatBytes:  Int)
    extends LazyModule()(Parameters.empty)
    with AXI4DspBlock {

  val streamNode = AXI4StreamNexusNode(
    masterFn =
      (ms: Seq[AXI4StreamMasterPortParameters]) => AXI4StreamMasterPortParameters(ms.map(_.masters).reduce(_ ++ _)),
    slaveFn = ss => {
      AXI4StreamSlavePortParameters(ss.map(_.slaves).reduce(_ ++ _))
    }
  )
  val mem = Some(AXI4IdentityNode())
  val axiRegSlaveNode = AXI4RegisterNode(address = csrAddress, beatBytes = beatBytes) // AXI4 Register

  val ramSlaveNode = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        Seq(
          AXI4SlaveParameters(
            address = Seq(ramAddress),
            supportsRead = TransferSizes(1, beatBytes),
            supportsWrite = TransferSizes(1, beatBytes),
            interleavedId = Some(0)
          )
        ),
        beatBytes = beatBytes,
        minLatency = 1
      )
    )
  )

  // Define AXI4 Xbar
  val topXbar = AXI4Xbar()

  ramSlaveNode := topXbar
  axiRegSlaveNode := topXbar
  topXbar := mem.get

  val numMulPipes = params.numMulPipes

  lazy val module = new LazyModuleImp(this) {
    val (ins, _) = streamNode.in.unzip
    val (outs, _) = streamNode.out.unzip

    val (ramIn, ramInEdge) = ramSlaveNode.in.head
    val windowMem = SyncReadMem(params.numPoints, params.protoWin)
    val r_addr_reg = RegInit(0.U((log2Ceil(params.numPoints)).W))

    val numPoints = Wire(UInt((log2Ceil(params.numPoints)).W)) // be careful not saved in register

    val w_full = RegInit(false.B)
    val wdata = ramIn.w.bits.data.asTypeOf(params.protoWin)

    // take only appropriate number of LSB bits
    val w_addr = (ramIn.aw.bits.addr(log2Ceil(params.numPoints) - 1 + log2Ceil(beatBytes), 0) >> log2Ceil(beatBytes))
      .asTypeOf(r_addr_reg)

    ramIn.aw.ready := ramIn.w.valid && (ramIn.b.ready || !w_full)
    ramIn.w.ready := ramIn.aw.valid && (ramIn.b.ready || !w_full)

    when(ramIn.b.fire) { w_full := false.B }
    when(ramIn.aw.fire) { w_full := true.B }
    ramIn.b.valid := w_full

    when(ramIn.aw.fire) {
      windowMem.write(w_addr, wdata)
    }
    // generate simple state machine to handle address generation for window ram
    val sIdle :: sProcess :: Nil = Enum(2)
    val state = RegInit(sIdle)
    val state_next = Wire(state.cloneType)
    state_next := state

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

    val bposWin = (params.protoWin match {
      case fp: FixedPoint => fp.binaryPoint.get
      case _ => 0
    })

    val dirName = params.dirName
    val dir = new File(dirName)
    if (!(dir.exists() && dir.isDirectory()))
      dir.mkdir()

    val file = new File(params.memoryFile)

    val w = new BufferedWriter(new FileWriter(file))
    val windowSeqShifted = windowSeq.map(c => c * scala.math.pow(2, bposWin).toInt)

    for (i <- windowSeq.indices) {
      w.write(f"${windowSeqShifted(i).toInt}%02x" + "\n")
    }
    if (params.memoryFile.trim().nonEmpty) {
      loadMemoryFromFileInline(windowMem, params.memoryFile)
    }
    w.close()

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
      r_addr_reg := r_addr_reg + 1.U
    }
    when(r_addr_reg === (numPoints - 1.U) && ins(0).fire) {
      r_addr_reg := 0.U
    }

    val winCoeff =
      windowMem(r_addr_reg) //if (params.constWindow == true) windowROM(address_rom) else windowMem(r_addr_reg)

    // Control registers
    val fftSize =
      RegInit(
        params.numPoints.U(log2Ceil(params.numPoints + 1).W)
      ) // default value is equal to compile time parameter for fft size
    val fftDir = RegInit(true.B)
    val enableWind = RegInit(false.B)

    if (params.runTime)
      numPoints := fftSize
    else
      numPoints := params.numPoints.U

    // Define register fields
    val fields = Seq(
      // settable registers
      RegField(
        log2Ceil(params.numPoints),
        fftSize,
        RegFieldDesc(name = "fftSize", desc = "contains fft size which is used for run time configurability control")
      ),
      RegField(1, enableWind, RegFieldDesc(name = "enableWin", desc = "enable or disable windowing")),
      RegField(1, fftDir, RegFieldDesc(name = "fftDir", desc = "transform direction: fft or ifft"))
    )
    axiRegSlaveNode.regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f) }): _*)

    for ((in, inIdx) <- ins.zipWithIndex) {
      // TODO: Enable all datawidths not only 16
      val inComplex = RegNext(in.bits.data.asTypeOf(params.protoIQ))
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
        outs(inIdx).bits.data := windowedInput.asUInt
        outs(inIdx).bits.last := in.bits.last
      } else {
        val queueDelay = numMulPipes + 2
        val inputsDelay = numMulPipes + 1
        val queueData = Module(new Queue(params.protoIQ.cloneType, queueDelay, flow = true)) // + 1 for input delaying
        queueData.io.enq.bits := windowedInput
        queueData.io.enq.valid := ShiftRegister(in.valid && in.ready, inputsDelay, true.B)
        queueData.io.deq.ready := outs(inIdx).ready

        val queueLast = Module(new Queue(Bool(), queueDelay, flow = true)) // +1 for input delaying
        queueLast.io.enq.valid := ShiftRegister(in.valid && in.ready, inputsDelay, true.B) // +1 for input delaying
        queueLast.io.enq.bits := ShiftRegister(in.bits.last, inputsDelay, true.B) // +1 for input delaying
        queueLast.io.deq.ready := outs(inIdx).ready

        // Connect output
        outs(inIdx).valid := queueData.io.deq.valid
        outs(inIdx).bits.data := queueData.io.deq.bits.asUInt
        outs(inIdx).bits.last := queueLast.io.deq.bits
      }
      in.ready := outs(inIdx).ready
    }
  }
}

object AXI4WindowingBlockRAMMultipleInOutsApp extends App {
  val paramsWindowing = WindowingParams.fixed(
    dataWidth = 16,
    binPoint = 14,
    numMulPipes = 1,
    trimType = Convergent,
    dirName = "test_run_dir",
    memoryFile = "./test_run_dir/blacman.txt",
    windowFunc = WindowFunctionTypes.Blackman(dataWidth_tmp = 16)
  )
  implicit val p: Parameters = Parameters.empty

  val testModule = LazyModule(
    new AXI4WindowingBlockRAMMultipleInOuts(
      csrAddress = AddressSet(0x010000, 0xff),
      ramAddress = AddressSet(0x000000, 0x0fff),
      paramsWindowing,
      beatBytes = 4
    ) with WindowingRAMMultipleInOutsStandaloneBlock {
      override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    }
  )
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => testModule.module)))
}
