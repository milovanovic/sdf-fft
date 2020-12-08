// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util._
import dsptools._
import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

 class FFTBlockWithWindowing [T <: Data : Real: BinaryRepresentation] (csrAddress: AddressSet, ramAddress: AddressSet, val params: FFTParams[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with AXI4DspBlock {
  require(params.windowFunc == WindowFunctionTypes.None(), "Parameter for window function inside SDFFFT module must be set to None type")
  
  val streamNode = AXI4StreamIdentityNode()
  val mem = Some(AXI4IdentityNode())
  val axiRegSlaveNode = AXI4RegisterNode(address = csrAddress, beatBytes = beatBytes) // AXI4 Register
 
  val ramSlaveNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
  Seq(AXI4SlaveParameters(
    address       = Seq(ramAddress),
    supportsRead  = TransferSizes(1, beatBytes),
    supportsWrite = TransferSizes(1, beatBytes),
    interleavedId = Some(0))),
  beatBytes  = beatBytes,
  minLatency = 1)))
  
  // Define AXI4 Xbar
  val topXbar = AXI4Xbar()
  
  ramSlaveNode    := topXbar
  axiRegSlaveNode := topXbar
  topXbar         := mem.get
  
  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)
    
    val (ramIn, ramInEdge) = ramSlaveNode.in.head
    val windowMem = SyncReadMem(params.numPoints, params.protoWin)
    val r_addr_reg = RegInit(1.U((log2Ceil(params.numPoints)).W))
    //val r_addr     = Wire(r_addr_reg.cloneType)
    
    val numPoints = Wire(UInt((log2Ceil(params.numPoints)).W)) // be careful not saved in register 
    

    val w_full = RegInit(false.B)
    val wdata = ramIn.w.bits.data.asTypeOf(params.protoWin)
    
    // take only appropriate number of LSB bits
    val w_addr = (ramIn.aw.bits.addr(log2Ceil(params.numPoints) - 1 + log2Ceil(beatBytes), 0) >> log2Ceil(beatBytes)).asTypeOf(r_addr_reg)
    
    ramIn.aw.ready := ramIn. w.valid && (ramIn.b.ready || !w_full)
    ramIn.w.ready := ramIn.aw.valid && (ramIn.b.ready || !w_full)
   
    when (ramIn. b.fire()) { w_full := false.B }
    when (ramIn.aw.fire()) { w_full := true.B }
    ramIn. b.valid := w_full
    
    when (ramIn.aw.fire()) {
      windowMem.write(w_addr, wdata)
    }
    
    // generate simple state machine to handle address generation for window ram
    val sIdle :: sProcess :: Nil = Enum(2)
    val state = RegInit(sIdle)
    val state_next = Wire(state.cloneType)
    state_next := state 
    
    switch(state) {
      is (sIdle) {
        when(in.fire()) {
          state_next := sProcess
        }
      }
      is (sProcess) {
        when(in.bits.last) {
          state_next := sIdle
        }
      }
    }
    state := state_next
    
    when (in.fire()) {
      r_addr_reg := r_addr_reg + 1.U
    }
    when (in.bits.last || r_addr_reg === (numPoints - 1.U)) {
      r_addr_reg := 1.U
    }
    val r_addr = Mux(state === sIdle && state_next =/= sProcess, 0.U, r_addr_reg)
    val winCoeff = windowMem(r_addr)
    
    //  FFT module
    val fft = Module(new SDFFFT(params))

    // Number of stages
    val numStages = log2Ceil(params.numPoints)
    
    // Control registers
    val fftSize         = RegInit(numStages.U(log2Ceil(numStages + 1).W)) // default value is equal to compile time parameter for fft size
    val fftDir          = RegInit(true.B)
    val keepMSBorLSBReg = RegInit(0.U((numStages).W))
    val doWindowing     = RegInit(false.B)
    // Status registers
    val busy            = RegInit(false.B)
    val overflowReg     = RegInit(0.U(log2Ceil(numStages).W))

    if (params.runTime == true)
      numPoints := (2.U << (fftSize - 1.U))
    else 
      numPoints := params.numPoints.U
    
    // Connect FFT signals to registers
    busy := fft.io.busy
    if (params.runTime) {
      fft.io.fftSize.get := fftSize
    }
    if (params.keepMSBorLSBReg) {
      fft.io.keepMSBorLSBReg.get := keepMSBorLSBReg.asBools
    }
    if (params.fftDirReg) {
      fft.io.fftDirReg.get := fftDir
    }
    if (params.overflowReg) {
      overflowReg := fft.io.overflow.get.asUInt
    }

      // Define register fields
    val fields = Seq(
      // settable registers
      RegField(log2Ceil(numStages), fftSize,
        RegFieldDesc(name = "fftSize", desc = "contains fft size which is used for run time configurability control")),
      RegField(1, fftDir, 
        RegFieldDesc(name = "fftDir", desc = "transform direction: fft or ifft")),
      RegField(1, doWindowing,
        RegFieldDesc(name = "doWindowing", desc = "enable or disable windowing")),
      //RegField(1, flushData,
        //RegFieldDesc(name = "flushData", desc = "trigger flushing")),
      RegField(numStages, keepMSBorLSBReg,
        RegFieldDesc(name = "keepMSBorLSBReg", desc = "defines scaling behaviour for each stage")),
      // read-only status registers
      RegField.r(1, busy, 
        RegFieldDesc(name = "busy", desc = "indicates if fft core is in the state flush data")),
      RegField.r(log2Ceil(numStages), overflowReg,
        RegFieldDesc(name = "overflowReg", desc = "returns overflow status for the each stage"))
    )

    axiRegSlaveNode.regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
  
    // Connect inputs
    fft.io.in.valid   := in.valid
    
    /********************** assign input data  ***********************/
    //fft.io.in.bits    := in.bits.data.asTypeOf(params.protoIQ) * winCoeffComplex
    val inComplex = in.bits.data.asTypeOf(params.protoIQ)
    when (doWindowing) {
      fft.io.in.bits.real := inComplex.real * winCoeff
      fft.io.in.bits.imag := inComplex.imag * winCoeff
    }
    .otherwise {
      fft.io.in.bits := in.bits.data.asTypeOf(params.protoIQ)
    }
    /****************************************************************/
    // 
    in.ready          := fft.io.in.ready
    fft.io.lastIn     := in.bits.last
    

    // Connect output
    out.valid        := fft.io.out.valid
    fft.io.out.ready := out.ready
    out.bits.data    := fft.io.out.bits.asUInt
    out.bits.last    := fft.io.lastOut
     // It is not necessary to connect these signals
    // out.bits.id   := 0.U
    // out.bits.dest := 0.U
  }
}


object FFTBlockWithWindowingApp extends App
{
  val paramsFFT = FFTParams.fixed(
    dataWidth = 16,
    twiddleWidth = 16,
    numPoints = 1024,
    runTime = true,
    numAddPipes = 1,
    numMulPipes = 1,
    expandLogic = Array.fill(log2Up(1024))(0),
    keepMSBorLSB = Array.fill(log2Up(1024))(true),
    overflowReg = true,
    keepMSBorLSBReg = true,
    binPoint = 1
  )
  
  implicit val p: Parameters = Parameters.empty
  val testModule = LazyModule(new FFTBlockWithWindowing(csrAddress = AddressSet(0x010000, 0xFF), ramAddress = AddressSet(0x000000, 0x0FFF), paramsFFT, beatBytes = 4) with AXI4StandaloneBlock {
    override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  })
  chisel3.Driver.execute(args, ()=> testModule.module)
}
