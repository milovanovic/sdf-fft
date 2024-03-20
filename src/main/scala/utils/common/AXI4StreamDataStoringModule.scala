package dspblocks.utils.common

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import org.chipsalliance.cde.config.Parameters

abstract class DataStoringModule[D, U, E, O, B <: Data](beatBytes: Int, totalData: Int, triggerIn: Boolean)
    extends LazyModule()(Parameters.empty)
    with DspBlock[D, U, E, O, B]
    with HasCSR {

  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    val trigger = if (triggerIn) Some(IO(Input(Bool()))) else None
    val triggerOut = if (triggerIn) Some(IO(Output(Bool()))) else None
    val triggerHold = RegInit(false.B)

    when(trigger.getOrElse(true.B)) {
      triggerHold := true.B
    }

    val triggerReg = RegNext(triggerHold)

    if (triggerIn) {
      triggerOut.get := triggerReg
    }

    // Control registers
    val expectedData = RegInit(totalData.U(log2Up(totalData + 1).W))
    val cntIn = RegInit(0.U(log2Up(totalData).W))
    val cntOut = RegInit(0.U(log2Up(totalData).W))
    val ram = SyncReadMem(totalData, UInt(32.W)) // this is custom memory and used to stream radar data

    val sIdle :: sStore :: sLoad :: Nil = Enum(3)
    val state = RegInit(sIdle)
    val state_next = Wire(state.cloneType)
    state_next := state

    when(state_next === sIdle) {
      triggerHold := false.B
    }

    switch(state) {
      is(sIdle) {
        when(in.fire()) {
          state_next := sStore
        }
      }
      is(sStore) {
        when(cntIn === (expectedData - 1.U)) { // this means that all data are written
          state_next := sLoad
        }
      }
      is(sLoad) {
        //when (cntOut === (expectedData - 1.U) && out.fire()) {
        when(cntOut === (expectedData) && out.fire()) {
          when(in.fire()) {
            state_next := sStore
          }.otherwise {
            state_next := sIdle
          }
        }
      }
    }

    state := state_next

    //when (in.fire() && state =/= sLoad) { // i am not sure is this the righ
    when(in.fire() && RegNext(state =/= sLoad)) {
      ram(cntIn) := in.bits.data.asUInt
      cntIn := cntIn + 1.U
    }

    when(out.ready && state === sLoad && triggerReg) {
      //   when (out.fire()) {
      cntOut := cntOut + 1.U
    }
      .elsewhen(cntOut === expectedData && out.fire()) {
        cntOut := 0.U
      }
    val readAddress = Wire(cntOut.cloneType)
    readAddress := cntOut
    val readData = ram(readAddress)

    /* out.bits.data := readData
      out.valid := RegNext(state === sLoad, false.B)*/
    in.ready := RegNext(state =/= sLoad) // be carefull with this one

    val outQueue =
      Module(
        new Queue(chiselTypeOf(readData), entries = 1, pipe = false, flow = true)
      ) // prev flow on true and pipe on true
    outQueue.io.enq.bits := readData
    outQueue.io.enq.valid := RegNext(
      out.ready && triggerReg,
      false.B
    ) && state === sLoad //RegNext(state === sLoad && out.ready && triggerReg, false.B) // RegNext(state === sLoad && out.ready, false.B)
    outQueue.io.deq.ready := out.ready && triggerReg

    out.bits.data := outQueue.io.deq.bits
    out.valid := outQueue.io.deq.valid

    // Define register fields
    val fields = Seq(
      RegField(
        log2Up(totalData + 1),
        expectedData,
        RegFieldDesc(
          name = "total_data",
          desc = "Define number of data that should be considered in the process of reading and writing"
        )
      )
    )

    // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f) }): _*)
  }
}

class AXI4StreamDataStoringModule(
  address:   AddressSet,
  beatBytes: Int = 4,
  totalData: Int,
  triggerIn: Boolean = false
)(
  implicit p: Parameters)
    extends DataStoringModule[
      AXI4MasterPortParameters,
      AXI4SlavePortParameters,
      AXI4EdgeParameters,
      AXI4EdgeParameters,
      AXI4Bundle
    ](beatBytes, totalData, triggerIn)
    with AXI4DspBlock
    with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}

trait AXI4StreamDataStoringPins extends AXI4StreamDataStoringModule {
  def numBytes: Int = 4
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

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = numBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(
    AXI4StreamMasterParameters(n = numBytes)
  ) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

object AXI4StreamDataStoringApp extends App {
  implicit val p: Parameters = Parameters.empty
  val totalData = 512 * 64
  val lazyDut = LazyModule(
    new AXI4StreamDataStoringModule(AddressSet(0x00, 0xf), beatBytes = 4, totalData, true)
      with AXI4StreamDataStoringPins { override def numBytes = 4 }
  )
  (new ChiselStage).execute(
    Array("--target-dir", "verilog/AXI4StreamDataStoring"),
    Seq(ChiselGeneratorAnnotation(() => lazyDut.module))
  )
}
