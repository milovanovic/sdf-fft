package fft

import chisel3._
import chisel3.experimental._
import chisel3.util._

import dspblocks._
import dsptools._
import dsptools.numbers._
import dspjunctions._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

import breeze.math.Complex

abstract class MemForTestingFFT[T <: Data : Real : BinaryRepresentation, D, U, E, O, B <: Data]
(val proto: DspComplex[T], beatBytes: Int, memSize: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {
  
  val masterParams = AXI4StreamMasterParameters(
		name = "AXI4 Stream memory",
		n = 4,
		u = 0,
		numMasters = 1
	)
  val streamNode = AXI4StreamMasterNode(masterParams)

  lazy val module = new LazyModuleImp(this) {
    val (out, _) = streamNode.out(0)
    
    val startReading = RegInit(false.B)
    val runLast = RegInit(false.B)
    val cntr = RegInit(0.U(log2Up(memSize).W))

    val rom = Wire(Vec(memSize, proto.cloneType))
    
    val f1 = 32/memSize
    // fill memory with real data only
    val realSin = (0 until memSize).map(i => Complex(((math.sin(2 * math.Pi * f1 * i))*(math.pow(2,14)-1)), 0)).toVector
    
    (0 until 1024).map(n => {
      rom(n).real := Real[T].fromDouble(realSin(n).real)
      rom(n).imag := Real[T].fromDouble(realSin(n).imag)
    })
    
    val rstProtoIQ = Wire(proto.cloneType)
    rstProtoIQ.real:= Real[T].fromDouble(0.0)
    rstProtoIQ.imag:= Real[T].fromDouble(0.0)
    val outData = RegInit(rstProtoIQ)

    when (startReading) {
      cntr := cntr + 1.U
    }
    outData := rom(cntr)
    
    out.valid := RegNext(startReading)
    out.bits.data := outData.asUInt
    out.bits.last := runLast
    
    val fields = Seq(
    // settable registers
    RegField(1, startReading,
      RegFieldDesc(name = "startReading", desc = "enable reading from the memory")),
    RegField(1, runLast, 
      RegFieldDesc(name = "runLast", desc = "set last signal"))
    )
    
    regmap(
      fields.zipWithIndex.map({ case (f, i) =>
        i * beatBytes -> Seq(f)
      }): _*
    )
  }
}

class AXI4MemForTestingFFT[T <: Data : Real: BinaryRepresentation](proto: DspComplex[T], address: AddressSet, beatBytes: Int = 4, memSize: Int = 1024)(implicit p: Parameters) extends MemForTestingFFT[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](proto, beatBytes, memSize) with AXI4DspBlock with AXI4HasCSR {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}


object MemForTestingBlock extends App
{
	trait AXI4Block extends DspBlock[
    AXI4MasterPortParameters,
    AXI4SlavePortParameters,
    AXI4EdgeParameters,
    AXI4EdgeParameters,
    AXI4Bundle] {
    def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    val ioMem = mem.map { m => {
      val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

      m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

      val ioMem = InModuleBody { ioMemNode.makeIO() }
      ioMem
    }}

    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode :=
      AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
      streamNode

    val out = InModuleBody { ioOutNode.makeIO() }
	}
	implicit val p: Parameters = Parameters.empty
	
	val memTest = LazyModule(new AXI4MemForTestingFFT(DspComplex(FixedPoint(16.W, 0.BP)), AddressSet(0x000000, 0xFF), beatBytes = 4, memSize = 1024) with AXI4Block {
		override def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
	})
	chisel3.Driver.execute(args, ()=> memTest.module)
}

