package dspblocks.sdffft

import chisel3.{fromDoubleToLiteral => _, fromIntToBinaryPoint => _, _}
import fixedpoint._
import chisel3.experimental.DataMirror

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.{Config, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.BaseSubsystem

import dsptools.numbers._

/* FFT parameters and addresses */
case class FFTParamsAndAddress[T <: Data: Real: BinaryRepresentation](
  fftParams:  FFTParams[T],
  fftAddress: AddressSet,
  useAXI4:    Boolean)

/* AXI4SDFFFT FixedPoint Key */
case object SDFFFTKey extends Field[Option[FFTParamsAndAddress[FixedPoint]]](None)

trait CanHavePeripherySDFFFT { this: BaseSubsystem =>
  private val portName = "fft"

  val fft = p(SDFFFTKey) match {
    case Some(params) => {
      val fft = if (params.useAXI4) {
        val fft = LazyModule(
          new AXI4FFTBlock(
            address = params.fftAddress,
            params = params.fftParams,
            _beatBytes = pbus.beatBytes,
            configInterface = false
          )
        )
        // Connect mem
        pbus.coupleTo("fft") {
          fft.mem.get := AXI4Buffer() := TLToAXI4() := TLFragmenter(
            pbus.beatBytes,
            pbus.blockBytes,
            holdFirstDeny = true
          ) := _
        }
        // return
        Some(fft)
      } else {
        val fft = LazyModule(
          new TLFFTBlock(
            address = params.fftAddress,
            params = params.fftParams,
            beatBytes = pbus.beatBytes,
            configInterface = false
          )
        )
        // Connect mem
        pbus.coupleTo("fft") { fft.mem.get := TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _ }
        // return
        Some(fft)
      }
      // streamNode
      val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = pbus.beatBytes)))
      val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

      ioOutNode := AXI4StreamToBundleBridge(
        AXI4StreamSlaveParameters()
      ) := fft.get.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = pbus.beatBytes)) := ioInNode

      val fft_in = InModuleBody { ioInNode.makeIO() }
      val fft_out = InModuleBody { ioOutNode.makeIO() }

      // return
      Some(Seq(fft_in, fft_out))
    }
    case None => None
  }
}

trait CanHavePeripherySDFFFTModuleImp extends LazyModuleImp {
  val outer: CanHavePeripherySDFFFT
}

class SDFFFTIO[T <: Data](private val gen1: T, private val gen2: T) extends Bundle {
  val in = DataMirror.internal.chiselTypeClone[T](gen1)
  val out = Flipped(DataMirror.internal.chiselTypeClone[T](gen2))
}

/* Mixin to add AXI4SDFFFT to rocket config */
class WithSDFFFT(fftParams: FFTParams[FixedPoint], fftAddress: AddressSet = AddressSet(0x2000, 0xff), useAXI4: Boolean)
    extends Config((site, here, up) => {
      case SDFFFTKey =>
        Some(
          (FFTParamsAndAddress(
            fftParams = fftParams,
            fftAddress = fftAddress,
            useAXI4 = useAXI4
          ))
        )
    })

case object SDFFFTAdapter {
  def tieoff(fft: Option[SDFFFTIO[AXI4StreamBundle]]): Unit = {
    fft.foreach { s =>
      s.in.valid := false.B
      s.in.bits := DontCare
      s.out.ready := true.B
    }
  }

  def tieoff(fft: SDFFFTIO[AXI4StreamBundle]): Unit = { tieoff(Some(fft)) }
}
