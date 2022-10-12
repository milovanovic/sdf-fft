// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.config.{Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem.BaseSubsystem

import dsptools.numbers._

/* FFT parameters and addresses */
case class FFTParamsAndAddress[T <: Data: Real: BinaryRepresentation] (
  fftParams  : FFTParams[T],
  fftAddress : AddressSet
)

/* AXI4SDFFFT FixedPoint Key */
case object AXI4SDFFFTKey extends Field[Option[FFTParamsAndAddress[FixedPoint]]](None)

trait CanHavePeripheryAXI4SDFFFT { this: BaseSubsystem =>
  private val portName = "fft"

  // Only build if we are using the TL (nonAXI4) version
  val fft = p(AXI4SDFFFTKey) match {
    case Some(params) => {
      val fft = LazyModule(new AXI4FFTBlock(address = params.fftAddress, params = params.fftParams, _beatBytes = pbus.beatBytes, configInterface = false))

      pbus.toSlave(Some(portName)) {
        // toVariableWidthSlave doesn't use holdFirstDeny, which TLToAXI4() needsx
        fft.mem.get := AXI4Buffer () := TLToAXI4 () := TLFragmenter(pbus.beatBytes, pbus.blockBytes, holdFirstDeny = true)
      }
      // streamNode
      val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = pbus.beatBytes)))
      val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

      ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := fft.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = pbus.beatBytes)) := ioInNode

      val fft_in = InModuleBody { ioInNode.makeIO() }
      val fft_out = InModuleBody { ioOutNode.makeIO() }
      
      // return
      Some(Seq(fft_in, fft_out))
    }
    case None => None
  }
}

trait CanHavePeripheryAXI4SDFFFTModuleImp extends LazyModuleImp{
  val outer: CanHavePeripheryAXI4SDFFFT
}

class AXI4SDFFFTIO[T <: Data](private val gen1: T, private val gen2: T) extends Bundle {
  val in = DataMirror.internal.chiselTypeClone[T](gen1)
  val out = Flipped(DataMirror.internal.chiselTypeClone[T](gen2))
}

/* Mixin to add AXI4SDFFFT to rocket config */
class WithAXI4SDFFFT (fftParams : FFTParams[FixedPoint], fftAddress: AddressSet = AddressSet(0x2000, 0xFF)) extends Config((site, here, up) => {
  case AXI4SDFFFTKey => Some((FFTParamsAndAddress(
    fftParams  = fftParams,
    fftAddress = fftAddress
  )))
})


case object AXI4SDFFFTAdapter {
  def tieoff(fft: Option[AXI4SDFFFTIO[AXI4StreamBundle]]) {
    fft.foreach { s =>
      s.in.valid := false.B
      s.in.bits := DontCare
      s.out.ready := true.B
    }
  }

  def tieoff(fft: AXI4SDFFFTIO[AXI4StreamBundle]) { tieoff(Some(fft)) }
}