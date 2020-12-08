// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util._
import chisel3.experimental._

import dsptools._
import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import chisel3.iotesters.Driver
import chisel3.iotesters.PeekPokeTester
import scala.math.{Pi, pow}

import breeze.math.Complex
import breeze.linalg._
import breeze.plot._

import java.io._

import org.scalatest.{FlatSpec, Matchers}

class FFTWithMemTester(
  dut: FFTWithMem[FixedPoint],
  fftAddress: AddressSet,
  memAddress: AddressSet,
  beatBytes: Int
) extends PeekPokeTester(dut.module) with AXI4MasterModel  {

  override def memAXI: AXI4Bundle = dut.ioMem.get.getWrappedValue
  poke(dut.outStream.ready, 1)
  
  step(1)
  memWriteWord(memAddress.base, 0x000001)
  
  var outSeq = Seq[Int]()
  var peekedVal: BigInt = 0
  var realSeq = Seq[Int]()
  var imagSeq = Seq[Int]()
  var tmpReal: Short = 0
  var tmpImag: Short = 0

  // check only one fft window 
  while (outSeq.length < 1024) {
    if (peek(dut.outStream.valid) == 1 && peek(dut.outStream.ready) == 1) {
      peekedVal = peek(dut.outStream.bits.data)
      outSeq = outSeq :+ peekedVal.toInt
      tmpReal = (peekedVal.toInt / pow(2,16)).toShort
      tmpImag = (peekedVal.toInt - (tmpReal.toInt * pow(2,16))).toShort
      realSeq = realSeq :+ tmpReal.toInt
      imagSeq = imagSeq :+ tmpImag.toInt
    }
    step(1)
  }
  
  val complexOut = realSeq.zip(imagSeq).map { case (real, imag) => Complex(real, imag) }
  val dB = false
  
  val f = Figure()
  val p = f.subplot(0)
  p.legend_=(true)
  val xaxis = (0 until complexOut.length).map(e => e.toDouble).toSeq.toArray

  // Log 0!
  val plotMin = 0.0000000001
  
  val chiselPlot = complexOut.map(c => if (dB) 20 * math.log10(Seq(c.abs, plotMin).max) else Seq(c.abs, plotMin).max).toSeq
 
  p += plot(xaxis, chiselPlot.toArray, name = "Chisel fft")
  
  p.ylim(Seq(-100.0, chiselPlot.min).max, chiselPlot.max)
  p.title_=(s"Amplitude spectrum of FFT for fft lenght ${complexOut.length}")

  p.xlabel = "Frequency Bin"
  p.ylabel = "Amplitude"//"20log10(||Vpeak||)"
  f.saveas(s"test_run_dir/fft_${complexOut.length}.pdf")
  
  val pwReal = new PrintWriter(new File("resultReal.txt"))
  realSeq.foreach(x => pwReal.println(x.toDouble.toString))
  val pwImag = new PrintWriter(new File("resultImag.txt"))
  imagSeq.foreach(x => pwImag.println(x.toDouble.toString))
  pwReal.close
  pwImag.close
  
  
  // plot fft 
  //plot_fft(complexOut, complexScala, dB = true)
  
  step(1000)
}


class FFTWithMemSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

    val fftParams = FFTParams.fixed(
      dataWidth = 16,
      binPoint  = 0,
      twiddleWidth = 16,
      numPoints = 1024,
      useBitReverse  = false,
      runTime = true,
      numAddPipes = 1,
      numMulPipes = 1,
      expandLogic = Array.fill(log2Up(1024))(0),
      keepMSBorLSB = Array.fill(log2Up(1024))(true),
      minSRAMdepth = 1024 // just do not use ShiftRegisterMem
    )
   val fftAddress      = AddressSet(0x60000100, 0xFF)
   val fftRAM          = AddressSet(0x60002000, 0xFFF)
   val memAddress      = AddressSet(0x60003000, 0xF)
   val  protoMem        = FixedPoint(16.W, 0.BP)
   val  beatBytes         = 4
  
    
  it should "test fft with simple rom" in {
    val lazyDut = LazyModule(new FFTWithMem(fftParams, fftAddress, fftRAM, memAddress, protoMem, beatBytes) )
    chisel3.iotesters.Driver.execute(Array("-tiwv", "-tbn", "verilator", "-tivsuv"), () => lazyDut.module) {
      c => new FFTWithMemTester(lazyDut, fftAddress, memAddress, beatBytes)
    } should be (true)
  }
}

