// SPDX-License-Identifier: Apache-2.0

package fft

import dsptools._
import dsptools.numbers._
import org.scalatest.{FlatSpec, Matchers}

import breeze.math.Complex
import breeze.signal.{fourierTr, iFourierTr}
import breeze.linalg._//{DenseVector, randomDouble}
import breeze.plot._

import chisel3.util.log2Up
import scala.util.Random

/**
* Should run simple FFT testers and shows functionality of the core without going into details..
* Running this test, various plot diagrams are generated and saved in "test_run_dir" directory
*/

class Radix2PlotSpec extends FlatSpec with Matchers {
  
  it should f"compute and plot radix 2 fft of the size 1024, no growing logic and FixedPoint data type " in {//ignore {
    val paramsFixed = FFTParams.fixed(
    numPoints = 1024,
    dataWidth = 16,
    numAddPipes = 0,//1
    numMulPipes = 0,
    binPoint = 14,
    decimType = DITDecimType,
    expandLogic = Array.fill(log2Up(1024))(0),
    keepMSBorLSB = Array.fill(log2Up(1024))(true), // divide by 2
    trimType = RoundHalfUp,
    sdfRadix = "2")

    dsptools.Driver.execute(
      () => new SDFFFT(paramsFixed), Array("-tbn", "treadle")){ c =>
      new FixedPointSDFFFTTester(c) {
        val testSignal = getTone(numSamples = 1024, f1r = 0.03125)
        val (chiselFFT, scalaFFT) = this.simpleTestFFT(2, testSignal, paramsFixed)
        val f = Figure()
        val p_sin = f.subplot(0)
        p_sin.legend_=(true)
        val xaxis = (0 until testSignal.length).map(e => e.toDouble).toSeq.toArray //check this!
        val testSignalReal = testSignal.map(e => e.real)

        p_sin += plot(xaxis, testSignalReal.toArray, name = "sin(2*pi*(32/1024)*n)")
        p_sin.title_= (s"Sinusoidal test signal")
        p_sin.xlabel = "n"
        p_sin.ylabel = "y[n]"

        val p_fft = f.subplot(2,1,1)
        p_fft.legend_=(true)
        val chiselPlot = chiselFFT.map(c => c.abs).toSeq
        val scalaPlot = scalaFFT.map(c => c.abs).toSeq

        p_fft += plot(xaxis, chiselPlot.toArray, '.', name = "Chisel fft")
        p_fft += plot(xaxis, scalaPlot.toArray, '-', name = "Scala fft")

        p_fft.ylim(Seq(-100.0, chiselPlot.min).max, chiselPlot.max + 10)
        p_fft.title_=(s"Amplitude spectrum of FFT for fft lenght ${chiselFFT.length}")

        p_fft.xlabel = "Frequency Bin"
        p_fft.ylabel = "Magnitude"//"20log10(||Vpeak||)"
        f.saveas(s"test_run_dir/FFT_sin_1024_radix2.pdf")
      }
    } should be (true)
 }
  it should f"compute and plot radix 2 fft of the size 1024, no growing logic and FixedPoint data type (test signal with noise)" in {//in {
    val paramsFixed = FFTParams.fixed(
      numPoints = 1024,
      dataWidth = 16,
      numAddPipes = 1,
      numMulPipes = 0,
      binPoint = 14,
      decimType = DITDecimType,
      expandLogic = Array.fill(log2Up(1024))(0),
      keepMSBorLSB = Array.fill(log2Up(1024))(true), // divide by 2
      trimType = RoundHalfUp,
      sdfRadix = "2")
    
    dsptools.Driver.execute(
      () => new SDFFFT(paramsFixed), Array("-tbn", "treadle")){ c =>
      new FixedPointSDFFFTTester(c) {
        val testSignal = getTone(numSamples = 1024, f1r = 0.03125, addNoise = 0.4)
        val (chiselFFT, scalaFFT) = this.simpleTestFFT(3, testSignal, paramsFixed)
        val f = Figure()
        val p_sin = f.subplot(0)
        p_sin.legend_=(true)
        val xaxis = (0 until testSignal.length).map(e => e.toDouble).toSeq.toArray
        val testSignalReal = testSignal.map(e => e.real)

        p_sin += plot(xaxis, testSignalReal.toArray, name = "sin(2*pi*(32/1024)*n) + noise")
        p_sin.title_= (s"Sinusoidal test signal")
        p_sin.xlabel = "n"
        p_sin.ylabel = "y[n]"

        val p_fft = f.subplot(2,1,1)
        p_fft.legend_=(true)
        val chiselPlot = chiselFFT.map(c => c.abs).toSeq
        val scalaPlot = scalaFFT.map(c => c.abs).toSeq

        p_fft += plot(xaxis, chiselPlot.toArray, '.', name = "Chisel fft")
        p_fft += plot(xaxis, scalaPlot.toArray, '-', name = "Scala fft")

        p_fft.ylim(chiselPlot.min, chiselPlot.max + 10)
        p_fft.title_=(s"Amplitude spectrum of FFT for fft lenght ${chiselFFT.length}")

        p_fft.xlabel = "Frequency Bin"
        p_fft.ylabel = "Magnitude"//"20log10(||Vpeak||)"
        f.saveas(s"test_run_dir/FFT_sin_1024_radix2_noisy.pdf")
      }
    } should be (true)
  }
  
  it should f"compute and plot radix 2 fft/ifft of the size 512, no growing logic and FixedPoint data type " in {//in {
    val paramsFixed = FFTParams.fixed(
    numPoints = 512,
    dataWidth = 16,
    numAddPipes = 1,
    numMulPipes = 0,
    binPoint = 14,
    decimType = DIFDecimType,
    expandLogic = Array.fill(log2Up(512))(0),
    keepMSBorLSB = Array.fill(log2Up(512))(true), // divide by 2
    fftDirReg = true,
    trimType = RoundHalfUp,
    sdfRadix = "2")

    dsptools.Driver.execute(
      () => new SDFFFT(paramsFixed), Array("-tbn", "verilator")){ c =>
      new FixedPointSDFFFTTester(c) {
        val testSignal = getTone(numSamples = 512, f1r = 0.03125)
        val (chiselFFT, chiselIFFT) = this.testFFT_IFFT(2, testSignal, paramsFixed)
        val f = Figure()
        val p_sin = f.subplot(0)
        p_sin.legend_=(true)
        val xaxis = (0 until testSignal.length).map(e => e.toDouble).toSeq.toArray
        val testSignalReal = testSignal.map(e => e.real)

        p_sin += plot(xaxis, testSignalReal.toArray, name = "sin(2*pi*(16/512)*n)")
        p_sin.title_= (s"Sinusoidal test signal")
        p_sin.xlabel = "n"
        p_sin.ylabel = "y[n]"
        
        val scalaFFT = fourierTr(DenseVector(testSignal.toArray)).toScalaVector
        val p_fft = f.subplot(2,2,2)
        p_fft.legend_=(true)
        val chiselPlot = chiselFFT.map(c => c.abs).toSeq
        val scalaPlot = scalaFFT.map(c => c.abs).toSeq
        
        p_fft += plot(xaxis, chiselPlot.toArray, '.', name = "Chisel fft")
        p_fft += plot(xaxis, scalaPlot.toArray, '-', name = "Scala fft")
        p_fft.ylim(chiselPlot.min, chiselPlot.max + 10)
        p_fft.title_=(s"Amplitude spectrum of the FFT for fft size ${chiselFFT.length}")
        p_fft.xlabel = "Frequency Bin"
        p_fft.ylabel = "Magnitude"//"20log10(||Vpeak||)"
        
        val p_ifft_real = f.subplot(2,2,1)
        p_ifft_real.legend_=(true)
        //val xaxis = (0 until chiselIFFT.length).map(e => e.toDouble).toSeq.toArray

        val chiselPlotReal = chiselIFFT.map(e => e.real)
        val scalaPlotReal = testSignal.map(e => e.real)
        
        p_ifft_real += plot(xaxis, chiselPlotReal.toArray,'.', name = "Chisel ifft real part")
        p_ifft_real += plot(xaxis, scalaPlotReal.toArray,'-', name = "Scala ifft real part")
      
        //p.ylim(Seq(-100.0, chiselPlot.min).max, chiselPlot.max)
        p_ifft_real.title_= (s"Real part of the IFFT")

        p_ifft_real.xlabel = "n"
        p_ifft_real.ylabel = "y[n]"
        
        val p_ifft_imag = f.subplot(2,2,3)
        p_ifft_imag.legend_=(true)

        val chiselPlotImag = chiselIFFT.map(e => e.imag)
        val scalaPlotImag = testSignal.map(e => e.imag)
        
        p_ifft_imag.ylim(chiselPlotImag.min-1, chiselPlotImag.max+1)

        p_ifft_imag += plot(xaxis, chiselPlotImag.toArray, '.', name = "Chisel ifft imag part")
        p_ifft_imag += plot(xaxis, scalaPlotImag.toArray, '-', name = "Scala ifft imag part")
        p_ifft_imag.title_= (s"Imaginary part of the IFFT")

        p_ifft_imag.xlabel = "n"
        p_ifft_imag.ylabel = "y[n]"
        
        f.saveas(s"test_run_dir/FFT_IFFT_sin_512_radix2_FixedPoint.pdf")
      }
    } should be (true)
 }
  
  it should f"compute and plot radix 2 fft/ifft of the size 512 and no growing logic and DspReal data type " in {//ignore {
    val paramsFixed = FFTParams.DSPReal(
    numPoints = 512,
   // dataWidth = 16,//this parameter is not relevant for DSPReal testing
    numAddPipes = 1,
    numMulPipes = 1,
    binPoint = 0,
    decimType = DIFDecimType,
    expandLogic = Array.fill(log2Up(512))(0),
    keepMSBorLSB = Array.fill(log2Up(512))(true), // divide by 2
    fftDirReg = true,
    trimType = RoundHalfUp,
    sdfRadix = "2")

    dsptools.Driver.execute(
      () => new SDFFFT(paramsFixed), Array("-tbn", "verilator")){ c =>
      new DspRealSDFFFTTester(c) {
          val testSignal = getTone(numSamples = 512, f1r = 0.03125)
          val (chiselFFT, chiselIFFT) = this.testFFT_IFFT(2, testSignal, paramsFixed)
          val f = Figure()
          val p_sin = f.subplot(0)
          p_sin.legend_=(true)
            val xaxis = (0 until testSignal.length).map(e => e.toDouble).toSeq.toArray
          val testSignalReal = testSignal.map(e => e.real)

          p_sin += plot(xaxis, testSignalReal.toArray, name = "sin(2*pi*(16/512)*n)")
          p_sin.title_= (s"Sinusoidal test signal")
          p_sin.xlabel = "n"
          p_sin.ylabel = "y[n]"
          
          val scalaFFT = fourierTr(DenseVector(testSignal.toArray)).toScalaVector
          val p_fft = f.subplot(2,2,2)
          p_fft.legend_=(true)
          val chiselPlot = chiselFFT.map(c => c.abs).toSeq
          val scalaPlot = scalaFFT.map(c => c.abs).toSeq
          
          p_fft += plot(xaxis, chiselPlot.toArray, '.', name = "Chisel fft")
          p_fft += plot(xaxis, scalaPlot.toArray, '-', name = "Scala fft")
          p_fft.ylim(chiselPlot.min, chiselPlot.max + 10)
          p_fft.title_=(s"Amplitude spectrum of the FFT for fft size ${chiselFFT.length}")
          p_fft.xlabel = "Frequency Bin"
          p_fft.ylabel = "Magnitude"//"20log10(||Vpeak||)"
          
          val p_ifft_real = f.subplot(2,2,1)
          p_ifft_real.legend_=(true)
          
          val chiselPlotReal = chiselIFFT.map(e => e.real)
          val scalaPlotReal = testSignal.map(e => e.real)
          
          p_ifft_real += plot(xaxis, chiselPlotReal.toArray,'.', name = "Chisel ifft real part")
          p_ifft_real += plot(xaxis, scalaPlotReal.toArray,'-', name = "Scala ifft real part")
        
          //p.ylim(Seq(-100.0, chiselPlot.min).max, chiselPlot.max) // range ylim and xlim is not problem
          p_ifft_real.title_= (s"Real part of the IFFT")

          p_ifft_real.xlabel = "n"
          p_ifft_real.ylabel = "y[n]"
          
          val p_ifft_imag = f.subplot(2,2,3)
          p_ifft_imag.legend_=(true)

          val chiselPlotImag = chiselIFFT.map(e => e.imag)
          val scalaPlotImag = testSignal.map(e => e.imag)
          
          p_ifft_imag.ylim(chiselPlotImag.min-1, chiselPlotImag.max+1)

          p_ifft_imag += plot(xaxis, chiselPlotImag.toArray, '.', name = "Chisel ifft imag part")
          p_ifft_imag += plot(xaxis, scalaPlotImag.toArray, '-', name = "Scala ifft imag part")
          p_ifft_imag.title_= (s"Imaginary part of the IFFT")

          p_ifft_imag.xlabel = "n"
          p_ifft_imag.ylabel = "y[n]"
          
          f.saveas(s"test_run_dir/FFT_IFFT_sin_512_radix2_DspReal.pdf")
        }
    } should be (true)
  }
}

