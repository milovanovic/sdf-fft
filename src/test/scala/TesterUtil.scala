// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.util.log2Up
import dsptools.DspTester
import dsptools.numbers._
import breeze.math.Complex
import breeze.linalg._

/**
 * Contains useful helper functions for testers
 */
trait HasTesterUtil[T <: Module] extends DspTester[T] {

  /**
   * Waits to see if a signal is asserted. If more than the allowed number
   * of cycles is passed while waiting, fail the test.
   */
  def wait_for_assert(signal: Bool, maxCyclesWait: Int) {
    require(maxCyclesWait > 0, "maximum number of cycles to wait must be positive")
    var cyclesWaiting = 0
    while (!peek(signal) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      if (cyclesWaiting >= maxCyclesWait) { expect(false, "waited for input too long") }
      step(1)
    }
  }
  
  /**
   * Compare two complex sequences
   */
  def check_complex_seq(sig_vec: Seq[Complex], exp_seq: Seq[Complex]) {
    exp_seq.zipWithIndex.foreach { case (expected, index) => sig_vec(index).equals(expected) }
  }
}

trait HasSignalUtils {

//   /**
//    * Calculate signal to quantization noise ratio of the original scala fft function and chisel fft
//    */
//   def calc_sqnr(chiselFFT: Seq[Complex], scalaFFT: Seq[Complex]): Double = {
//     import breeze.signal._
//     import breeze.linalg._
//     import scala.math._
//
//     val signal  = scalaFFT.map(c => sqrt(pow(c.real,2) + pow(c.imag,2)))
//     val noise = chiselFFT.zip(scalaFFT).map { case (cFFT, sFFT) => sqrt(pow((cFFT - sFFT).real,2) + pow((cFFT - sFFT).imag,2)) }
//     val sumSignal = signal.foldLeft(0.0)(_+_) //divide by length not necessary
//     val noiseSum = noise.foldLeft(0.0)(_+_) //divide by length not necessary
//     20*math.log10(sumSignal/noiseSum)
//   }

  /**
  * Calculate signal to quantization noise ratio of the original scala fft function and chisel fft
  */
  def calc_sqnr(chiselFFT: Seq[Complex], scalaFFT: Seq[Complex]): Double = {
    import breeze.signal._
    import breeze.linalg._
    import scala.math._

    val scalaMagnitude = scalaFFT.map(c => sqrt(pow(c.real,2) + pow(c.imag,2)))
    val chiselMagnitude = chiselFFT.map(c => sqrt(pow(c.real,2) + pow(c.imag,2)))
    val noise = chiselMagnitude.zip(scalaMagnitude).map { case (cFFT, sFFT) => (pow(cFFT - sFFT,2)) }
    val signal = scalaMagnitude.map(c => pow(c, 2))
    val sumSignal = signal.foldLeft(0.0)(_+_) //divide by length not necessary
    val noiseSum = noise.foldLeft(0.0)(_+_) //divide by length not necessary
    10*math.log10(sumSignal/noiseSum)
  }
  
  /**
   * Plot chisel and scala fft in the same figure
   */
  def plot_fft (chiselFFT: Seq[Complex], scalaFFT: Seq[Complex], dB: Boolean = false): Unit = {
    import breeze.linalg._
    import breeze.plot._

    val f = Figure()
    val p = f.subplot(0)
    p.legend_=(true)
    val xaxis = (0 until chiselFFT.length).map(e => e.toDouble).toSeq.toArray

    // Log 0!
    val plotMin = 0.0000000001
    
    val chiselPlot = chiselFFT.map(c => if (dB) 20 * math.log10(Seq(c.abs, plotMin).max) else Seq(c.abs, plotMin).max).toSeq
    val scalaPlot = scalaFFT.map(c => if (dB) 20 * math.log10(Seq(c.abs, plotMin).max) else Seq(c.abs, plotMin).max).toSeq
    
    p += plot(xaxis, chiselPlot.toArray, name = "Chisel fft")
    p += plot(xaxis, scalaPlot.toArray, '.', name = "Scala fft")
   
    p.ylim(Seq(-100.0, chiselPlot.min).max, chiselPlot.max)
    p.title_=(s"Amplitude spectrum of FFT for fft lenght ${chiselFFT.length}")

    p.xlabel = "Frequency Bin"
    p.ylabel = "Amplitude"//"20log10(||Vpeak||)"
    f.saveas(s"test_run_dir/fft_${chiselFFT.length}.pdf")
  }
  /**
   * Plot chisel and scala ifft in the same figure
   */
  def plot_ifft (chiselIFFT: Seq[Complex], scalaIFFT: Seq[Complex]): Unit = {
    import breeze.linalg._
    import breeze.plot._

    val f = Figure()
    val p_real = f.subplot(0)
    p_real.legend_=(true)
    val xaxis = (0 until chiselIFFT.length).map(e => e.toDouble).toSeq.toArray

    val chiselPlotReal = chiselIFFT.map(e => e.real)
    val scalaPlotReal = scalaIFFT.map(e => e.real)
    
    p_real += plot(xaxis, chiselPlotReal.toArray, name = "Chisel ifft - real part")
    p_real += plot(xaxis, scalaPlotReal.toArray, '.', name = "Scala ifft - real part")
   
    //p.ylim(Seq(-100.0, chiselPlot.min).max, chiselPlot.max)
    p_real.title_= (s"Real part of the ifft - Chisel vs Scala")

    p_real.xlabel = "n"
    p_real.ylabel = "y[n]"
    
    val p_imag = f.subplot(2,1,1)
    p_imag.legend_=(true)

    val chiselPlotImag = chiselIFFT.map(e => e.imag)
    val scalaPlotImag = scalaIFFT.map(e => e.imag)
        
    p_imag += plot(xaxis, chiselPlotImag.toArray, name = "Chisel ifft - imag part")
    p_imag += plot(xaxis, scalaPlotImag.toArray, '.', name = "Scala ifft - imag part")
   
    //p.ylim(Seq(-100.0, chiselPlot.min).max, chiselPlot.max) // range ylim and xlim should not be problem i guess
    p_imag.title_= (s"Imag part of the ifft - Chisel vs Scala")

    p_imag.xlabel = "n"
    p_imag.ylabel = "y[n]"
    
    f.saveas(s"test_run_dir/ifft_${chiselIFFT.length}_imag.pdf")
  }
  
  /**
   * Generates complex or real sinusoids with optional noise
   */
  def getTone(numSamples: Int, f1r: Double, f2r: Double = 0, f1i: Double = 0, f2i: Double = 0, addNoise: Double = 0, scalingFactor: Int = 1): Seq[Complex] = {
    require(f1r != 0, "Digital frequency should not be zero!")
    import scala.util.Random
    
    (0 until numSamples).map(i => Complex(
    (math.sin(2 * math.Pi * f1r * i) + math.sin(2 * math.Pi * f2r * i))/scalingFactor + addNoise*((Random.nextDouble()*2.0)-1.0),
    (math.sin(2 * math.Pi * f1i * i) + math.sin(2 * math.Pi * f2i * i))/scalingFactor + addNoise*((Random.nextDouble()*2.0)-1.0)))
  }
  
  /**
   * Generates random complex input data
   */
  def genRandSignal(numSamples: Int, scalingFactor: Int): Seq[Complex] = {
    import scala.math.sqrt
    import scala.util.Random
    
    (0 until numSamples).map(x => Complex(Random.nextDouble(), Random.nextDouble()))
    //(0 until numSamples).map(i => Complex((-sqrt(2) + (2*sqrt(2))*Random())/(numSamples/scalingFactor), (-sqrt(2) + (2*sqrt(2))*randomDouble())/(numSamples/scalingFactor))).toVector
  }
  
   /**
   * Generates random realinput data
   */
  def genRandRealSignal(numSamples: Int, scale: Int = 1): Seq[Double] = {
    import scala.math.sqrt
    import scala.util.Random
    
    Random.setSeed(11110L) // generate always the same test example
    (0 until numSamples).map(x => Random.nextDouble())
  }
  
  
  /**
   * Returns bit reversed index
   */
  def bit_reverse(in: Int, width: Int): Int = {
    import scala.math.pow
    var test = in
    var out = 0
    for (i <- 0 until width) {
      if (test / pow(2, width-i-1) >= 1) {
        out += pow(2,i).toInt
        test -= pow(2,width-i-1).toInt
      }
    }
    out
  }
  
  /**
   * Reordering data
   */
  def bitrevorder_data(testSignal: Seq[Complex]): Seq[Complex] = {
    val seqLength = testSignal.size
    val new_indices = (0 until seqLength).map(x => bit_reverse(x, log2Up(seqLength)))
    new_indices.map(x => testSignal(x))
  }
  
  /**
   * Converts data (Int type) to binary represantation
   */
  def asNdigitBinary (source: Int, digits: Int): String = {
    val lstring = source.toBinaryString
    //val sign = if (source > 0) "%0" else "%1"
    if (source >= 0) {
      //val l: java.lang.Long = lstring.toLong
      val l: java.lang.Long = lstring.toLong
      String.format ("%0" + digits + "d", l)
    }
    else
      lstring.takeRight(digits)
  }
  
  /**
   * Format complex data to be compatible with AXI4 stream interface (lenght = 32)
   */
  def formAXI4StreamComplexData(inData : Seq[Complex], dataWidth: Int): Seq[Int] = {
    inData.map(data => java.lang.Long.parseLong(
                                  asNdigitBinary(data.real.toInt, dataWidth) ++ 
                                  asNdigitBinary(data.imag.toInt, dataWidth), 2).toInt)
  }
  /**
   * Format real data to be compatible with AXI4 stream interface (lenght = 32)
   */
  def formAXI4StreamRealData(inData: Seq[Int], dataWidth: Int): Seq[Int] = {
    inData.map(data => java.lang.Long.parseLong(
                                  asNdigitBinary(data, dataWidth) ++ 
                                  asNdigitBinary(0, dataWidth), 2).toInt)
  }
  
  /**
   * Check fft error 
   * Expected data is scala fft result, in the future it can be result of the Python model
   */
  def checkFFTError(expected: Seq[Complex], received: Seq[Complex], tolerance: Int = 4) {
    expected.zip(received).foreach {
      case (in, out) => 
        //println(math.abs(in.real - out.real).toString)
        require(math.abs(in.real - out.real) <= tolerance & math.abs(in.imag - out.imag) <= tolerance, "Tolerance is not satisfied")
    }
  }
}
