package dspblocks.utils.dsp.windowing

import breeze.math.Complex

/**
  * Contains useful helper functions for testers
  */

trait HasSignalUtils {

  /**
    * Generates complex or real sinusoids with optional noise
    */
  def getTone(
    numSamples:    Int,
    f1r:           Double,
    f2r:           Double = 0,
    f1i:           Double = 0,
    f2i:           Double = 0,
    addNoise:      Double = 0,
    scalingFactor: Int = 1
  ): Seq[Complex] = {
    require(f1r != 0, "Digital frequency should not be zero!")
    import scala.util.Random

    (0 until numSamples).map(i =>
      Complex(
        (math.sin(2 * math.Pi * f1r * i) + math
          .sin(2 * math.Pi * f2r * i)) / scalingFactor + addNoise * ((Random.nextDouble() * 2.0) - 1.0),
        (math.sin(2 * math.Pi * f1i * i) + math
          .sin(2 * math.Pi * f2i * i)) / scalingFactor + addNoise * ((Random.nextDouble() * 2.0) - 1.0)
      )
    )
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
    * Generates random real input data
    */
  def genRandRealSignal(numSamples: Int, scale: Int = 1): Seq[Double] = {
    import scala.math.sqrt
    import scala.util.Random

    Random.setSeed(11110L) // generate always the same test example
    (0 until numSamples).map(x => Random.nextDouble())
  }

  /**
    * Converts data (Int type) to binary represantation
    */
  def asNdigitBinary(source: Int, digits: Int): String = {
    val lstring = source.toBinaryString
    //val sign = if (source > 0) "%0" else "%1"
    if (source >= 0) {
      //val l: java.lang.Long = lstring.toLong
      val l: java.lang.Long = lstring.toLong
      String.format("%0" + digits + "d", l)
    } else
      lstring.takeRight(digits)
  }

  /**
    * Format complex data to be compatible with AXI4 stream interface (length = 32)
    */
  def formAXI4StreamComplexData(inData: Seq[Complex], dataWidth: Int): Seq[Int] = {
    inData.map(data =>
      java.lang.Long
        .parseLong(
          asNdigitBinary(data.real.toInt, dataWidth) ++
            asNdigitBinary(data.imag.toInt, dataWidth),
          2
        )
        .toInt
    )
  }

  /**
    * Format real data to be compatible with AXI4 stream interface (length = 32)
    */
  def formAXI4StreamRealData(inData: Seq[Int], dataWidth: Int): Seq[Int] = {
    inData.map(data =>
      java.lang.Long
        .parseLong(
          asNdigitBinary(data, dataWidth) ++
            asNdigitBinary(0, dataWidth),
          2
        )
        .toInt
    )
  }

  /**
    * Check error
    */
  def checkError(expected: Seq[Complex], received: Seq[Complex], tolerance: Int = 4) {
    expected.zip(received).foreach {
      case (in, out) =>
        //println(math.abs(in.real - out.real).toString)
        require(
          math.abs(in.real - out.real) <= tolerance & math.abs(in.imag - out.imag) <= tolerance,
          "Tolerance is not satisfied"
        )
    }
  }
}
