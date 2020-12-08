// SPDX-License-Identifier: Apache-2.0

package fft

import breeze.numerics.{cos, sin}
import scala.math.{abs, Pi, pow}


sealed trait WindowFunctionType {
  val dataWidth: Int }

object WindowFunctionTypes {
  case class Hamming(dataWidth_tmp : Int = 16, alpha: Double = 0.54, beta: Double = 0.46, isPeriodic: Boolean = true) extends WindowFunctionType { 
    val dataWidth = dataWidth_tmp
    override def toString = "Hamming window (" + alpha + ", " + beta + ")" // add info whether it is symmetric or periodic
  }
  case class Hanning(dataWidth_tmp: Int = 16, isPeriodic: Boolean = true) extends WindowFunctionType {
    val dataWidth = dataWidth_tmp
    override def toString = "Hanning window"
  }
  case class Triangular(dataWidth_tmp: Int = 16, isPeriodic: Boolean = true) extends WindowFunctionType {
    val dataWidth = dataWidth_tmp
    override def toString  = "Triangular window"
  }
  case class Blackman(dataWidth_tmp: Int = 16, val a0: Double = 0.42, val a1: Double = 0.5, val a2: Double = 0.08, isPeriodic: Boolean = true) extends WindowFunctionType {
    val dataWidth = dataWidth_tmp

    override def toString = "Blackman window (" + a0 + a1 + a2 + ")"
  }
  case class User(val dataWidth_tmp: Int = 16, val dv: Seq[Double]) extends WindowFunctionType {
    val dataWidth = dataWidth_tmp
    override def toString = "user-specified window"
  }
  case class None(val dataWidth_tmp: Int = 16) extends WindowFunctionType {
    val dataWidth = dataWidth_tmp
    override def toString = "no window"
  }
}

// periodic for spectral analyses, symmetric for filtering
object WindowFunctions {

  def hammingWindow(length: Int, alpha: Double = 0.54, beta: Double = 0.46, isPeriodic: Boolean = true): Seq[Double] = {
    if (length == 1) Seq(1.0)
    else if (isPeriodic) (0 until (length + 1)).map((count: Int) => alpha - beta * cos(2 * Pi * count.toDouble / length)).take(length)
    else (0 until length).map((count: Int) => alpha - beta * cos(2 * Pi * count.toDouble / (length - 1)))
  }
  
  def triangularWindow(length: Int, isPeriodic: Boolean = true): Seq[Double] = {
   if (length == 1) Seq(1.0)
   else if (isPeriodic) (0 until (length + 1)).map((count: Int) => 1 - abs((count.toDouble - length / 2.0) / (length / 2.0))).take(length)
   else (0 until length).map((count: Int) => 1 - abs((count.toDouble - (length - 1) / 2.0) / ((length - 1) / 2.0)))
  }

  def blackmanWindow(length: Int, a0: Double = 0.42, a1: Double = 0.5, a2: Double = 0.08, isPeriodic: Boolean = true): Seq[Double] = {
    if (length == 1) Seq(1.0)
    else if (isPeriodic) (0 until (length + 1)).map((count: Int) => a0 - a1 * cos(2 * Pi * count.toDouble / length) + a2 * cos(4 * Pi * count.toDouble / length))
    else (0 until length).map((count: Int) => a0 - a1 * cos(2 * Pi * count.toDouble / (length - 1)) + a2 * cos(4 * Pi * count.toDouble / (length - 1)))
  }

  def hanningWindow(length: Int, isPeriodic: Boolean = true): Seq[Double] = {
    if (length == 1) Seq(1.0)
    else if (isPeriodic) (0 until (length + 1)).map((count: Int) => 0.5 * (1 - cos(2 * Pi * count.toDouble / length)))
    else (0 until length).map((count: Int) => 0.5 * (1 - cos(2 * Pi * count.toDouble / (length - 1))))
  }
}
