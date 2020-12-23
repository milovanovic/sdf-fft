// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._

import dsptools._
import dsptools.numbers._

// Try to integrate it into chisel3 util library
object RegEnableWithReset {

  /** Returns a register with the specified next, update enable gate, manual reset and reset initialization.
    *
    * @example {{{
    * val regWithEnableAndReset = RegEnable(nextVal, 0.U, reset, ena)
    * }}}
    */
  def apply[T <: Data](next: T, init: T, reset: Bool, enable: Bool): T = {
    val r = RegInit(init)
    
    when (reset) { r := init }
    .elsewhen (enable) { r := next }
    r
  }
}
// Try to integrate it into chisel3 util library
object ShiftRegisterWithReset
{
  /** Returns the n-cycle delayed version of the input signal.
    *
    * @param in input to delay
    * @param n number of cycles to delay
    * @param resetData reset value for each register in the shift
    * @param reset manual reset
    * @param en enable the shift
    */
  def apply[T <: Data](in: T, n: Int, resetData: T, reset: Bool, en: Bool = true.B): T = {
    // The order of tests reflects the expected use cases.
    if (n != 0) {
      RegEnableWithReset(apply(in, n-1, resetData, reset, en), resetData, reset, en)
    } else {
      in
    }
  }
}

/**
  * Simple radix 2 Butterfly
  */
object Butterfly extends hasContext {
  def apply[T <: Data : Real : BinaryRepresentation](in: Seq[DspComplex[T]]): Seq[DspComplex[T]] = {
    require(in.length == 2, "2-point DFT only for no defined twiddle type")
    (Seq(DspContext.alter(
      DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 0))
        { in(0) context_+ in(1) },
      DspContext.alter(
        DspContext.current.copy(overflowType = Grow, binaryPointGrowth = 0))
        { in(0) context_- in(1) }))
   }
}
