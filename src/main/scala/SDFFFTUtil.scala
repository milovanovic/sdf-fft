package fft

import chisel3._

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
