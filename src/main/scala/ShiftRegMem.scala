// SPDX-License-Identifier: Apache-2.0

package fft

import chisel3._
import chisel3.internal.requireIsHardware
import chisel3.util._
import chisel3.experimental.ChiselEnum

object ShiftRegMem {

  // use_sp_mem = use single port SRAMs? if false, use dual-port SRAMs
  def apply[T <: Data](in: T, n: Int, en: Bool = true.B, use_sp_mem: Boolean = false, name: String = null): T =
  {
    requireIsHardware(in)
    //require(n%2 == 0, "Odd ShiftRegsiterMem not supported yet")

    if (n == 0) {
      in
    } else if (n == 1) {
      val out = RegEnable(in, en)
      out
    //} else if (use_sp_mem && n%2 == 0) { // TODO: this passes the test but doesn't work for all cases
    //  val out = Wire(in.cloneType)
    //  val mem = SyncReadMem(n/2, Vec(in, in))
    //  if (name != null) {
    //    println(s"Name support not implemented")
    //    //sram.setName(name)
    //  }
    //  val index_counter = Counter(en, n)._1
    //  val raddr = (index_counter + 2.U) >> 1.U
    //  val waddr = RegEnable(index_counter >> 1.U, (n/2-1).U, en)
    //  val wen = index_counter(0) && en
    //  val des = Reg(in.cloneType)
    //  val ser = Reg(in.cloneType)

    //  val sram_out = Reg(next=mem.read(raddr, !wen))

    //  when (wen) {
    //    mem.write(waddr, Vec(des, in))
    //    out := ser
    //  } .otherwise {
    //    des := in
    //    out := sram_out(0)
    //    ser := sram_out(1)
    //  }
    //  out
    } else {
      val out_reg = RegInit(0.U.asTypeOf(in.cloneType))
      dontTouch(out_reg)
      out_reg.suggestName("out_reg")

      val mem = SyncReadMem(n, in.cloneType)
      if (name != null) {
        mem.suggestName(name)
      }
      val raddr = Counter(en, n)._1
      val out = mem.read(raddr)

      when (RegNext(en,false.B)) {
        out_reg := out
      }

      val waddr = RegEnable(raddr, (n-1).U, en) //next, init, enable
      when (en) {
        mem.write(waddr, in)
      }

      val out_w = Mux(RegNext(en,false.B), out, out_reg)

      out_w
    }
  }
}







// // SPDX-License-Identifier: Apache-2.0

// package fft

// import chisel3._
// import chisel3.internal.requireIsHardware
// import chisel3.util._
// import chisel3.experimental.ChiselEnum

// object ShiftRegMem {

//   // use_sp_mem = use single port SRAMs? if false, use dual-port SRAMs
//   def apply[T <: Data](in: T, n: Int, en: Bool = true.B, use_sp_mem: Boolean = false, name: String = null): T =
//   {
//     requireIsHardware(in)
//     //require(n%2 == 0, "Odd ShiftRegsiterMem not supported yet")

//     if (n == 0) {
//       in
//     } else if (n == 1) {
//       val out = RegEnable(in, en)
//       out
//     //} else if (use_sp_mem && n%2 == 0) { // TODO: this passes the test but doesn't work for all cases
//     //  val out = Wire(in.cloneType)
//     //  val mem = SyncReadMem(n/2, Vec(in, in))
//     //  if (name != null) {
//     //    println(s"Name support not implemented")
//     //    //sram.setName(name)
//     //  }
//     //  val index_counter = Counter(en, n)._1
//     //  val raddr = (index_counter + 2.U) >> 1.U
//     //  val waddr = RegEnable(index_counter >> 1.U, (n/2-1).U, en)
//     //  val wen = index_counter(0) && en
//     //  val des = Reg(in.cloneType)
//     //  val ser = Reg(in.cloneType)

//     //  val sram_out = Reg(next=mem.read(raddr, !wen))

//     //  when (wen) {
//     //    mem.write(waddr, Vec(des, in))
//     //    out := ser
//     //  } .otherwise {
//     //    des := in
//     //    out := sram_out(0)
//     //    ser := sram_out(1)
//     //  }
//     //  out
//     } else {

//       // FSM
//       object State extends ChiselEnum {
//         val s0, s1, s2, s3 = Value
//       }
//       val state     = RegInit(State.s0)
//       val out_reg_0 = RegInit(0.U.asTypeOf(in.cloneType))
//       val out_reg_1 = RegInit(0.U.asTypeOf(in.cloneType))

//       dontTouch(state)
//       dontTouch(out_reg_0)
//       dontTouch(out_reg_1)
//       state.suggestName("state")
//       out_reg_0.suggestName("out_reg_0")
//       out_reg_1.suggestName("out_reg_1")


//       val mem = SyncReadMem(n, in.cloneType)
//       if (name != null) {
//         mem.suggestName(name)
//       }
//       val raddr   = Counter(en, n)._1
//       val readmem = mem.read(raddr)
//       val out = Wire(in.cloneType)

//       out_reg_0 := readmem
//       out_reg_1 := RegEnable(readmem, 0.U.asTypeOf(in.cloneType), RegNext(en,false.B)) //next, init, enable

//       val waddr = RegEnable(raddr, (n-1).U, en) //next, init, enable
//       when (en) {
//         mem.write(waddr, in)
//       }

//       // FSM
//       when(state === State.s0) {
//         state := State.s1
//         out   := readmem
//       }
//       .elsewhen(state === State.s1) {
//         when(en) {
//           out := readmem
//           state := State.s0
//         }
//         .otherwise {
//           out := out_reg_0
//           state := State.s2
//         }
//       }
//       .elsewhen(state === State.s2) {
//         out := out_reg_1
//         when(en) {
//           state := State.s0
//         }
//         .otherwise {
//           state := State.s2
//         }
//       }
//       .otherwise {
//         state := State.s0
//         out   := readmem
//       }

//       out
//     }
//   }
// }
