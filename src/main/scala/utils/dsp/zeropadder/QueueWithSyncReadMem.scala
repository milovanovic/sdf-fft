package utils.dsp.zeropadder

import chisel3._
import chisel3.util._

import chisel3.experimental.{requireIsChiselType, DataMirror, Direction}
import chisel3.internal.naming._

// Add this because of the incompatibilities with Chisel version!

class QueueWithSyncReadMem[T <: Data](
  val gen:            T,
  val entries:        Int,
  val pipe:           Boolean = false,
  val flow:           Boolean = false,
  val useSyncReadMem: Boolean = false,
  val useBlockRam:    Boolean = false
)(
  implicit compileOptions: chisel3.CompileOptions)
    extends Module() {
  require(entries > -1, "Queue must have non-negative number of entries")
  require(entries != 0, "Use companion object Queue.apply for zero entries")
  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val io = IO(new QueueIO(genType, entries))

  val ram = if (useSyncReadMem) SyncReadMem(entries, genType) else Mem(entries, genType)
  //if (useSyncReadMem) SyncReadMem(entries, genType, SyncReadMem.WriteFirst) else Mem(entries, genType)
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)

  val ptr_match = enq_ptr.value === deq_ptr.value

  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full // added RegNext
  val do_enq = WireDefault(io.enq.fire())
  val do_deq = WireDefault(!empty && io.deq.ready)

  when(do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when(do_deq) {
    deq_ptr.inc()
  }
  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.enq.ready := !full

  if (useSyncReadMem) {
    val deq_ptr_next = Mux(deq_ptr.value === (entries.U - 1.U), 0.U, deq_ptr.value + 1.U)
    if (useBlockRam) {
      // TODO: It should be checked how flow and pipe parameters can influence implementation with block ram
      val r_addr_delayed = RegNext(WireDefault(Mux(do_deq, deq_ptr_next, deq_ptr.value)))
      val outQueue = Module(new Queue(io.deq.bits.cloneType, entries = 1, pipe = true, flow = true))
      outQueue.io.enq.bits := ram(r_addr_delayed)
      outQueue.io.enq.valid := RegNext(do_deq)
      outQueue.io.deq.ready := io.deq.ready
      io.deq.bits := outQueue.io.deq.bits
      io.deq.valid := outQueue.io.deq.valid
    } else {
      io.deq.valid := !empty
      val r_addr = WireDefault(Mux(do_deq, deq_ptr_next, deq_ptr.value))
      io.deq.bits := ram.read(r_addr)
    }
  } else {
    io.deq.valid := !empty
    io.deq.bits := ram(deq_ptr.value)
  }

  if (flow) {
    when(io.enq.valid) { io.deq.valid := true.B }
    when(empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when(io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when(io.deq.ready) { io.enq.ready := true.B }
  }

  val ptr_diff = enq_ptr.value - deq_ptr.value
  if (isPow2(entries)) {
    io.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(
      ptr_match,
      Mux(maybe_full, entries.asUInt, 0.U),
      Mux(deq_ptr.value > enq_ptr.value, entries.asUInt + ptr_diff, ptr_diff)
    )
  }
}

/** Factory for a generic hardware queue.
  *
  * @param enq input (enqueue) interface to the queue, also determines width of queue elements
  * @param entries depth (number of elements) of the queue
  *
  * @return output (dequeue) interface from the queue
  *
  * @example {{{
  * consumer.io.in <> Queue(producer.io.out, 16)
  * }}}
  */
object QueueWithSyncReadMem {

  /** Create a queue and supply a DecoupledIO containing the product. */
  def apply[T <: Data](
    enq:            ReadyValidIO[T],
    entries:        Int = 2,
    pipe:           Boolean = false,
    flow:           Boolean = false,
    useSyncReadMem: Boolean = false,
    useBlockRam:    Boolean = false
  ): DecoupledIO[T] = {
    if (entries == 0) {
      val deq = Wire(new DecoupledIO(chiselTypeOf(enq.bits)))
      deq.valid := enq.valid
      deq.bits := enq.bits
      enq.ready := deq.ready
      deq
    } else {
      val q = Module(new QueueWithSyncReadMem(chiselTypeOf(enq.bits), entries, pipe, flow, useSyncReadMem, useBlockRam))
      q.io.enq.valid := enq.valid // not using <> so that override is allowed
      q.io.enq.bits := enq.bits
      enq.ready := q.io.enq.ready
      q.io.deq
    }
  }

  /** Create a queue and supply a IrrevocableIO containing the product.
    * Casting from Decoupled is safe here because we know the Queue has
    * Irrevocable semantics; we didn't want to change the return type of
    * apply() for backwards compatibility reasons.
    */
  def irrevocable[T <: Data](
    enq:            ReadyValidIO[T],
    entries:        Int = 2,
    pipe:           Boolean = false,
    flow:           Boolean = false,
    useSyncReadMem: Boolean = false,
    useBlockRam:    Boolean = false
  ): IrrevocableIO[T] = {
    val deq = apply(enq, entries, pipe, flow, useSyncReadMem, useBlockRam)
    require(entries > 0, "Zero-entry queues don't guarantee Irrevocability")
    val irr = Wire(new IrrevocableIO(chiselTypeOf(deq.bits)))
    irr.bits := deq.bits
    irr.valid := deq.valid
    deq.ready := irr.ready
    irr
  }
}
