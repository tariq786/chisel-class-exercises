package scratchpad

import chisel3._
import chisel3.util._




class Fifo2(depth: Int)  extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val out = Decoupled(UInt(8.W))
  })


  val ram = Mem(depth, UInt(8.W))
  val enqptr = RegInit(0.U(log2Ceil(depth).W))
  val deqptr = RegInit(0.U(log2Ceil(depth).W))
  val isFull = RegInit(false.B)
  val isEmpty = (deqptr === enqptr) && !isFull

  io.in.ready := !isFull
  io.out.bits := ram(deqptr)
  io.out.valid := !isEmpty

  when(io.in.valid && !isFull) {
    ram(enqptr) := io.in.bits
    enqptr := Mux(enqptr === (depth - 1).U, 0.U, enqptr + 1.U)
    when((enqptr + 1.U)  === deqptr) {
      isFull := true.B
    }
  }
  when(io.out.fire && !isEmpty) {
//    io.out.bits := ram(deqptr)
    deqptr := Mux(deqptr === (depth - 1).U, 0.U, deqptr + 1.U)
  }

}