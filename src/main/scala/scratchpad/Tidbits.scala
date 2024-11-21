package scratchpad

import chisel3._
import chisel3.util.{circt, _}

class ManyConnections extends Module {
  val io = IO(new Bundle {
    val in0  = Input(Bool())
    val in1  = Input(Bool())
    val in2  = Input(Bool())
    val in3  = Input(Bool())
    val out = Output(Bool())
  })
  io.out := io.in0 & io.in1 & io.in2 & io.in3
}

object ManyConnections
{
  // YOUR CODE HERE
  def apply(a: Bool,b: Bool,c: Bool,d: Bool) = {
     val inst = Module(new ManyConnections())
      inst.io.in0 := a
      inst.io.in1 := b
      inst.io.in2 := c
      inst.io.in3 := d
      inst.io.out

    //val x = new ManyConnections()  //why this fails?????
  }
}


class UseManyConn extends Module {
  val io = IO(new Bundle {
    val in0 = Input(Bool())
    val in1 = Input(Bool())
    val in2 = Input(Bool())
    val in3 = Input(Bool())
    val out = Output(Bool())
  })

  val one = ManyConnections(true.B, true.B, true.B, true.B)
  val two = ManyConnections(io.in0, io.in1, io.in2, io.in3)

  //     Hopefully above is more appealing than multiples of these:

  //     val m0 = Module(new ManyConnections)
  //     m0.io.in0 := io.in0
  //     m0.io.in1 := io.in1
  //     m0.io.in2 := io.in2
  //     m0.io.in3 := io.in3
  //     val and = m0.io.out

  io.out := one & one//two
  printf(p"io.out inside DUT = ${io.out}")


}


