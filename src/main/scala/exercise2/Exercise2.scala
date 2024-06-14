package exercise2

import chisel3._
import chisel3.util._
import exercise1.Multiplier

/** Compute the square of the input value
 *
 * Use the provided Multiplier block to create a module which computes the square of the input.
 * Note that the multiplier ready signals will not be asserted at the same time, so some logic
 * must be written to adapt the incoming dataIn to the timing required by the two multiplier
 * input ports.
 *
 * Suggested approaches are to either create a finite state machine to track when when ready
 * signals are asserted, or use Chisel's built-in Queue module to provide buffering.
 *
 */
class Exercise2 extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(Decoupled(UInt(8.W)))
    val dataOut = Decoupled(UInt(16.W))
  })
  val mult = Module(new Multiplier(8))
  val rep = Module(Distributor("reg", UInt(8.W), 2))

  io.dataIn <> rep.in
  rep.dest := Fill(2, 1.B)
  rep.out(0) <> mult.io.a
  rep.out(1) <> mult.io.b
  mult.io.z <> io.dataOut
}
