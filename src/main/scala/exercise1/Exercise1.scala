package exercise1

import chisel3._
import chisel3.util._

/** Compute the square of the input value
 *
 * Uses the provided Multiplier block to create a module which computes the square of the input.
 * Note that the multiplier ready signals will not be asserted at the same time, so some logic
 * must be written to adapt the incoming dataIn to the timing required by the two multiplier
 * input ports.
 *
 * Creates an instance of the Multiplier connected to your design ("Exercise1Replicate") which
 * performs the logic function of connecting the interfaces.
 */
class Exercise1 extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(Decoupled(UInt(8.W)))
    val dataOut = Decoupled(UInt(16.W))
  })
  val mult = Module(new Multiplier(8))
  val rep = Module(new Exercise1Replicate(8))

  io.dataIn <> rep.io.dataIn
  rep.io.dataOut(0) <> mult.io.a
  rep.io.dataOut(1) <> mult.io.b
  mult.io.z <> io.dataOut
}

/** Your implmentation of data replication
 *
 * Suggested approaches are to either create a finite state machine to track when when ready
 * signals are asserted, or use Chisel's built-in Queue module to provide buffering.
 */
class Exercise1Replicate(width : Int) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(Decoupled(UInt(width.W)))
    val dataOut = Vec(2, Decoupled(UInt(width.W)))
  })
  // Write your code to replicate data on dataIn across both dataOut interfaces
}
