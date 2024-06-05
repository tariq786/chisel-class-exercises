package exercise3

import chisel3._
import chisel3.util._
import exercise2.Distributor

abstract class Crossbar[D <: Data](dtype : D, numIn : Int, numOut : Int) extends Module {
  val in = IO(Vec(numIn, Flipped(Decoupled(dtype))))
  val dest = IO(Input(Vec(numIn, UInt(numOut.W))))
  val out = IO(Vec(numOut, Decoupled(dtype)))
}

class DistributorCrossbar[D <: Data](dtype : D, numIn : Int, numOut : Int) extends Crossbar(dtype, numIn, numOut) {
  // Create your own implementation of a crossbar using the RegDistributor implementation from
  // exercise2 and the RRArbiter Chisel component
}

object Crossbar {
  def apply[D <: Data](imp : String, dtype : D, numIn : Int, numOut : Int) : Crossbar[D] = {
    imp match {
      case _ => new DistributorCrossbar(dtype, numIn, numOut)
    }
  }

  def getImpTypes : Seq[String] = Seq("distributor", "ring", "banyan")
}