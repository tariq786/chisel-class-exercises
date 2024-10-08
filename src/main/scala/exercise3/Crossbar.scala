package exercise3

import chisel3._
import chisel3.util._
import exercise2.{Distributor, RegDistributor}

abstract class Crossbar[D <: Data](dtype : D, numIn : Int, numOut : Int) extends Module {
  val in = IO(Vec(numIn, Flipped(Decoupled(dtype))))
  val dest = IO(Input(Vec(numIn, UInt(numOut.W)))) //corresponds to every in, so { in(0) => dest(0)}, {in(1) => dest(1)}
  val out = IO(Vec(numOut, Decoupled(dtype)))     //and every dest(i) is numOut wide. For 2x3 Crossbar, dest(i) is 3 bits wide
}                                                // can be from {{000},...,{111}}

class DistributorCrossbar[D <: Data](dtype : D, numIn : Int, numOut : Int) extends Crossbar(dtype, numIn, numOut) {
  // Create your own implementation of a crossbar using the RegDistributor implementation from
  // exercise2 and the RRArbiter Chisel component

  //create numOut number of RRArbiters where each has numIn number of inputs
  val arb = for (s <- 0 until numOut) yield Module(new RRArbiter(dtype, numIn))

  for(i <-0 until(numOut)) {
    for(j <-0 until(numIn)) {
      arb(i).io.in(j) <> in(j)
    }
  }

  for(i <-0 until(numOut)) {
    arb(i).io.out <> out(i)
  }

  /*//instantiate RegDistributor for every input, note "numOut" is the number of output interfaces NOT numIn
  val regD = for (i <- 0 until numIn) yield Module (new RegDistributor[D](dtype,numOut))

  //connect RegDistributor to every input
  for(i <-0 until(numIn))
    {
        in(i) <> regD(i).in
        dest(i) <> regD(i).dest
    }

  //connect RRArbiter to output of every RegDistributor
  for(i <-0 until(numOut)) {
    for(j <-0 until(numIn)) {
        arb(i).io.in(j) <> regD(j).out(i)
        }
  }
  //connect RRArbiter output to every out(i)
  for(i <-0 until(numOut)) {
  arb(i).io.out <> out(i)
  }
*/


}//end of class DistributorCrossbar

object Crossbar {
  def apply[D <: Data](imp : String, dtype : D, numIn : Int, numOut : Int) : Crossbar[D] = {
    imp match {
      case _ => new DistributorCrossbar(dtype, numIn, numOut)
    }
  }

  def getImpTypes : Seq[String] = Seq("distributor", "ring", "banyan")
}