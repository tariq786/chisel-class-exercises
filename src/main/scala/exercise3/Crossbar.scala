package exercise3

import chisel3._
import chisel3.util._
import exercise2.{ComboDistributor, Distributor, RegDistributor}
import chisel3.dontTouch

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


  /*for(i <-0 until(numOut)) {  //this does not work
    for(j <-0 until(numIn)) {
      arb(i).io.in(j) <> in(j)
    }
  }
  for(i <-0 until(numOut)) {
    arb(i).io.out <> out(i)
  }*/

  //instantiate RegDistributor for every input, note "numOut" is the number of output interfaces NOT numIn
  val regD = for (i <- 0 until numIn) yield Module (new RegDistributor[D](dtype,numOut))

  //connect RegDistributor to every input
  for(i <-0 until(numIn))
    {
        in(i) <> regD(i).in
        dest(i) <> regD(i).dest
    }

  //connect RRArbiter to output of every RegDistributor
  //go over each RRArbiter and connect all RegD outputs to the input of the RRArbiter
  for(i <-0 until(numOut)) {
    for(j <-0 until(numIn)) {
        arb(i).io.in(j) <> regD(j).out(i)
        }
  }
  //connect RRArbiter output to every out(i)
  for(i <-0 until(numOut)) {
  arb(i).io.out <> out(i)
  }



}//end of class DistributorCrossbar

class RingCrossbar[D <: Data](dtype : D, numIn : Int, numOut : Int) extends Crossbar(dtype, numIn, numOut) {
/*
  //To store ring's data (shouldn't we use Reg???? so need to double check)
  //val ring = Wire(Vec(numOut, Decoupled(dtype)))

  //To store and process dest(i) that corresponds to every in(i)
  val  idest     = dontTouch(RegInit(VecInit(Seq.fill(numIn)(0.U(numOut.W)))))

  //capture dest(i) for corresponding in(i)
  for(i <- 0 until(numIn)) {
    idest(i) := Mux(in(i).fire, dest(i), idest(i))
  }

  //Create numOut number of RRArbiters where each has 2 inputs????
  val arb = for (i <- 0 until numOut) yield Module(new RRArbiter(dtype, 2)) //note 2 inputs

  //Instantiate RegDistributor for every Output, note "numOut" is the number of output interfaces NOT numIn
  val regD = for (i <- 0 until numOut) yield Module (new RegDistributor(dtype,1)) //note 1 input

  //one-entry deep FIFO for every RRArbiter output
  val queue = for(i <- 0 until  numOut) yield Module(new Queue(dtype, 1))

  //Connect RRArbiter's inputs and outputs
  for(i <- 0 until numOut) {
    arb(i).io.in(0) <> queue(i).io.deq
    arb(i).io.in(1) <> in(i)
    arb(i).io.out   <> queue((i+1) % numOut).io.enq  //ring( (i+1) % numOut) //change this ???????
  }
  /*//last RRArbiter(0)
  arb(0).io.in(0) <> queue(0).io.deq
  arb(0).io.in(1) <> in(0)
  arb(0).io.out   <> queue(1).io.enq
*/
  //Connect FIFO to RegDistributor(i)
  for(i <- 1 until(numOut)){
    queue(i-1).io.deq <> regD(i).in
    dest(i) <> regD(i).dest
    regD(i).out(0) <> out(i)
  }
  //last RegDistributor(0)
  queue(3).io.deq <> regD(0).in
  dest(0) <> regD(0).dest
  regD(0).out(0) <> out(0)


  for (i <- 0 until numIn) {
    for (j <- 0 until numOut) {
      when(idest(i)(j)) { // Check if idest(i) has bit j set to 1
        idest(i) := idest(i).bitSet(j.asUInt, false.B) // Unset the bit j
      }
    }
  }*/
val st = for (i <- 0 until numOut) yield Module(new Station[D](dtype,numIn,numOut))
  for(i <- 0 until numOut) {
    st(i).ringDataOut.sdest <> dest
    st(i).arb.io.in(0) <> in
    st(i).regD.out <> out
  }

} //end of class RingCrossbar

object Crossbar {
  def apply[D <: Data](imp : String, dtype : D, numIn : Int, numOut : Int) : Crossbar[D] = {
    imp match {
      case "ring" => new RingCrossbar(dtype, numIn,numOut)
      case _ => new DistributorCrossbar(dtype, numIn, numOut)
    }
  }

  def getImpTypes : Seq[String] = Seq(/*"distributor",*/ "ring"/*, "banyan"*/)
}

object GenCrossbar extends App
{
  val baseArguments = Array("--strip-debug-info", "--split-verilog", "--disable-all-randomization", "--lowering-options=disallowExpressionInliningInPorts,disallowLocalVariables")


  //
  //  ChiselStage.emitSystemVerilogFile(new Exercise4, Array.empty, baseArguments)
  emitVerilog(new DistributorCrossbar[UInt](UInt(8.W),3,2))    //use sbt run from the command line to get verilog

}
