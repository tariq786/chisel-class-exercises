package exercise3

import chisel3._
import chisel3.util.{PriorityEncoder, _}
import exercise2.{ComboDistributor, Distributor, RegDistributor}

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

val st = for (i <- 0 until numOut) yield Module(new Station[D](dtype,numIn,numOut,i))

  for(i <- 0 until numOut) {
    st( (i+1) % numOut).ringDataIn <> st(i).ringDataOut
    st(i).dest <> dest(i)
    st(i).in <> in(i)
    st(i).out <> out(i)
     }

} //end of class RingCrossbar

class BanyanCrossbar[D <: Data](dtype : D, numIn : Int, numOut : Int) extends Crossbar(dtype, numIn, numOut) {

//constructing a 4x4 cross bar using
// 1) N/2 SEs and
// 2) two networks of N/2*N/2

val se = for(i <-0 until(numOut)) yield Module(new DistributorCrossbar(dtype, 2,2))

    //inputs

    val Dest0 = PriorityEncoder(dest(0)).asBools
    val Dest1 = PriorityEncoder(dest(1)).asBools
    val Dest2 = PriorityEncoder(dest(2)).asBools
    val Dest3 = PriorityEncoder(dest(3)).asBools

    se(0).in(0)   <> in(0)
    se(0).in(1)   <> in(1)
    se(0).dest(0) <> Mux(Dest0(1),2.U,1.U) //goes with in(0)
    se(0).dest(1) <> Mux(Dest1(1),2.U,1.U) //goes with in(1)

    se(1).in(0)   <> in(2)
    se(1).in(1)   <> in(3)
    se(1).dest(0) <> Mux(Dest2(1),2.U,1.U) //goes with in(2)
    se(1).dest(1) <> Mux(Dest3(1),2.U,1.U) //goes with in(3)

    //middle

    se(2).dest(0) <> Mux(se(0).in(0).ready,Mux(Dest0(0),2.U,1.U),Mux(Dest1(0),2.U,1.U))
    se(2).dest(1) <> Mux(se(1).in(0).ready,Mux(Dest2(0),2.U,1.U),Mux(Dest3(0),2.U,1.U))

    se(3).dest(0) <> Mux(se(0).in(0).ready,Mux(Dest0(0),2.U,1.U),Mux(Dest1(0),2.U,1.U))
    se(3).dest(1) <> Mux(se(1).in(0).ready,Mux(Dest2(0),2.U,1.U),Mux(Dest3(0),2.U,1.U))

    se(0).out(0) <> se(2).in(0)
    se(0).out(1) <> se(3).in(0)

    se(1).out(0) <> se(2).in(1)
    se(1).out(1) <> se(3).in(1)


    //outputs

    se(2).out(0) <> out(0)
    se(2).out(1) <> out(1)
    se(3).out(0) <> out(2)
    se(3).out(1) <> out(3)


} //end of class BanyanCrossbar




object Crossbar {
  def apply[D <: Data](imp : String, dtype : D, numIn : Int, numOut : Int) : Crossbar[D] = {
    imp match {
      case "ring" => new RingCrossbar(dtype, numIn,numOut)
      case "banyan" => new BanyanCrossbar(dtype, numIn,numOut)
      case _ => new DistributorCrossbar(dtype, numIn, numOut)
    }
  }

  def getImpTypes : Seq[String] = Seq(/*"distributor", "ring", */"banyan")
}

object GenCrossbar extends App
{
  val baseArguments = Array("--strip-debug-info", "--split-verilog", "--disable-all-randomization", "--lowering-options=disallowExpressionInliningInPorts,disallowLocalVariables")


  //
  //  ChiselStage.emitSystemVerilogFile(new Exercise4, Array.empty, baseArguments)
 // emitVerilog(new DistributorCrossbar[UInt](UInt(8.W),3,2))    //use sbt run from the command line to get verilog

}
