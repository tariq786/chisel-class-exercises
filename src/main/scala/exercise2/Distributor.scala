package exercise2

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import chisel3.util._

/** Receives an incoming atom and replicates it across num interfaces
 *
 * Abstract base class allows for multiple implementations.
 *
 * @param dtype Atom data type
 * @param num   Number of output interfaces
 */

//my comments
//In Chisel, the <: operator is used to denote type parameter bounds, specifying that a type parameter must be
// a subtype of a specified type. This is often used in generic classes or traits to ensure that the provided
// type parameter adheres to certain constraints.

// Define an abstract class with a type parameter "D" that must be a subtype of "Data" (which is the base class
//for all Chisel types (UInt, SInt, Bool, FixedPoint, Bundle, Vec) )

//[D <: Data], the type parameter D is constrained to be a subtype of Data
//(dtype : D, num : Int) parameters allow the class to be instantiated for different Chisel Data types and width
abstract class Distributor[D <: Data](dtype : D, num : Int) extends Module
{
  val in = IO(Flipped(Decoupled(dtype)))      //new style chisel
  val dest = IO(Input(UInt(num.W)))       //which output/s gets input data e.g., when num = 2, "dest" could be either of
                                          // 0 or 00  (no output receives input), or
                                          // 1 or 01 (only first output receives input), or
                                          //2 or 10 (only second output receives input), or
                                          //3 or 11 (both outputs receive input)
                                          //when num = 3, "dest" could be either of
                                          // 000
                                         // 001
                                         // 010
                                         // 011
                                         //100
                                         //101
                                        //110
                                        //111

  val out = IO(Vec(num, Decoupled(dtype)))
}

/** Distributor with registered data
 *
 * Implementation which registers incoming data, and therefore has a one-cycle latency
 * on data transmission.  Implementation should be permissive on all interfaces.
 */
class RegDistributor[D <: Data](dtype : D, num : Int) extends Distributor(dtype, num)
{
  // Create your implementation of distributor here


  //Approach 1
  // val iData = Reg(dtype)
  // assert in.ready = 1.U for permissiveness (can we assume the same with in.valid???)
  // when (in.valid) {
  //    iData = in.bits     //??? do we really need it for the implementation below? (just like Exercise4 (Multiplier FSM)
  //    saveDest = dest
  //    deassert in.ready ???
  //                }
  // for i = 0 to saveDest {
  //      out(i).valid = 1.U
  //      when(out(i).ready) {
  //            out(i).bits = iData
  //            }
  //       }
  //    in.ready = 1.U

   val  iData     = Reg(dtype)
   val  idest     = RegInit(UInt(num.W),0.U)
   val  nextidest = Wire(UInt(num.W))
   //val  allready  = Reverse(Cat(for(i<- 0 until num)  yield out(i).ready)) //yields seq of all ready signals

   val allreadyVec = Reg(Vec(num,Bool()))
    allreadyVec := VecInit(Seq.fill(num)(false.B))
   val allready = allreadyVec.asUInt

    iData     := Mux(in.fire,in.bits,iData)
    idest     := Mux(in.fire, dest, nextidest)

  //  in.ready  := (idest === 0.U) || (nextidest === 0.U)
      in.ready := (allready === dest)
    for(i <- 0 until  num)
      {
        out(i).valid := idest(i)
        out(i).bits  := iData
        when(out(i).ready) {
          allreadyVec(i) := out(i).ready
        }

      }
      nextidest := ~allready & idest



//  Big AND is not permissive, so can't use it here without doing something else with it

//   in.ready  := 1.U       // for permissiveness (can we assume the same with in.valid???)
//   when (in.valid)
//   {
//      iData := in.bits     //??? do we really need it for the implementation below? (just like Exercise4 (Multiplier FSM)
//      idest := dest
//      in.ready := 0.U
//   }
//  for (i <-0 until num) {
//        out(i).valid := 1.U //
//        out(i).bits := iData
//        when(out(i).ready) {
//
//              }
//         }
//      in.ready := 1.U

} //end of  RegDistributor class


//to select between multiple implementations
/** Distributor with combinatorial datapath
 *
 * This combinatorial implementation does not register the data, and forces the source
 * to hold the data by deasserting ready until all destinations have acknowledged.
 *
 * Note that this will create a dependency of in.ready on in.valid, thereby making this
 * implementation demanding on the in port.
 *
 * Implementing ComboDistributor is optional, to test this implementation, add the
 * string "combo" to getImpTypes, below.
 */
class ComboDistributor[D <: Data](dtype : D, num : Int) extends Distributor(dtype, num)
{


  val allreadyVec = Wire(Vec(num,Bool()))
  val allready = allreadyVec.asUInt
  //Registers to keep track of what destinations have been fulfilled
  val iDest = RegInit(VecInit(Seq.fill(num)(false.B)))

  //Defaults
  for (i <- 0 until num) {
    out(i).valid := false.B
    out(i).bits := 0.U
    allreadyVec(i) := 0.B
  }
  in.ready := 0.B

   when(in.valid)
   {
     for (i <- 0 until num)
     {
       when(dest(i)) //if ith bit position/s in dest = 1, then that port/s is/are chosen
       {
         iDest(i) := true.B
         out(i).valid := true.B
         out(i).bits := in.bits

         when(out(i).ready) {
           allreadyVec(i) := out(i).ready
         }
         when((allready === dest) && (dest > 0)) {
           in.ready := 1.B
         }
       }
     }
   }
} //end of ComboDistributor class


/* FullDistributor can accept data for an output port/s as long as it/they is/are free. It has storage for
  every output port. Using composition to implement FullDistributor using FIFO and ComboDistributor
 */
class FullDistributor[D <: Data](dtype : D, num : Int) extends Distributor(dtype, num)
{


  //Registers to save in.bits for every port
  //val regs = Reg(Vec(num,dtype))

  val idest= RegInit(UInt(num.W),0.U)
 // val nextidest = Wire(UInt(num.W))


  //one-entry FIFO for every output port
  val queue = for(i <- 0 until  num) yield Module(new Queue(dtype, 1))

  idest     := Mux(in.fire, dest, idest)

  //instantiate Combo Distributor
  val combo = Module(new ComboDistributor(dtype,num))
//  //connect output of Queue to Combo Distributor
//  combo.in <> queue.io.deq

  combo.in  <> in
  combo.dest <> dest


  //defaults
  for(i <- 0 until  num)
  {
    queue(i).io.enq.valid :=  0.B
    queue(i).io.enq.bits :=  0.U
    out(i).valid := false.B
    out(i).bits := 0.U

    combo.out(i) <> queue(i).io.enq
    out(i) <> queue(i).io.deq
  }

  for(i <- 0 until  num)
    {
      when(combo.out(i).valid) //enqueue and then deque after 1-cycle of latency all those destinations that are done
      {
          combo.out(i) <> queue(i).io.enq
          queue(i).io.deq <> out(i)

          //update the allreadyVec on the next clock cycle
          when( (dest(i) === out(i).ready)  ) { //when new destination is presented at the input that matches allready or less than
            in.ready := true.B
            idest := dest

          }
      }
    }
//  }


} //end of FullDistributor class




object Distributor {
  def apply[D <: Data](imp : String, dtype : D, num : Int) : Distributor[D] = {
    imp match {
      case "combo" => new ComboDistributor(dtype, num)
      case "full" => new FullDistributor(dtype, num)
      case _ => new RegDistributor(dtype, num)
    }
  }

  def getImpTypes : Seq[String] = Seq("full")
}


object GenDistributor extends App
{
  val baseArguments = Array("--strip-debug-info", "--split-verilog", "--disable-all-randomization", "--lowering-options=disallowExpressionInliningInPorts,disallowLocalVariables")


  //
  //  ChiselStage.emitSystemVerilogFile(new Exercise4, Array.empty, baseArguments)
  emitVerilog(new FullDistributor[UInt](UInt(8.W),4))    //use sbt run from the command line to get verilog

}
