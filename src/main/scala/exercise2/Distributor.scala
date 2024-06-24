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
abstract class Distributor[D <: Data](dtype : D, num : Int) extends Module
{
  val in = IO(Flipped(Decoupled(dtype)))      //new style chisel
  val dest = IO(Input(UInt(num.W)))
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

   val  iData = Reg(dtype)
   val  idest = Reg(UInt(num.W))
   val  nextidest = Wire(UInt(num.W))
   val  allready = Reverse(Cat(for(i<- 0 until num)  yield out(i).ready)) //yields seq of all ready signals

    iData := Mux(in.fire,in.bits,iData)
    idest := Mux(in.fire, dest, nextidest)
    in.ready := (idest === 0.U)
    for(i <- 0 until  num)
      {
        out(i).valid := idest(i)
        out(i).bits := iData

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

}


//to select between multiple implementations
object Distributor {
  def apply[D <: Data](imp : String, dtype : D, num : Int) : Distributor[D] = {
    imp match {
      case _ => new RegDistributor(dtype, num)
    }
  }

  def getImpTypes : Seq[String] = Seq("reg")
}