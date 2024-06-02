package exercise2

import chisel3._
import chisel3.util.ImplicitConversions.intToUInt
import chisel3.util._

/** Receives an incoming atom and replicates it across num interfaces
 *
 * @param dtype Atom data type
 * @param num   Number of output interfaces
 */
abstract class Distributor[D <: Data](dtype : D, num : Int) extends Module {
  val in = IO(Flipped(Decoupled(dtype)))
  val dest = IO(Input(UInt(num.W)))
  val out = IO(Vec(num, Decoupled(dtype)))
}

/** Distributor with registered data
 *
 */
class RegDistributor[D <: Data](dtype : D, num : Int) extends Distributor(dtype, num) {
  // Create your implementation of distributor here
}

object Distributor {
  def apply[D <: Data](imp : String, dtype : D, num : Int) : Distributor[D] = {
    imp match {
      case _ => new RegDistributor(dtype, num)
    }
  }

  def getImpTypes : Seq[String] = Seq("reg", "combo")
}