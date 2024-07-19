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
abstract class Distributor[D <: Data](dtype : D, num : Int) extends Module {
  val in = IO(Flipped(Decoupled(dtype)))
  val dest = IO(Input(UInt(num.W)))
  val out = IO(Vec(num, Decoupled(dtype)))
}

/** Distributor with registered data
 *
 * Implementation which registers incoming data, and therefore has a one-cycle latency
 * on data transmission.  Implementation should be permissive on all interfaces.
 */
class RegDistributor[D <: Data](dtype : D, num : Int) extends Distributor(dtype, num) {
  // Create your implementation of distributor here
}

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
class ComboDistributor[D <: Data](dtype : D, num : Int) extends Distributor(dtype, num) {
  // Create your implementation of distributor here (optional)
}

object Distributor {
  def apply[D <: Data](imp : String, dtype : D, num : Int) : Distributor[D] = {
    imp match {
      case "combo" => new ComboDistributor(dtype, num)
      case _ => new RegDistributor(dtype, num)
    }
  }

  def getImpTypes : Seq[String] = Seq("reg")  // Seq("reg", "combo")
}