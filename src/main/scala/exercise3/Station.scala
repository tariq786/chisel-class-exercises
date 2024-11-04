package exercise3
import chisel3._
import chisel3.util._
import exercise2.{RegDistributor}

class  Station_Data[D <: Data] (dtype : D, numOut : Int) extends Bundle{
  val sdest = UInt(numOut.W)
  val sData = (dtype)
}

class Station [D <: Data] (dtype : D, numIn : Int, numOut : Int) extends   Module
{
  val in = IO(Flipped(Decoupled(dtype)))
  val dest = IO(Input(UInt(numOut.W)))
  val out = IO(Decoupled(dtype))
  val ringDataIn  = IO(Flipped(Decoupled(new Station_Data(dtype,numOut))))
  val ringDataOut = IO(Decoupled(new Station_Data(dtype,numOut)))

  //Instantiate RegDistributor
  val regD = Module (new RegDistributor[D](dtype,numOut))
  regD.dest     := ringDataIn.bits.sdest
  regD.in.bits  := ringDataIn.bits.sData
  regD.in.ready := ringDataIn.ready
  regD.in.valid := ringDataIn.valid
  regD.out  <> out

  //Instantiate RRArbiter which has 2 inputs
  val arb = Module(new RRArbiter(dtype, 2))
  arb.io.in(0) <> in
  arb.io.in(1).bits  := ringDataIn.bits.sData
  arb.io.in(1).valid := ringDataIn.valid
  arb.io.in(1).ready := ringDataIn.ready


  //Instantiate One Entry Queue
  val queue = Module(new Queue(dtype, 1))
  queue.io.enq           <> arb.io.out
  queue.io.deq.bits      := ringDataOut.bits.sData
  queue.io.deq.valid     := ringDataOut.valid
  queue.io.deq.ready     := ringDataOut.ready
  ringDataOut.bits.sData := dest
} //end of Station class
