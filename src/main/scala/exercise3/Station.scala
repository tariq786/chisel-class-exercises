package exercise3
import chisel3._
import chisel3.util._
import exercise2.RegDistributor

class  Station_Data[D <: Data] (dtype : D, numOut : Int) extends Bundle{
  val sdest = UInt(numOut.W)
  val sData = dtype
}

class Station [D <: Data] (dtype : D, numIn : Int, numOut : Int, stationID: Int) extends   Module
{
  val in = IO(Flipped(Decoupled(dtype)))
  val dest = IO(Input(UInt(numOut.W)))
  val out = IO(Decoupled(dtype))
  val ringDataIn  = IO(Flipped(Decoupled(new Station_Data(dtype,numOut)))) //Decoupled creates a new class
  val ringDataOut = IO(Decoupled(new Station_Data(dtype,numOut)))

//  println(s"inside station class line 19")
  //Instantiate RegDistributor with two output ports
  val regD = Module (new RegDistributor(new Station_Data(dtype,numOut),2))
  //Instantiate RRArbiter which has 2 inputs
  val arb = Module(new RRArbiter(new Station_Data(dtype,numOut), 2))
  //Instantiate One Entry Queue
  val queue = Module(new Queue(new Station_Data(dtype,numOut), 1))

  val reg_dest = Wire(Vec(2,Bool()))
  reg_dest(0) := ringDataIn.bits.sdest(stationID) //decides whether data exits and/or continues in the ring
  regD.dest := reg_dest.asUInt

  //Doing ringDataIn.bits.sdest(stationID) := 0.B
  val rdi_sdest = ringDataIn.bits.sdest.asBools
  val rdo_sdest = Wire(Vec(numOut,Bool()))
  for(i <- 0 until numOut)
    if (i != stationID)
      rdo_sdest(i) := rdi_sdest(i)
    else rdo_sdest(i) := 0.B

  reg_dest(1) := rdo_sdest.asUInt =/= 0.U   //reg_dest(1) is single bit indicating continue on the ring or not

  regD.in <> ringDataIn
  out.valid := regD.out(0).valid
  out.bits := regD.out(0).bits.sData
  regD.out(0).ready := out.ready  // you assign to outputs
  regD.out(1) <> arb.io.in(0)
  arb.io.in(0).bits.sdest := rdo_sdest.asUInt   //override bulk connect

  arb.io.in(1).bits.sdest := dest
  arb.io.in(1).bits.sData := in.bits
  arb.io.in(1).valid := in.valid
  in.ready := arb.io.in(1).ready //you assign to outputs

  queue.io.enq  <> arb.io.out
  ringDataOut <> queue.io.deq
//  println(s"inside station class END")
  
} //end of Station class