package scratchpad

import chisel3._
import chisel3.util._
import exercise3.DistributorCrossbar

//packet = destination MAC address + payload data
class myPacket extends Bundle {
  val length = UInt(8.W) //length of the packet in bytes
  val data = Vec(64, UInt(8.W)) //max packet size is 64 bytes
}

class packetHeader extends Bundle {
  val da = UInt(48.W)   //destination MAC address is 6 bytes = 48 bits
}


//1 bit per port rather than the encoded value.
//0 means no port selected, 15 means all 4 ports selected
class LookupResult(numPorts: Int) extends Bundle {
  val destPort = UInt(numPorts.W)  //for 4 output ports switch, if numPorts=4, destPort is 4 bits wide
                                  // and can represent 16 combinations
}
class packetParser extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new myPacket))
    val outHeader = Decoupled(new packetHeader) //contains destination MAC address
    val outData = Decoupled(new myPacket)
  })
  io.outHeader.bits.da := Cat(io.in.bits.data.slice(0, 6)) //the first 6 bytes are header

  //No need to buffer data as we have the whole packet
  //This is not the case when the data is streaming like AXI stream



  //Defaults

  io.outData.bits := io.in.bits
  io.outData.valid := false.B
  io.in.ready := false.B
  io.outHeader.valid := false.B

  //We cannot do io.out <> io.in because we have 3 different interfaces
  //One incoming, two outgoings
  //We need big AND gate to combine the ready/valid signals
  //AND(incoming valid,incoming readys) == ougoing valids/outgoing readys
  when(io.in.valid && io.outHeader.ready && io.outData.ready) {
    io.outData.valid := true.B
    io.in.ready := true.B
    io.outHeader.valid := true.B
  }
} //end of packetParser

class LookUpTable extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new packetHeader)) //contains destination MAC address
    val out = Decoupled(new LookupResult(4))  //for 4 output ports switch


  })

  //Defaults
  io.in.ready := false.B
  io.out.valid := false.B
  io.out.bits.destPort := 0.U



  when(io.in.valid) {
    io.out.bits.destPort := MuxLookup(io.in.bits.da, 0.U(4.W))(Seq(
      "h000000000000".U(48.W) -> 0x0.U(4.W), //port 0
      "h000000000001".U(48.W) -> 0x1.U(4.W), //port1
      "h000000000002".U(48.W) -> 0x2.U(4.W), //port2
      "h000000000003".U(48.W) -> 0x3.U(4.W) //port3
    )) }


  val q = Module (new Queue(UInt(4.W),1))
  q.io.enq.bits := io.out.bits.destPort
  io.in.ready := q.io.enq.ready
  q.io.enq.valid := io.in.valid

  //These two signals need to be out of above when(io.in.valid) block to be helpful.
  /*io.out.valid := io.in.valid
  io.in.ready := io.out.ready
*/
  io.out.valid := q.io.deq.valid
  io.out.bits.destPort := q.io.deq.bits
  q.io.deq.ready := io.out.ready


} //end of LookUpTable class


class PacketForwarder extends Module {
  val io = IO(new Bundle {
      val in = Flipped(Decoupled(new myPacket))
      val destPort = Flipped(Decoupled(UInt(4.W))) //from LookupResult
      val out = Decoupled(new myPacket)
      val outPort = Output(UInt(4.W)) //indicates which output port the packet is sent to
  })

  //defaults
  io.out.bits := io.in.bits
  io.out.valid := false.B
  io.in.ready := false.B
  io.destPort.ready := false.B
  io.outPort := 0.U

  //We cannot do io.out <> io.in because we have 3 different interfaces
  //Two incomings, One outgoing
  //We need big AND gate to combine the ready/valid signals
  //outgoingValid, outgoingReady = AND(incomingValids, incomingReadys)
  when(io.in.valid && io.destPort.valid && io.out.ready) {
    io.out.valid := true.B
    io.outPort := io.destPort.bits
    io.in.ready := true.B
    io.destPort.ready := true.B
  }

} //end of PacketForwarder class


//create combiner class that combines LookupResult and myPacket

class TopLogic(numPorts: Int) extends Module {
  val io = IO(new Bundle {
    val in =  Vec(numPorts,Flipped(Decoupled(new myPacket))) //e.g, 4 input ports
    val out = Vec(numPorts, Decoupled(new myPacket)) //e.g, 4 output ports
  })


  //instantiate 4 queues, 4 parsers, 4 LUTs, and 4 forwarders
  val queues = for (i <- 0 until numPorts) yield Module(new Queue(new myPacket, 2))
  val parsers = for (i <- 0 until numPorts) yield Module (new packetParser)
  val luts = for( i <- 0 until numPorts) yield Module (new LookUpTable)
  val forwarders = for (i <- 0 until numPorts) yield Module (new PacketForwarder)

  //instantiate "one" 4x4 crossbar
  val crossbar = Module (new DistributorCrossbar(new myPacket,numPorts,numPorts))


  //defaults
for(i <-0 until numPorts) {
   io.in(i).ready := false.B
   io.out(i).valid := false.B
   io.out(i).bits := 0.U.asTypeOf(new myPacket)
  }


  //conect
  for(i <-0 until numPorts) {
    io.in(i) <> parsers(i).io.in
    parsers(i).io.outData <> queues(i).io.enq
    parsers(i).io.outHeader <> luts(i).io.in
    forwarders(i).io.destPort.valid := luts(i).io.out.valid
    forwarders(i).io.destPort.bits := luts(i).io.out.bits.destPort
    luts(i).io.out.ready := forwarders(i).io.destPort.ready
    queues(i).io.deq <> forwarders(i).io.in
    forwarders(i).io.out <> crossbar.in(i)
    crossbar.dest(i) := forwarders(i).io.outPort
    crossbar.out(i) <> io.out(i)

    }

} //end of TopLogic class

