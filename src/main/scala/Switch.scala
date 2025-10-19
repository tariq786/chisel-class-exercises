package exercise5
package exercise3.DistributorCrossbar

import chisel3._
import chisel3.util._

//packet = destination MAC address + payload data
class myPacket extends Bundle {
  val length = UInt(8.W)
  val data = Vec(64, UInt(8.W))
}

class packetHeader extends Bundle {
  val da = UInt(48.W)
}


//1 bit per port rather than the encoded value.
//0 means no port selected, 15 means all 4 ports selected
class LookupResult(numPorts: Int) extends Bundle {
  val destPort = UInt(numPorts.W)  //for 4 output ports switch
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
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready


} //end of LookUpTable class

//create combiner class that combines LookupResult and myPacket

class TopLogic extends Module {
  val io = IO(new Bundle {
    val in =  Vec(4,Flipped(Decoupled(new myPacket))) //4 input ports
    val out = Vec(4, Decoupled(new myPacket)) //4 output ports
  })

val queue = Module(new Queue(new myPacket, 2))

}

