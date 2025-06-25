package exercise4

import chisel3._
import chisel3.util._

import scala.language.postfixOps

class AxiInterfaceBits(width: Int) extends Bundle {
  val tdata = UInt((width * 8).W) //width byte wide
  val tkeep = UInt(width.W) //per byte, if tdata is 1 byte, tkeep is 1 bit, if tdata is 4 bytes, tkeep is 4 bits
  val tlast = Bool()      //end of current packet

  def getBusWidth = width
}

//converts inWidth to outWidth (inWidth could be smaller (for Upsize) or greater (Downsize) than outWidth)
//Assumption: outWidth must be a multiple of inWidth
//Transaction can span multiple cycles
//Token (or Flit) is a single cycle transaction so a packet consists of one or more tokens including the token that has
// tlast asserted


//Example of BusUpsize, if inWidth = 2, outWidth = 4, then save inputs into 2 registers, when
// 4 bytes have been accumulated in a transaction , you assert valid output.
//Corner case is if tlast is asserted, then you need to flush out the output no matter what.


class BusUpsize(inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AxiInterfaceBits(inWidth)))
    val out = Decoupled(new AxiInterfaceBits(outWidth))
  })

  require(
    (inWidth <= outWidth && outWidth % inWidth == 0),
    "For upsizing, outWidth must be a multiple of inWidth."
  )

  val ratio = outWidth / inWidth

  val registerArray = Reg(Vec(ratio, UInt((inWidth * 8).W))) //to save each incoming token's in.data which is inWidth*8 bits
  val keepArray = Reg(Vec(ratio, UInt(inWidth.W))) //to save each incoming in.keep which is inWidth bit.
  // If inWidth=1, in.keep is 1 bit
  val ctr = RegInit(0.U(log2Ceil(ratio).W)) //to keep track of incoming tokens in a tx
  val tlastSeen = RegInit(Bool(), 0.B)
  val txDone = RegInit(Bool(), 0.B)

  //defaults
  io.out.valid := false.B
  io.out.bits.tdata := 0.U
  io.out.bits.tkeep := 0.U
  io.out.bits.tlast := false.B
  io.in.ready := !txDone || (txDone && io.out.ready)

  //  io.in.ready := !tlastSeen //(ctr =/= ratio.U) //CHECK CHECK CHECK


  //Case 1: tlast=false and ctr < ratio meaning cannot send an upsized tx yet
  when(io.in.fire && (!io.in.bits.tlast) && (ctr < (ratio.U))) {
    registerArray(ctr) := io.in.bits.tdata //0->a, 1->b, 2->c, 3->d
    keepArray(ctr) := io.in.bits.tkeep
    ctr := ctr + 1.U
    //      printf(p"\t Inside fire ratio=$ratio, ctr=$ctr \n")
  }

  //when tx is done (independent check). ctr will wrap around to 0 in the next cycle
  when(ctr === ratio.U - 1.U) {
    ctr := 0.U    //explicitly reset the counter to 0
    txDone := true.B
  }

  //in the next cycle, get the tx out and in the meanwhile keep reading new tx at the input
  when(txDone) {
    io.out.valid := txDone
    io.out.bits.tdata := Cat(registerArray.reverse)
    io.out.bits.tkeep := Cat(keepArray.reverse)
    when(io.out.ready) {
      for (i <- 1 until ratio) {
        registerArray(i) := 0.U
        keepArray(i) := 0.U
       }
      txDone := ~txDone
    }
  }

  //Case2: tlast = true. Since tlast can come anytime, so reset the ctr immediately.
  when(io.in.bits.tlast && io.in.fire) {
    tlastSeen := true.B
    registerArray(ctr) := io.in.bits.tdata
    keepArray(ctr) := io.in.bits.tkeep
    ctr := 0.U
    //    printf(p"\t Inside tlastSeen ratio=$ratio, ctr=$ctr \n")
  }

  when(tlastSeen) { //in the next cycle after tlastSeen
    io.out.valid := tlastSeen
    io.out.bits.tlast := tlastSeen
    io.out.bits.tdata := Cat(registerArray.reverse)
    io.out.bits.tkeep := Cat(keepArray.reverse)
    when(io.out.ready) {
      //reset registerArray and keepArray. In this cycle, valid is low !!!
      for (i <- 0 until ratio) {
        registerArray(i) := 0.U
        keepArray(i) := 0.U
      }

      tlastSeen := ~tlastSeen
    }

  }

} //end of BusUpsize class

//Example of BusDownsize, if inWidth = 8, outWidth = 2,


class BusDownsize(inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AxiInterfaceBits(inWidth)))
    val out = Decoupled(new AxiInterfaceBits(outWidth))
  })

  require(
    ( (inWidth >= outWidth) && (inWidth % outWidth == 0) ),
    "For downsizing, inWidth must be a multiple of outWidth."
  )


  val ratio = inWidth / outWidth

  val ctr = RegInit(0.U(inWidth.W))             //to keep track of outgoing items
  val saveOutReg = RegInit(0.U((inWidth * 8).W))   //to save incoming data inWidth bytes
  val saveOutKeep = RegInit(0.U(inWidth.W))        //to save incoming keep inwidth bits
  val byteOutWire = Wire(UInt((outWidth * 8).W))   //that holds outwidth bytes chunk of inWidth bytes
  val keepOutWire = Wire(UInt(outWidth.W))        // that holds outwidth bits of inWidth bits
  val txDone = RegInit(Bool(), 0.B)           //that keeps track of when outgoing outWidth data is ready to be sent
  val tlastSeen = RegInit(Bool(), 0.B)        // Register that keeps track of incoming inWidth data tlast


  //defaults
  io.out.valid := false.B
  io.out.bits.tdata := 0.U
  io.out.bits.tkeep := 0.U
  io.out.bits.tlast := false.B
  byteOutWire := 0.U
  keepOutWire := 0.U


  io.in.ready := !txDone || (txDone && io.out.ready) //|| !io.in.bits.tlast //true.B //!tlastSeen //&& io.in.valid // Discuss

  //Case1: tlast=false
  when(io.in.fire && !io.in.bits.tlast) {
    saveOutReg := io.in.bits.tdata
    saveOutKeep := io.in.bits.tkeep
    txDone := true.B
  }

  //in the next cycle, ???get the tx out WHILE getting the new tx in
  when(txDone) {
    io.out.valid := txDone
    byteOutWire := saveOutReg(outWidth * 8 - 1, 0)
    keepOutWire := saveOutKeep(outWidth - 1, 0)
    //    printf(p"\t Inside, saveReg=$saveReg, byteReg=$byteWire, keepReg=$keepWire \n")
    io.out.bits.tdata := byteOutWire
    saveOutReg := saveOutReg >> (outWidth * 8)
    io.out.bits.tkeep := keepOutWire
    saveOutKeep := saveOutKeep >> (outWidth)

    when(io.out.ready) {
      txDone := ~txDone
    }
  }

  //Case2: tlast=true
  when(io.in.fire && io.in.bits.tlast) {
    saveOutReg := io.in.bits.tdata
    saveOutKeep := io.in.bits.tkeep
    tlastSeen := true.B
  }

  when(tlastSeen) {
    io.out.valid := true.B
    byteOutWire := saveOutReg(outWidth*8-1, 0)
    keepOutWire := saveOutKeep(outWidth-1,0)
    //    printf(p"\t Inside, saveReg=$saveReg, byteReg=$byteWire, keepReg=$keepWire \n")
    io.out.bits.tdata := byteOutWire
    saveOutReg := saveOutReg >> (outWidth * 8)
    io.out.bits.tkeep := keepOutWire
    saveOutKeep := saveOutKeep >> (outWidth)

    //    for (i <-0 until(ratio)){ //not the parallel way
    when(io.out.ready) {
        io.out.bits.tlast := true.B
        tlastSeen := false.B

      }
  }


} //end of BusDownSize class




