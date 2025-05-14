package exercise4
import chisel3._
import chisel3.util._

import scala.language.postfixOps

class AxiInterfaceBits(width: Int) extends Bundle{
  val tdata = UInt((width*8).W)
  val tkeep = UInt(width.W)
  val tlast = Bool()

  def getBusWidth = width
}

//converts inWidth to outWidth (inWidth could be smaller (for Upsize) or greater (Downsize) than outWidth)
//Assumption: outWidth must be a multiple of inWidth
//Transaction can span multiple cycles
//Token (or Flit) is a single cycle transaction so a packet consists of one or more tokens including the token that has tlast
//asserted


//Example, if inWidth = 2, outWidth = 4, then save inputs into 2 registers, when
// 4 bytes are accumulated because of a transaction , you assert valid output.
//Corner case is if tlast is asserted, then you need to flush out the output no matter what.

class BusUpsize(inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AxiInterfaceBits(inWidth)))
    val out = Decoupled(new AxiInterfaceBits(outWidth))
  })

  require(
    (inWidth < outWidth && outWidth % inWidth == 0),
    "For upsizing, outWidth must be a multiple of inWidth."
  )

  val ratio =  outWidth / inWidth

  val registerArray = Reg(Vec(ratio, UInt((inWidth*8).W)))  //to save each incoming token in.data which is inWidth*8 bits
  val keepArray     = Reg(Vec(ratio, UInt(inWidth.W)))      //to save each incoming in.keep which is inWidth bit. If inWidth=1, in.keep is 1 bit
  val ctr = RegInit(0.U(log2Ceil(ratio).W)) //to keep track of incoming items
  val tlastSeen = RegInit(Bool(),0.B)
  val txDone = RegInit(Bool(),0.B)

  //defaults
  io.out.valid := false.B
  io.out.bits.tdata := 0.U
  io.out.bits.tkeep := 0.U
  io.out.bits.tlast := false.B
  io.in.ready := true.B


//  io.in.ready := !tlastSeen //(ctr =/= ratio.U) //CHECK CHECK CHECK



    when(io.in.fire && (!io.in.bits.tlast) && (ctr < (ratio.U) )) {
      registerArray(ctr) := io.in.bits.tdata        //0->a, 1->b, 2->c, 3->d
      keepArray(ctr) := io.in.bits.tkeep
      ctr := ctr + 1.U
//      printf(p"\t Inside fire ratio=$ratio, ctr=$ctr \n")
    }

    when(ctr === ratio.U - 1.U){

      txDone := true.B
    }

    when( txDone )
    {
      io.out.valid := txDone
      when(io.out.ready)
      {
        io.out.bits.tdata := Cat(registerArray.reverse)
        io.out.bits.tkeep := Cat(keepArray.reverse)
      }

      //reset registerArray and keepArray from index 1 as you are reading into
      // the registerArray[0] in the above thread

      for( i <-1 until ratio )
      {
        registerArray(i) := 0.U
        keepArray(i) := 0.U
      }
    txDone := ~txDone
  }

  when(io.in.bits.tlast && io.in.fire) {
    tlastSeen := true.B
    registerArray(ctr) := io.in.bits.tdata
    keepArray(ctr) := io.in.bits.tkeep
    ctr := 0.U
//    printf(p"\t Inside tlastSeen ratio=$ratio, ctr=$ctr \n")
  }

     when(tlastSeen){       //in the next cycle after io.in.bits.tlast
      io.out.valid := tlastSeen
      io.out.bits.tlast := tlastSeen
      when(io.out.ready)
      {
         io.out.bits.tdata := Cat(registerArray.reverse)
         io.out.bits.tkeep := Cat(keepArray.reverse)
      }
       //reset registerArray and keepArray
       for( i <-0 until ratio )
         {
           registerArray(i) := 0.U
           keepArray(i) := 0.U
         }

       tlastSeen := ~tlastSeen
     }




} //end of BusUpsize class


class BusDownsize(inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AxiInterfaceBits(inWidth)))
    val out = Decoupled(new AxiInterfaceBits(outWidth))
  })

  require(
    (inWidth > outWidth && inWidth % outWidth == 0),
    "For downsizing, inWidth must be a multiple of outWidth."
  )



  val ratio = inWidth / outWidth

  val ctr = RegInit(0.U(log2Ceil(ratio).W)) //to keep track of outgoing items
  val saveReg = RegInit(0.U((inWidth*8).W))
  val saveKeep = RegInit(0.U(inWidth.W))
  val byteWire = Wire(UInt((outWidth*8).W))
  val keepWire = Wire(UInt(outWidth.W))
  val tlastSeen = RegInit(Bool(),0.B)


  //defaults
  io.out.valid := false.B
  io.out.bits.tdata := 0.U
  io.out.bits.tkeep := 0.U
  io.out.bits.tlast := false.B
  byteWire := 0.U
  keepWire := 0.U


  when(io.in.fire && io.in.bits.tlast){
    saveReg := io.in.bits.tdata
    saveKeep := io.in.bits.tkeep
    tlastSeen := true.B
  }

  when(tlastSeen){
    io.out.valid := tlastSeen
//    for (i <-0 until(ratio)){ //not the parallel way

      when(io.out.ready){
      byteWire := saveReg(7,0)
      keepWire := saveKeep(0)
//    printf(p"\t Inside, saveReg=$saveReg, byteReg=$byteWire, keepReg=$keepWire \n")
      io.out.bits.tdata := byteWire
      saveReg := saveReg >> (outWidth*8)
      io.out.bits.tkeep := keepWire
      saveKeep:= saveKeep >> (outWidth)
      ctr := ctr + 1.U
    }
    when(ctr === ratio.U -1.U) {
      io.out.bits.tlast := true.B
      tlastSeen := false.B
      ctr := 0.U
    }
  }

  io.in.ready := !tlastSeen

} //end of BusDownSize class




