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
// 4 bytes have been accumulated in a tran  saction , you assert valid output.
//Corner case is if tlast is asserted, then you need to flush out the output no matter what.

class WrapBusUpsize(inWidth: Int, outWidth:Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AxiInterfaceBits(inWidth))) // Input bus
    val out = Decoupled(new AxiInterfaceBits(outWidth)) // Output bus
  })

  val upsize = Module(new BusUpsize(inWidth, outWidth))
  val outq = Module(new Queue(new AxiInterfaceBits(outWidth), 1)) // Output queue of depth 2 (DISCUSS ???)

  upsize.io.in <> io.in
  outq.io.enq <> upsize.io.out
  io.out <> outq.io.deq
}

class BusUpsize(inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AxiInterfaceBits(inWidth)))
    val out = Decoupled(new AxiInterfaceBits(outWidth))
  })

  require(
    (inWidth < outWidth && outWidth % inWidth == 0),
    "For upsizing, outWidth must be a multiple of inWidth."
  )

  val ratio = outWidth / inWidth

  if(ratio == 1) {
    io.in <> io.out //if ratio is 1, then no need to upsize, just pass the input to output
  } else {
    //if ratio > 1, then we need to save incoming tokens in registers and send them out when we have enough tokens
    //to send out
    //so we need to save ratio number of tokens in registers
    //and send them out when we have enough tokens


    val registerArray = Reg(Vec(ratio, UInt((inWidth * 8).W))) //to save each incoming token's in.data which is inWidth*8 bits
    val keepArray = Reg(Vec(ratio, UInt(inWidth.W))) //to save each incoming in.keep which is inWidth bit.
    // If inWidth=1 (byte), in.keep is 1 bit
    val ctr = RegInit(0.U(log2Ceil(ratio).W)) //to keep track of incoming tokens in a tx

    val tlastSeen = RegInit(Bool(), 0.B)
    val txDone = RegInit(Bool(), 0.B)

    //defaults
    io.out.valid := false.B
    io.out.bits.tdata := 0.U
    io.out.bits.tkeep := 0.U
    io.out.bits.tlast := false.B
    io.in.ready := !txDone || (txDone && io.out.ready)

    when(io.in.fire) {
      registerArray(ctr) := io.in.bits.tdata //0->a,
      // 1->b,
      // 2->c,
      keepArray(ctr) := io.in.bits.tkeep
      ctr := ctr + 1.U
      when((ctr === ratio.U - 1.U) || io.in.bits.tlast) {
        // ctr := 0.U    //cannot set the counter to 0 when io.bits.tlast is asserted, because ctr might not be equal to ratio-1
           tlastSeen := io.in.bits.tlast
           txDone := true.B
        //      printf(p"\t Inside fire ratio=$ratio, ctr=$ctr \n")
      }
    }

    //in the next cycle, get the tx out and in the meanwhile keep reading new tx at the input
    when(txDone) {
      io.out.valid := txDone
      io.out.bits.tdata := Cat(registerArray.reverse)
      io.out.bits.tkeep := Cat(keepArray.reverse)
      io.out.bits.tlast := tlastSeen
      when(io.out.ready) {
        for (i <- 0 until ratio) {
          when(i.U > ctr) { //if tlast came in the middle and ctr could not reach ratio-1 so restting the
            // remaining registers otherwise one will get 100F0E0D0C131211 instead of 00000000131211

            registerArray(i) := 0.U
            keepArray(i) := 0.U
          }
        }

        when(tlastSeen) {
          tlastSeen := false.B
          txDone := false.B
          ctr := 0.U
        }.elsewhen(!io.in.bits.tlast) {
          txDone := false.B
        }
      }
    }
  } //end of else
} //end of BusUpsize class

//Example of BusDownsize, if inWidth = 8, outWidth = 2,

class WrapBusDownsize(inWidth: Int, outWidth:Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AxiInterfaceBits(inWidth))) // Input bus
    val out = Decoupled(new AxiInterfaceBits(outWidth)) // Output bus
  })

  val downsize = Module(new BusDownsize(inWidth, outWidth))
  val outq = Module(new Queue(new AxiInterfaceBits(outWidth), 2)) // Output queue of depth 2 (DISCUSS ???)

  downsize.io.in <> io.in
  outq.io.enq <> downsize.io.out
  io.out <> outq.io.deq
}

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

  if(ratio == 1) {
    io.in <> io.out //if ratio is 1, then no need to downsize, just pass the input to output
  } else {

    val ctr = RegInit(0.U(log2Ceil(ratio).W)) //to keep track of how many outgoing items, if (8,2) then ratio=4
    val popCtr = Reg(UInt(log2Ceil(ratio).W)) //
    val saveOutReg = RegInit(0.U((inWidth * 8).W)) //to save incoming data inWidth bytes
    val saveOutKeep = RegInit(0.U(inWidth.W)) //to save incoming keep inwidth bits
    val byteOutWire = Wire(UInt((outWidth * 8).W)) //that holds outwidth bytes chunk of inWidth bytes
    val keepOutWire = Wire(UInt(outWidth.W)) // that holds outwidth bits of inWidth bits
    val txDone = RegInit(Bool(), 0.B) //that keeps track of when outgoing outWidth data is ready to be sent
    val tlastSeen = RegInit(Bool(), 0.B) // Register that keeps track of incoming inWidth data tlast


    //defaults
    io.out.valid := false.B
    io.out.bits.tdata := 0.U
    io.out.bits.tkeep := 0.U
    io.out.bits.tlast := false.B
    byteOutWire := 0.U
    keepOutWire := 0.U


    io.in.ready := !txDone // || (ctr ===(ratio-1).U && io.out.ready) //|| !io.in.bits.tlast //true.B //!tlastSeen //&& io.in.valid // Discuss

    when(io.in.fire) {
      saveOutReg := io.in.bits.tdata(inWidth * 8 - 1, outWidth * 8)
      saveOutKeep := io.in.bits.tkeep(inWidth - 1, outWidth)
      txDone := true.B
      tlastSeen := io.in.bits.tlast
      //send one outWidth bytes in this cycle right away
      io.out.valid := true.B
      io.out.bits.tdata := io.in.bits.tdata(outWidth * 8 - 1, 0) //get the first outWidth bytes
      io.out.bits.tkeep := io.in.bits.tkeep(outWidth - 1, 0) //get the first outWidth bits
      when(io.in.bits.tlast) {
        val currentPopCount = PopCount(io.in.bits.tkeep(inWidth - 1, outWidth))
        popCtr := currentPopCount
        when(currentPopCount === 0.U) {
          io.out.bits.tlast := true.B
        }
      }.otherwise {
        ctr := ctr + 1.U
      }
    }

    //in the next cycle,
    when(txDone) {
      io.out.valid := txDone
      byteOutWire := saveOutReg(outWidth * 8 - 1, 0)
      keepOutWire := saveOutKeep(outWidth - 1, 0)
      //    printf(p"\t Inside, saveReg=$saveReg, byteReg=$byteWire, keepReg=$keepWire \n")
      io.out.bits.tdata := byteOutWire
      saveOutReg := saveOutReg >> (outWidth * 8)
      io.out.bits.tkeep := keepOutWire
      saveOutKeep := saveOutKeep >> (outWidth)
      when(!tlastSeen) {
        ctr := ctr + 1.U
      }
      when(io.out.ready) {
        when(ctr === (ratio - 1).U && !tlastSeen) {
          txDone := ~txDone
        }.elsewhen(tlastSeen) { //if tlast is seen, then send the last outWidth bytes,
          //                                //send ctr number of outWidth bytes
          when(popCtr === 0.U) {
            txDone := ~txDone
            tlastSeen := false.B
          }.elsewhen(popCtr === 1.U) {
            io.out.bits.tlast := true.B
            txDone := ~txDone
            tlastSeen := false.B
          }.otherwise {
            popCtr := popCtr - 1.U
          }
        }
      }
    }
  } //end of else

} //end of BusDownSize class


class BusDownsize2(inWidth: Int, outWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AxiInterfaceBits(inWidth)))
    val out = Decoupled(new AxiInterfaceBits(outWidth))
  })

  require(
    ( (inWidth > outWidth) && (inWidth % outWidth == 0) ),
    "For downsizing, inWidth must be a multiple of outWidth."
  )

  val ratio = inWidth / outWidth



} //end of BusDownSize2 class

