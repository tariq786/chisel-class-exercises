package exercise4

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable.ArrayBuffer

object AxiBusUtil {
  def sendAxiPacket(c: Module, in: DecoupledIO[AxiInterfaceBits], pkt: Seq[Int]) = {
    var count: Int = 0
    //in.bits.code.code.poke(1)
    while (count < pkt.length) {
      var tdata : BigInt = 0
      var tkeep : Int = 0
      in.valid.poke(1)
      for (i <- 0 until in.bits.getBusWidth) {
        if (count < pkt.length) {
          tdata = tdata | (BigInt(pkt(count)) << i*8)
          tkeep = tkeep | (1 << i)
        }
        count += 1
      }
      in.bits.tdata.poke(tdata.U)
      in.bits.tkeep.poke(tkeep.U)
      if (count >= pkt.length) {
        in.bits.tlast.poke(1)
      } else {
        in.bits.tlast.poke(0)
      }
      while (!in.ready.peekBoolean()) c.clock.step()
      c.clock.step()
    }
    in.valid.poke(0)
  }

  def receiveAxiPacket(c: Module, in: DecoupledIO[AxiInterfaceBits]) : Seq[Int] = {
    var done = false
    val packet = new ArrayBuffer[Int]()
    in.ready.poke(1)
    while (!done) {
      while (!in.valid.peekBoolean()) c.clock.step()

      val keep = in.bits.tkeep.peekInt()
      val data = in.bits.tdata.peekInt()
      for (i <- 0 until in.bits.getBusWidth) {
        if ((keep & (1 << i)) != 0) {
          packet.addOne(((data >> (i*8)) & 0xff).toInt)
        }
      }
      done = in.bits.tlast.peekBoolean()
      c.clock.step()
    }
    packet.toSeq
  }
}

class TestBusSize extends AnyFreeSpec with ChiselScalatestTester with Formal {



  "Upsize basic" in {
    test(new BusUpsize(1, 4)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        //Transaction 1 with 6 bytes, all valid
        // Transcation 1,  Input 1
        c.io.in.valid.poke(1)
        c.io.in.bits.tdata.poke("haa".U)
        c.io.in.bits.tkeep.poke(1)
        c.io.in.bits.tlast.poke(0)
        c.io.in.ready.expect(true) // Verify DUT is ready
        c.clock.step()

        // Transcation 1,Input 2
        c.io.in.bits.tdata.poke("hbb".U)
        c.io.in.ready.expect(true)
        c.clock.step()

        // Transcation 1,Input 3
        c.io.in.bits.tdata.poke("hcc".U)
        c.io.in.ready.expect(true)
        c.clock.step()

        // Transcation 1,Input 4
        c.io.in.bits.tdata.poke("hdd".U)
        c.io.in.ready.expect(true)
        c.clock.step()

        // Verify data is ready to be output
        //c.io.in.valid.poke(0)
        //c.io.in.ready.expect(false)
        //c.io.in.bits.tlast.poke(0)
        c.io.out.valid.expect(true)
        // Accept the output
        c.io.out.ready.poke(1)
//        c.io.out.bits.tlast.expect(1)
        c.io.out.bits.tdata.expect("hddccbbaa".U) //(10 << 24 | 11 << 16 |  12 << 8 | 13)
        c.io.out.bits.tkeep.expect("b1111".U)//1 << 3 | 1 << 2 | 1 << 1 | 1)

        // Transcation 1,Input 5
        c.io.in.bits.tdata.poke("hee".U)
        c.io.in.ready.expect(true)
        c.clock.step()

        // Transcation 1,Input 6 (last one)
        c.io.in.bits.tdata.poke("hff".U)
        c.io.in.ready.expect(true)
        c.io.in.bits.tlast.poke(1)
        c.clock.step()


        // Accept the output
        c.io.in.valid.poke(0)
        c.io.in.bits.tlast.poke(0)
        c.io.out.valid.expect(true)
        c.io.out.ready.poke(1)
        c.io.out.bits.tlast.expect(1)
        c.io.out.bits.tdata.expect("hffee".U) //(10 << 24 | 11 << 16 |  12 << 8 | 13)
        c.io.out.bits.tkeep.expect("b11".U)//1 << 3 | 1 << 2 | 1 << 1 | 1)

        c.clock.step()

        // Accept new tx
        c.io.in.valid.poke(1)
        c.io.out.valid.expect(0)
        c.io.out.bits.tlast.expect(0)
        c.io.in.ready.expect(1)

        c.io.in.bits.tdata.poke("haa".U)
        c.io.in.bits.tkeep.poke(1)
        c.io.in.bits.tlast.poke(1)
        c.clock.step()

        // Verify data is ready to be output
        c.io.in.valid.poke(0)
        c.io.in.bits.tlast.poke(0)
        c.io.out.valid.expect(true)

        // Accept the output
        c.io.out.ready.poke(1)
        c.io.out.bits.tlast.expect(1)
        c.io.out.bits.tdata.expect("h000000aa".U) //(10 << 24 | 11 << 16 |  12 << 8 | 13)
        c.io.out.bits.tkeep.expect("b0001".U)//1 << 3 | 1 << 2 | 1 << 1 | 1)

        c.clock.step()

        // Verify module returns to idle
        c.io.in.valid.poke(1)
        c.io.out.valid.expect(0)
        c.io.out.bits.tlast.expect(0)

      }
    }
  }

  "Upsize Packet" in {
    for (inWidth <- Seq(2, 4, 8)) {
      test(new BusUpsize(inWidth, 32)).withAnnotations(Seq(WriteVcdAnnotation)) {
        c => {
          c.clock.step(2)

          val pktIn = Seq.range(1, 20)

          fork {
            AxiBusUtil.sendAxiPacket(c, c.io.in, pktIn)
          }.fork {
            val expPacket = AxiBusUtil.receiveAxiPacket(c, c.io.out)
            assert(expPacket.equals(pktIn))
          }.join()

          c.clock.step(10)
        }
      }
    }
  }

  "Downsize Packet" in {
    for (inWidth <- Seq(8, 4, 2)) {
      test(new BusDownsize(inWidth, 2)).withAnnotations(Seq(WriteVcdAnnotation)) {
        c => {
          c.clock.step(2)

          val pktIn = Seq.range(1, 20)

          fork {
              AxiBusUtil.sendAxiPacket(c, c.io.in, pktIn)
          }.fork {
            val expPacket = AxiBusUtil.receiveAxiPacket(c, c.io.out)
            println(s"expPacket = ${expPacket}")
            assert(expPacket.equals(pktIn))
          }.join()

          c.clock.step(10)
        }
      }
    }
  }


  "Downsize" in {
    test(new BusDownsize(4, 1)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        //Transaction1 with 4 bytes, all valid

        c.io.in.valid.poke(1)
        c.io.in.bits.tdata.poke("haabbccdd".U)
        c.io.in.bits.tkeep.poke("b1111".U)
        c.io.in.bits.tlast.poke(1)
        c.io.in.ready.expect(true) // Verify DUT is ready
        c.clock.step(1)

        //c.io.in.bits.tlast.poke(0)
        c.io.in.valid.poke(0)
        c.io.in.ready.expect(false)
        c.io.out.valid.expect(true)
        c.io.out.ready.poke(1)

//        c.clock.step()
        c.io.out.bits.tdata.expect("hdd".U)
        c.io.out.bits.tkeep.expect(1)
        c.clock.step()

        c.io.out.bits.tdata.expect("hcc".U)
        c.io.out.bits.tkeep.expect(1)
        c.clock.step()

        c.io.out.bits.tdata.expect("hbb".U)
        c.io.out.bits.tkeep.expect(1)
        c.clock.step()

        c.io.out.bits.tdata.expect("haa".U)
        c.io.out.bits.tkeep.expect(1)
        c.io.out.bits.tlast.expect(true)
        c.clock.step()

        c.io.out.ready.poke(0)
        c.io.out.valid.expect(0)
        c.io.out.bits.tlast.expect(0)
        c.io.in.ready.expect(true)
      }
    }
  }



} //end of class
