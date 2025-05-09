package exercise4

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec

class TestBusSize extends AnyFreeSpec with ChiselScalatestTester with Formal {

  "Upsize" in {
    test(new BusUpsize(1, 4)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        //Transaction 1 with 4 bytes, all valid
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

        // Transcation 1,Input 4 with tlast
        c.io.in.bits.tdata.poke("hdd".U)
        c.io.in.bits.tlast.poke(1)
        c.io.in.ready.expect(true)
        c.clock.step()

        // Verify data is ready to be output
        c.io.in.valid.poke(0)
        c.io.in.ready.expect(false)
        c.io.in.bits.tlast.poke(0)
        c.io.out.valid.expect(true)

        // Accept the output
        c.io.out.ready.poke(1)
        c.io.out.bits.tlast.expect(1)
        c.io.out.bits.tdata.expect("hddccbbaa".U) //(10 << 24 | 11 << 16 |  12 << 8 | 13)
        c.io.out.bits.tkeep.expect("b1111".U)//1 << 3 | 1 << 2 | 1 << 1 | 1)

        c.clock.step()

        // Verify module returns to idle
        c.io.out.valid.expect(0)
        c.io.out.bits.tlast.expect(0)

        //Transaction 2 with only one valid byte
        //Transaction2, 1st input (one byte only) with tlast
        c.io.in.valid.poke(1)
        c.io.in.bits.tdata.poke("hee".U)
        c.io.in.bits.tkeep.poke(1)
        c.io.in.bits.tlast.poke(1)
        c.io.in.ready.expect(true) // Verify DUT is ready
        c.clock.step()

        c.io.in.valid.poke(0)
        c.io.in.ready.expect(false)
        c.io.in.bits.tlast.poke(0)
        c.io.out.valid.expect(true)

        // Accept the output
        c.io.out.ready.poke(1)
        c.io.out.bits.tlast.expect(1)
        c.io.out.bits.tdata.expect("h000000ee".U) //(10 << 24 | 11 << 16 |  12 << 8 | 13)
        c.io.out.bits.tkeep.expect("b0001".U)//1 << 3 | 1 << 2 | 1 << 1 | 1)

        c.clock.step()

        // Verify module returns to idle
        c.io.out.valid.expect(0)
        c.io.out.bits.tlast.expect(0)

        //Transaction3 with 4 bytes, keep is one for only two bytes

        //Transaction 3, input 1
        c.io.in.valid.poke(1)
        c.io.in.bits.tdata.poke("haa".U)
        c.io.in.bits.tkeep.poke(0)
        c.io.in.bits.tlast.poke(0)
        c.io.in.ready.expect(true) // Verify DUT is ready
        c.clock.step()

        // Transcation 3,Input 2
        c.io.in.bits.tdata.poke("hbb".U)
        c.io.in.bits.tkeep.poke(1)
        c.io.in.ready.expect(true)
        c.clock.step()

        // Transcation 3,Input 3
        c.io.in.bits.tdata.poke("hcc".U)
        c.io.in.bits.tkeep.poke(1)
        c.io.in.ready.expect(true)
        c.clock.step()

        // Transcation 3,Input 4 with tlast
        c.io.in.bits.tdata.poke("hdd".U)
        c.io.in.bits.tkeep.poke(0)
        c.io.in.bits.tlast.poke(1)
        c.io.in.ready.expect(true)
        c.clock.step()

        // Verify data is ready to be output
        c.io.in.valid.poke(0)
        c.io.in.ready.expect(false)
        c.io.in.bits.tlast.poke(0)
        c.io.out.valid.expect(true)

        // Accept the output
        c.io.out.ready.poke(1)
        c.io.out.bits.tlast.expect(1)
        c.io.out.bits.tdata.expect("hddccbbaa".U) //(10 << 24 | 11 << 16 |  12 << 8 | 13)
        c.io.out.bits.tkeep.expect("b0110".U)//1 << 3 | 1 << 2 | 1 << 1 | 1)

        c.clock.step()

        // Verify module returns to idle
        c.io.out.valid.expect(0)
        c.io.out.bits.tlast.expect(0)

      }
    }
  }


  "Downsize" in {
    test(new BusDownsize(4, 1)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        //Transaction 1 with 4 bytes, all valid

        c.io.in.valid.poke(1)
        c.io.in.bits.tdata.poke("haabbccdd".U)
        c.io.in.bits.tkeep.poke("b1111".U)
        c.io.in.bits.tlast.poke(1)
        c.io.in.ready.expect(true) // Verify DUT is ready
        c.clock.step()

        c.io.in.bits.tlast.poke(0)
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
