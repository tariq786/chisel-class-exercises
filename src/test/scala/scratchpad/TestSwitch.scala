package scratchpad

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class TestSwitch extends  AnyFreeSpec with ChiselScalatestTester {
  "test_switch" in {
    val numPorts = 4
    test(new TopLogic(numPorts)).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut => {
        // Set up clock domains for Decoupled interfaces
        dut.io.in(0).setSourceClock(dut.clock)
        dut.io.in(1).setSourceClock(dut.clock)
        dut.io.in(2).setSourceClock(dut.clock)
        dut.io.in(3).setSourceClock(dut.clock)
        dut.io.out(0).setSinkClock(dut.clock)
        dut.io.out(1).setSinkClock(dut.clock)
        dut.io.out(2).setSinkClock(dut.clock)
        dut.io.out(3).setSinkClock(dut.clock)

        // Set all "output" ports ready from the start to accept input
        dut.io.out(0).ready.poke(true.B)
        dut.io.out(1).ready.poke(true.B)
        dut.io.out(2).ready.poke(true.B)
        dut.io.out(3).ready.poke(true.B)

        // Test packets with valid MAC addresses that map to different ports
        // MAC 0x000000000000 -> port 0
        // MAC 0x000000000001 -> port 1
        // MAC 0x000000000002 -> port 2
        // MAC 0x000000000003 -> port 3

        // Test 1: send traffic by directly driving signals
        fork {
          // Drive valid high to send

          // Send packets to all input ports
          for (i <- 0 until numPorts) {
            dut.io.in(i).valid.poke(true.B)
            // Set up packet data - MAC address in bytes 0-5
            dut.io.in(i).bits.length.poke(64.U)
            for (j <- 0 until 64) {
              if (j == 5) dut.io.in(i).bits.data(j).poke(i.U(8.W))  // Last byte of MAC = port number
              else dut.io.in(i).bits.data(j).poke(0.U(8.W))
            }

            // Wait for ready handshake
            while (!dut.io.in(i).ready.peek().litToBoolean) {
              dut.clock.step(1)
            }
            dut.clock.step(1)
            // Deassert valid
            dut.io.in(i).valid.poke(false.B)
          }
          dut.clock.step(10)  // Give time for packet to traverse the pipeline
        }.fork {
          // Receive packets on all output ports
          for (i <- 0 until numPorts) {
            // Wait for valid on output port i
            while (!dut.io.out(i).valid.peek().litToBoolean) {
              dut.clock.step(1)
            }
            // Check packet data
            dut.io.out(i).bits.length.expect(64.U)
            for (j <- 0 until 64) {
              if (j == 5) dut.io.out(i).bits.data(j).expect(i.U(8.W))
              else dut.io.out(i).bits.data(j).expect(0.U(8.W))
            }
            dut.clock.step(1)
          }
          dut.clock.step(5)
        }.join()
      }

    }
  }
}//end of TestSwitch class
