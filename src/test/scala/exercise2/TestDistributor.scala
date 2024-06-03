package exercise2

import chiseltest._
import chisel3._
import org.scalatest.freespec.AnyFreeSpec

class TestDistributor extends AnyFreeSpec with ChiselScalatestTester {
  "complete with full throughput" in {
    val ports = 4
    test(Distributor("reg", UInt(8.W), ports)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        // first test with all output ports valid
        c.in.valid.poke(1)
        c.in.bits.poke(0)
        c.dest.poke((1 << ports) - 1)
        c.clock.step()
        for (i <- 1 to 10) {
          c.in.bits.poke(i)
          for (p <- 0 until ports) {
            c.out(p).valid.expect(1)
            c.out(p).bits.expect(i-1)
            c.out(p).ready.poke(1)
          }
          c.in.ready.expect(1.B)
          c.clock.step()
        }

        // next test port by port
        for (p <- 0 until ports) {
          c.in.valid.poke(1)
          c.in.bits.poke(0)
          c.in.ready.expect(1)
          c.dest.poke(1 << p)
          c.clock.step()

          for (i <- 1 to 10) {
            c.in.bits.poke(i)
            for (j <- 0 until ports) {
              if (j == p) {
                c.out(j).valid.expect(1)
                c.out(j).ready.poke(1)
              } else {
                c.out(j).valid.expect(0)
                c.out(j).ready.poke(0)
              }
            }
            c.out(p).bits.expect(i - 1)
            c.clock.step()
          }

        }
      }
    }
  }

  "do partial completion" in {
    val ports = 4
    val readySeq = Seq.range(0, ports)
    for (imp <- Distributor.getImpTypes) {
      test(Distributor(imp, UInt(8.W), ports)).withAnnotations(Seq(WriteVcdAnnotation)) {
        c => {
          for (value <- 1 to 30) {
            c.in.valid.poke(1)
            c.in.bits.poke(value)
            c.dest.poke(0xF)
            for (r <- readySeq) {
              for (i <- 0 until ports) {
                if (r == i) c.out(i).ready.poke(1)
                else c.out(i).ready.poke(0)
              }
              c.clock.step()
            }
          }
        }
      }
    }
  }
}