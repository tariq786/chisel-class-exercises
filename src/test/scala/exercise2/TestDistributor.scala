package exercise2

import chiseltest._
import chisel3._
import org.scalatest.freespec.AnyFreeSpec

class TestDistributor extends AnyFreeSpec with ChiselScalatestTester {
  "complete in single cycle when all source ready" in {
    val ports = 4
    for (imp <- Seq("combo")) {
      test(Distributor(imp, UInt(8.W), ports)).withAnnotations(Seq(WriteVcdAnnotation)) {
        c => {
          for (value <- 1 to 30) {
            // drive source values
            c.in.valid.poke(1)
            c.in.bits.poke(value)
            c.dest.poke(0xF)
            for (p <- 0 until ports) {
              c.out(p).ready.poke(1)
            }
            // check results
            c.in.ready.expect(1)
            for (p <- 0 until ports) {
              c.out(p).valid.expect(1)
              c.out(p).bits.expect(value)
            }
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