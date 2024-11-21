package exercise3

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

class TestCrossbar extends AnyFreeSpec with ChiselScalatestTester {
  "test independent paths" in {
    val ports = 4
    for (imp <- Crossbar.getImpTypes) {
      test(Crossbar(imp, UInt(8.W), ports, ports)).withAnnotations(Seq(WriteVcdAnnotation)) {
        c => {
          for (p <- 0 until ports) {
            c.in(p).initSource().setSourceClock(c.clock)
            c.out(p).initSink().setSinkClock(c.clock)
            c.dest(p).poke(1 << ((p + 2) % 4))
            println(s"p = ${p}")
            println(s"dest = ${1 << ((p + 2) % 4)}")
          }

          fork {
            c.in(0).enqueueSeq(Seq.range(0, 10).map(_.U))
          }.fork {
            c.in(1).enqueueSeq(Seq.range(10, 20).map(_.U))
          }.fork {
            c.in(2).enqueueSeq(Seq.range(20, 30).map(_.U))
          }.fork {
            c.in(3).enqueueSeq(Seq.range(30, 40).map(_.U))
          }.fork {
            c.out(2).expectDequeueSeq(Seq.range(0, 10).map(_.U))
          }.fork {
            c.out(3).expectDequeueSeq(Seq.range(10, 20).map(_.U))
          }.fork {
            c.out(0).expectDequeueSeq(Seq.range(20, 30).map(_.U))
          }.fork {
            c.out(1).expectDequeueSeq(Seq.range(30, 40).map(_.U))
          }.join()
        }
      }
    }
  }

  "test different sizes" in {
    for (iports <- 2 to 4) {
      for (oports <- 2 to 4) {
        test(Crossbar("dist", UInt(8.W), iports, oports)).withAnnotations(Seq(WriteVcdAnnotation)) {
          c => {
            // Cycle through each input and output port and send a single transaction
            for (ip <- 0 until iports) {
              for (op <- 0 until oports) {
                val r = Random.nextInt(256)
                c.in(ip).valid.poke(1)
                c.in(ip).bits.poke(r)
                c.dest(ip).poke((1 << op).U)
                c.out(op).ready.poke(1)
                while (!c.in(ip).ready.peekBoolean()) c.clock.step()
                c.clock.step()
                c.in(ip).valid.poke(0)
                while (!c.out(op).valid.peekBoolean()) c.clock.step()
                c.out(op).bits.expect(r)
                c.clock.step()
                c.out(op).ready.poke(0)
              }
            }
          }
        }
      }
    }
  }
}
