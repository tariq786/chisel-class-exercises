package exercise3

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable
import scala.util.Random
import scala.collection.mutable.Set

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

  "test broadcasting" in {
    val ports = 4
    val tokenCount = 10
    val tokens = for (i <- 0 until ports) yield Seq.range(i*tokenCount, (i+1)*tokenCount)
    val tokenTx = new Array[Int](ports)
    val tokenRx = new Array[Set[BigInt]](ports)
    var done = false

    test(Crossbar("dist", UInt(8.W), ports, ports)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        for (p <- 0 until ports) {
          tokenTx(p) = 0
          tokenRx(p) = new mutable.HashSet[BigInt]()
        }

        while (!done) {
          // check output
          done = true

          for (p <- 0 until ports) {
            c.out(p).ready.poke(1)

            if (c.out(p).valid.peekBoolean()) {
              tokenRx(p).add(c.out(p).bits.peekInt())
            }
            if (tokenRx(p).size < tokenCount*ports) done = false
          }


          // send tokens
          for (p <- 0 until ports) {
            if (tokenTx(p) < tokenCount) {
              c.dest(p).poke(((1 << ports)-1).U)
              c.in(p).valid.poke(1)
              c.in(p).bits.poke(tokens(p)(tokenTx(p)))
            }

            if (c.in(p).ready.peekBoolean()) {
              tokenTx(p) += 1
            }
          }

          c.clock.step()
        }

        println(f"Stimulus complete")
        val completeSet = new mutable.HashSet[BigInt]()
        for (s <- tokens)
          completeSet.addAll(s.map(BigInt(_)))

        for (p <- 0 until ports) {
          if (!completeSet.equals(tokenRx(p))) {
            val missing = completeSet.diff(tokenRx(p))
            println(s"Output port ${p} missing tokens ${missing}")
            assert(false, s"Comparing tokens for port ${p}")
          }
        }

        c.clock.step(10)
      }
    }
  }
}
