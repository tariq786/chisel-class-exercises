package exercise2

import chiseltest._
import chisel3._
import org.scalatest.Ignore
import org.scalatest.freespec.AnyFreeSpec
import scala.math.pow

import scala.util.Random

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

  // This is the same test from Exercise 1, but using your implementation of Distributor
  // instead of the logic function you created for the first exercise.
  "compute the square" in {
    test(new Exercise2).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        c.io.dataIn.setSourceClock(c.clock)
        c.io.dataOut.setSinkClock(c.clock)

        val numberSeq = for (i <- 0 to 10) yield Random.nextInt(255)
        val dataInSeq = for (x <- numberSeq) yield x.U
        val dataOutSeq = for (x <- numberSeq) yield (x * x).U

        fork {
          c.io.dataIn.enqueueSeq(dataInSeq)
        }.fork {
          c.io.dataOut.expectDequeueSeq(dataOutSeq)
        }.join()
      }
    }
  }

  /** Test the combinatorial implementation
   *
   * This test covers the same-cycle performance requirement of the combinatorial implementation.
   *
   * Implementing the combo distributor is optional, if you implement this, remove the "Ignore"
   * pramga below to run the unit test.
   */
  "test combinatorial distributor" in {
    assume(true, "This test is optional, comment this out to run the combo implementation")
    val ports = 4 //changed for debugging from 4 to 2
    // val readySeq = Seq.range(0, ports)  //NOT USED

    test(Distributor("combo", UInt(8.W), ports)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        for (value <- 1 to 30) { //changed for debugging from 30 to 8
          c.in.valid.poke(1)
          c.in.bits.poke(value)
          if (value % pow(2, ports).toInt != 0)
          {
            val selected = (value % pow(2, ports).toInt) //changed to have multiple output ports
            println(s" selected =  ${selected}")
            c.dest.poke(selected)
            for (i <- 0 until ports) {
              if (((selected >> i) & 1) == 1) {
                c.out(i).ready.poke(1)
                println(s"c.out(${i}).ready =  ${c.out(i).ready.peek().litValue}")
              }
            }
            for (i <- 0 until ports) {
              //          println(s" c.dest =  ${c.dest.peek().litValue}")
              //          println(s" c.out(${i}).bits =  ${c.out(i).bits.peek().litValue}")
              if (((selected >> i) & 1) == 1) //if multiple output ports are selected, then we need to check them one by one
              {
                println(s" c.out(${i}).bits =  ${c.out(i).bits.peek().litValue}")
                c.out(i).bits.expect(value)
                println(s" c.out(${i}).valid =  ${c.out(i).valid.peek().litValue}")
                c.out(i).valid.expect(true)
              }
            }
          c.in.ready.expect(1)
          c.clock.step()
          for (i <- 0 until ports) {
            if (((selected >> i) & 1) == 1) //if multiple output ports are selected, then we need to check them one by one
            {
              c.out(i).ready.poke(0)
            }
          }
        }
          println(s"****")
          println(s"")
        }
      }
    }
  }
}