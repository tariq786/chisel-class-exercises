package exercise2

import chiseltest._
import chisel3._
import chiseltest.formal._
import org.scalatest.Ignore
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

class TestDistributor extends AnyFreeSpec with ChiselScalatestTester with Formal {
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
      println(f"Testing ${imp} Implementation")
      test(Distributor(imp, UInt(8.W), ports)).withAnnotations(Seq(WriteVcdAnnotation)) {
        c => {
         // c.clock.setTimeout(5000)
          for (value <- 1 to 30) {
            c.in.valid.poke(1)
            c.in.bits.poke(value)
            c.dest.poke(0xF)
            for (r <- readySeq) {
//              println(s" Now Testing port ${r}")
              c.out(r).ready.poke(1)
              while (!c.out(r).valid.peekBoolean())
                c.clock.step()
              for (i <- 0 until ports) {
                if (i < r) {
//                  println(s" Ignoring port = ${i}, because ready is for port = ${r} ")
                   c.out(i).valid.expect(0.B)
                }
                if (r == i) {
            //      println(s" ready =  ${r}, port = ${i}")
                  c.out(i).valid.expect(1.B)
                } else c.out(i).ready.poke(0)
              }
              c.clock.step()
//              println("One iteration of ready sequence Done")
            }
          }
        }
      }
    }
  }

  "check nonblocking property" in {
    val ports = 2
    test(Distributor("full", UInt(8.W), 2)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        // Cycle 1
        c.in.valid.poke(1)
        c.dest.poke(1)
        c.in.bits.poke(100)
      //  c.in.ready.expect(1)
        c.clock.step()

        // Cycle 2
        // check output is asserted
        c.out(0).valid.expect(1)
        c.out(0).bits.expect(100)

        c.in.bits.poke(200)
        c.in.ready.expect(0)
        c.dest.poke(2)
        c.in.ready.expect(1)

        // Cycle 3
        c.clock.step(1)
        c.out(1).valid.expect(1)
        c.out(1).bits.expect(200)

        c.in.ready.expect(0)

        c.out(0).ready.poke(1)
        c.out(1).ready.poke(1)
        c.clock.step()
        c.in.bits.poke(50)
        c.dest.poke(3)
        c.in.ready.expect(1)
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
    //assume(false, "This test is optional, comment this out to run the combo implementation")
    val ports = 4
    val readySeq = Seq.range(0, ports)

    test(Distributor("combo", UInt(8.W), ports)).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        c.clock.step()
        for (value <- 1 to 30) {
          c.in.valid.poke(1)
          c.in.bits.poke(value)
          val selected = value % ports
          c.dest.poke(1 << selected)
          for (i <- 0 until ports) {
            c.out(i).ready.poke(i == selected)
          }
          for (i <- 0 until ports) {
            c.out(i).valid.expect(i == selected)
            c.out(i).bits.expect(value)
          }
          c.in.ready.expect(1)
          c.clock.step()
        }
      }
    }
  }

  "prove distributor properties" in {
    verify(new CheckDistributor(UInt(8.W), 4), Seq(BoundedCheck(5)))
  }
}

class CheckDistributor[D <: Data](dtype : D, num : Int) extends RegDistributor(dtype, num) {
  val out_valid = VecInit(out.map(_.valid)).asUInt
  val out_ready = VecInit(out.map(_.ready)).asUInt

  assume(dest =/= 0.U)
  when (in.valid & !in.ready) {
    assume(stable(in.bits))
  }
  when (past(in.fire)) {
    assert(out_valid === past(dest))
    for (i <- 0 until num) {
      assert(out(i).bits === past(in.bits))
    }
  }
  for (i <- 0 until num) {
    when(past(out(i).valid & !out(i).ready)) {
      assert(out(i).valid)
      //assert(out(0).bits === past(out(0).bits))
      assert(stable(out(i).bits))
    }
  }
}

