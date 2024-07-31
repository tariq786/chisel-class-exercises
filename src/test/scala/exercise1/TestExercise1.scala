package exercise1

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.formal.{BoundedCheck, Formal}
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

class TestExercise1 extends AnyFreeSpec with ChiselScalatestTester  {
  "compute the square" in {
    test(new Exercise1).withAnnotations(Seq(WriteVcdAnnotation)) {
      c => {
        c.io.dataIn.setSourceClock(c.clock)
        c.io.dataOut.setSinkClock(c.clock)

        val numberSeq = for (i <- 0 to 10) yield Random.nextInt(255)
        val dataInSeq = for (x <- numberSeq) yield x.U
        val dataOutSeq = for (x <- numberSeq) yield (x*x).U

        fork {
          c.io.dataIn.enqueueSeq(dataInSeq)
        }.fork {
          c.io.dataOut.expectDequeueSeq(dataOutSeq)
        }.join()
      }
    }
  }
}

class FormalCheckExercise1 extends AnyFreeSpec with ChiselScalatestTester with Formal {
  "check ex1 formal" in {
    verify(new FormalBenchExercise1, Seq(BoundedCheck(10)))
  }
}

class FormalBenchExercise1 extends Exercise1 {

  val lastInput = Module(new Queue(UInt(8.W), 4))

  lastInput.io.enq.valid := io.dataIn.fire
  lastInput.io.enq.bits := io.dataIn.bits
  lastInput.io.deq.ready := io.dataOut.fire
  val expectedAnswer = lastInput.io.deq.bits * lastInput.io.deq.bits

  when (io.dataOut.fire) {
    assert(io.dataOut.bits === expectedAnswer)
  }
}
