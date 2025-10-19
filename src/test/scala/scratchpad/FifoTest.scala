package scratchpad

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec


class FifoTest extends AnyFreeSpec with ChiselScalatestTester{
"fifo2_test" in {
  test(new Fifo2(4)).withAnnotations(Seq(WriteVcdAnnotation)) {
    dut => {
      // Set up clock domains for Decoupled interfaces
      dut.io.in.setSourceClock(dut.clock)
      dut.io.out.setSinkClock(dut.clock)

      fork{
        dut.io.in.valid.poke(true.B)
        for(i <- 0 until 10) {
          dut.io.in.bits.poke(i.U)
          dut.clock.step(1)
          }
      }.fork{
        dut.io.out.ready.poke(true.B)
        for(i <- 0 until 10) {
          dut.io.out.expectDequeue(i.U)}
        dut.clock.step(1)
      }.join()
//      dut.clock.step(10)
    }

}
}
}