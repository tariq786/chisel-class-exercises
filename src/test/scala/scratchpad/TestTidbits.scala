package scratchpad

import chisel3._
import chisel3.assert
import chiseltest._
import chiseltest.formal.Formal
import exercise2.Distributor
import org.scalatest.freespec.AnyFreeSpec

class TestTidbits extends AnyFreeSpec with ChiselScalatestTester with Formal {
  "Testing" in {

//    test(new ManyConnections).withAnnotations(Seq(WriteVcdAnnotation)) {
    test(new UseManyConn).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut =>
        //  test(new ManyConnections) { dut =>
        dut.io.in0.poke(true.B)
        dut.io.in1.poke(true.B)
        dut.io.in2.poke(true.B)
        dut.io.in3.poke(true.B)
        dut.io.out.expect(true.B)

        println(s"io.out = ${dut.io.out.peek.litValue}")

        dut.io.in0.poke(false.B)
        dut.io.in1.poke(true.B)
        dut.io.in2.poke(true.B)
        dut.io.in3.poke(true.B)
        dut.io.out.expect(false.B)
        println(s"io.out = ${dut.io.out.peek.litValue}")

    }
    true
  }
}
//assert(test(ManyConnections))