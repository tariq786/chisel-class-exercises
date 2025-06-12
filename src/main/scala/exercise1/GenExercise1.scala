package exercise4
import chisel3._

import circt.stage.ChiselStage

object GenExercise1 extends App {
  /*ChiselStage.emitSystemVerilogFile(new BusUpsize(1,4), Array.empty,
                                                        Array("--strip-debug-info",
                                                              "--split-verilog",
                                                              "-o=genrtl",
                                                              "--disable-all-randomization",
                                                              "--lowering-options=disallowLocalVariables,disallowExpressionInliningInPorts")
                                   )*/
  ChiselStage.emitSystemVerilogFile(new Fifo(UInt(8.W),16), Array.empty,
    Array("--strip-debug-info",
      "--split-verilog",
      "-o=genrtl",
      "--disable-all-randomization",
      "--lowering-options=disallowLocalVariables,disallowExpressionInliningInPorts")
  )
}


/*
object GenExercise1 extends App {
  val baseArguments = Array(//"--strip-debug-info",
                            "--split-verilog",
                            "--disable-all-randomization",
                           // "--lowering-options=disallowExpressionInliningInPorts,disallowLocalVariables",
                            "--target-dir=myVerilog")

  ChiselStage.emitSystemVerilogFile(new Exercise2,  firtoolOpts = Array(
    "-disable-all-randomization", // <-- This is the option
    "-strip-debug-info",
    "--split-verilog",
    "--target-dir=myVerilog",
  ))
}
*/
