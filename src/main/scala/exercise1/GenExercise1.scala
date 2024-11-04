package exercise1

import circt.stage.ChiselStage


object GenExercise1 extends App {
  val baseArguments = Array(//"--strip-debug-info",
                            "--split-verilog",
                           // "--disable-all-randomization",
                           // "--lowering-options=disallowExpressionInliningInPorts,disallowLocalVariables",
                            "--target-dir=myVerilog")

  ChiselStage.emitSystemVerilogFile(new Exercise1, baseArguments)
}
