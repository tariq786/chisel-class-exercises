package scratchpad
import chisel3._
import circt.stage.ChiselStage
import exercise3.RingCrossbar

object TestBuild extends App {
  val baseArguments = Array("--strip-debug-info",
                            "--split-verilog",
                            "--disable-all-randomization",
                            "--lowering-options=disallowExpressionInliningInPorts,disallowLocalVariables")
  val targetDir = "genrtl/pcs_core"

  ChiselStage.emitSystemVerilogFile(new RingCrossbar(UInt(8.W),4,4), Array.empty, baseArguments++Array("-o=" + targetDir))

}
