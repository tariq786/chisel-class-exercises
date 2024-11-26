// Generated by CIRCT firtool-1.37.0
module RRArbiter(
  input        clock,
               io_in_0_valid,
  input  [7:0] io_in_0_bits_sData,
  input  [3:0] io_in_0_bits_sdest,
  input        io_in_1_valid,
  input  [7:0] io_in_1_bits_sData,
  input  [3:0] io_in_1_bits_sdest,
  input        io_out_ready,
  output       io_in_0_ready,
               io_in_1_ready,
               io_out_valid,
  output [7:0] io_out_bits_sData,
  output [3:0] io_out_bits_sdest
);

  wire io_chosen_choice;
  wire _io_out_valid_output = io_chosen_choice ? io_in_1_valid : io_in_0_valid;
  reg  ctrl_validMask_grantMask_lastGrant;
  wire ctrl_validMask_1 = io_in_1_valid & ~ctrl_validMask_grantMask_lastGrant;
  assign io_chosen_choice = ctrl_validMask_1 | ~io_in_0_valid;
  always @(posedge clock) begin
    if (io_out_ready & _io_out_valid_output)
      ctrl_validMask_grantMask_lastGrant <= io_chosen_choice;
  end // always @(posedge)
  assign io_in_0_ready = ~ctrl_validMask_1 & io_out_ready;
  assign io_in_1_ready =
    (~ctrl_validMask_grantMask_lastGrant | ~(ctrl_validMask_1 | io_in_0_valid))
    & io_out_ready;
  assign io_out_valid = _io_out_valid_output;
  assign io_out_bits_sData = io_chosen_choice ? io_in_1_bits_sData : io_in_0_bits_sData;
  assign io_out_bits_sdest = io_chosen_choice ? io_in_1_bits_sdest : io_in_0_bits_sdest;
endmodule
