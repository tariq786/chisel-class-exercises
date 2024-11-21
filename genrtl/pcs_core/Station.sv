// Generated by CIRCT firtool-1.37.0
module Station(
  input        clock,
               reset,
               in_valid,
  input  [7:0] in_bits,
  input  [3:0] dest,
  input        out_ready,
               ringDataIn_valid,
  input  [7:0] ringDataIn_bits_sData,
  input  [3:0] ringDataIn_bits_sdest,
  input        ringDataOut_ready,
  output       in_ready,
               out_valid,
  output [7:0] out_bits,
  output       ringDataIn_ready,
               ringDataOut_valid,
  output [7:0] ringDataOut_bits_sData,
  output [3:0] ringDataOut_bits_sdest
);

  wire       reg_dest_1;
  wire [3:0] _arb_io_in_0_bits_sdest;
  wire       _arb_io_out_ready;
  wire       _arb_io_out_valid;
  wire [7:0] _arb_io_out_bits_sData;
  wire [3:0] _arb_io_out_bits_sdest;
  wire [1:0] _regD_dest;
  wire       _regD_out_1_ready;
  wire       _regD_out_1_valid;
  wire [7:0] _regD_out_1_bits_sData;
  assign reg_dest_1 = |(ringDataIn_bits_sdest[3:1]);
  assign _regD_dest = {reg_dest_1, ringDataIn_bits_sdest[0]};
  RegDistributor regD (
    .clock            (clock),
    .reset            (reset),
    .in_valid         (ringDataIn_valid),
    .in_bits_sData    (ringDataIn_bits_sData),
    .dest             (_regD_dest),
    .out_0_ready      (out_ready),
    .out_1_ready      (_regD_out_1_ready),
    .in_ready         (ringDataIn_ready),
    .out_0_valid      (out_valid),
    .out_0_bits_sData (out_bits),
    .out_1_valid      (_regD_out_1_valid),
    .out_1_bits_sData (_regD_out_1_bits_sData)
  );
  assign _arb_io_in_0_bits_sdest = {ringDataIn_bits_sdest[3:1], 1'h0};
  RRArbiter arb (
    .clock              (clock),
    .io_in_0_valid      (_regD_out_1_valid),
    .io_in_0_bits_sData (_regD_out_1_bits_sData),
    .io_in_0_bits_sdest (_arb_io_in_0_bits_sdest),
    .io_in_1_valid      (in_valid),
    .io_in_1_bits_sData (in_bits),
    .io_in_1_bits_sdest (dest),
    .io_out_ready       (_arb_io_out_ready),
    .io_in_0_ready      (_regD_out_1_ready),
    .io_in_1_ready      (in_ready),
    .io_out_valid       (_arb_io_out_valid),
    .io_out_bits_sData  (_arb_io_out_bits_sData),
    .io_out_bits_sdest  (_arb_io_out_bits_sdest)
  );
  Queue queue (
    .clock             (clock),
    .reset             (reset),
    .io_enq_valid      (_arb_io_out_valid),
    .io_enq_bits_sData (_arb_io_out_bits_sData),
    .io_enq_bits_sdest (_arb_io_out_bits_sdest),
    .io_deq_ready      (ringDataOut_ready),
    .io_enq_ready      (_arb_io_out_ready),
    .io_deq_valid      (ringDataOut_valid),
    .io_deq_bits_sData (ringDataOut_bits_sData),
    .io_deq_bits_sdest (ringDataOut_bits_sdest)
  );
endmodule

