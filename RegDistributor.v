module RegDistributor(
  input        clock,
  input        reset,
  output       in_ready, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 16:14]
  input        in_valid, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 16:14]
  input  [7:0] in_bits, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 16:14]
  input  [3:0] dest, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 17:16]
  input        out_0_ready, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  output       out_0_valid, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  output [7:0] out_0_bits, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  input        out_1_ready, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  output       out_1_valid, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  output [7:0] out_1_bits, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  input        out_2_ready, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  output       out_2_valid, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  output [7:0] out_2_bits, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  input        out_3_ready, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  output       out_3_valid, // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
  output [7:0] out_3_bits // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 32:15]
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
`endif // RANDOMIZE_REG_INIT
  reg [7:0] iData; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 61:24]
  reg [3:0] idest; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 62:24]
  wire [3:0] _allready_T = {out_0_ready,out_1_ready,out_2_ready,out_3_ready}; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 64:32]
  wire [3:0] allready = {_allready_T[0],_allready_T[1],_allready_T[2],_allready_T[3]}; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 64:28]
  wire  _iData_T = in_ready & in_valid; // @[src/main/scala/chisel3/util/Decoupled.scala 52:35]
  wire [3:0] _nextidest_T = ~allready; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 77:20]
  wire [3:0] nextidest = _nextidest_T & idest; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 77:30]
  assign in_ready = idest == 4'h0 | nextidest == 4'h0; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 70:34]
  assign out_0_valid = idest[0]; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 73:30]
  assign out_0_bits = iData; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 74:22]
  assign out_1_valid = idest[1]; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 73:30]
  assign out_1_bits = iData; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 74:22]
  assign out_2_valid = idest[2]; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 73:30]
  assign out_2_bits = iData; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 74:22]
  assign out_3_valid = idest[3]; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 73:30]
  assign out_3_bits = iData; // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 74:22]
  always @(posedge clock) begin
    if (_iData_T) begin // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 68:21]
      iData <= in_bits;
    end
    if (_iData_T) begin // @[home/remote/work/chisel-class-exercises/src/main/scala/exercise2/Distributor.scala 69:21]
      idest <= dest;
    end else begin
      idest <= nextidest;
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  iData = _RAND_0[7:0];
  _RAND_1 = {1{`RANDOM}};
  idest = _RAND_1[3:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
