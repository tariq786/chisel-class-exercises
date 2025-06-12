package exercise4

import chisel3._
import chisel3.util._

class Fifo[T <: Data](datatype: T, depth: Int) extends Module {
  val io = IO (new Bundle {
    val enq = new Bundle {
      val valid = Input(Bool())
      val bits  = Input(datatype)
      val ready = Output(Bool())
    }
    val deq = new Bundle {
      val valid = Output(Bool())
      val bits  = Output(datatype)
      val ready = Input(Bool())
    }
  }
  )


  //synch ram
  val ram = Mem(depth, datatype)
  val enqptr = RegInit(0.U(log2Ceil(depth).W))
  val deqptr = RegInit(0.U(log2Ceil(depth).W))

  val isFull = RegInit(false.B)
  val isEmpty = (deqptr === enqptr) && !isFull

  //write logic
  when(io.enq.valid && io.enq.ready) {
    ram(enqptr) := io.enq.bits
    enqptr := Mux(enqptr === (depth-1).U, 0.U,enqptr + 1.U)

    when( (enqptr + 1.U) % depth.U === deqptr) {
      isFull := true.B
    }

  }

  io.deq.bits := Mux(!isEmpty, ram(deqptr), 0.U)
  //read logic

  // Corrected read logic - only modify registers
  when(io.deq.valid && io.deq.ready) {
    // Update the dequeue pointer
    deqptr := Mux(deqptr === (depth-1).U, 0.U, deqptr + 1.U)

    // Always clear the full flag when dequeuing
    isFull := false.B

    // Note: We don't need to set isEmpty directly because it's automatically
    // calculated from the pointers and isFull
  }


  io.enq.ready := !isFull
  io.deq.valid := !isEmpty





} //end of class*/

/*
import chisel3._
import chisel3.util._

class Fifo[T <: Data](datatype: T, depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = new Bundle {
      val valid = Input(Bool())
      val bits = Input(datatype)
      val ready = Output(Bool())
    }
    val deq = new Bundle {
      val valid = Output(Bool())
      val bits = Output(datatype)
      val ready = Input(Bool())
    }
  })

  // Synch ram
  val ram = Mem(depth, datatype)
  val enqptr = RegInit(0.U(log2Ceil(depth).W))
  val deqptr = RegInit(0.U(log2Ceil(depth).W))

  // Count of elements in the FIFO
  val count = RegInit(0.U(log2Ceil(depth+1).W))

  // Derived empty and full conditions based on count
  val isEmpty = count === 0.U
  val isFull = count === depth.U

  // Handshaking signals
  val doEnq = io.enq.valid && io.enq.ready
  val doDeq = io.deq.valid && io.deq.ready

  // Update the element count based on operations
  when(doEnq && !doDeq) {
    count := count + 1.U
  }.elsewhen(!doEnq && doDeq) {
    count := count - 1.U
  }
  // Note: When both doEnq and doDeq are true, count stays the same

  // Write logic
  when(doEnq) {
    ram(enqptr) := io.enq.bits
    enqptr := Mux(enqptr === (depth-1).U, 0.U, enqptr + 1.U)
  }

  // Read logic
  when(doDeq) {
    deqptr := Mux(deqptr === (depth-1).U, 0.U, deqptr + 1.U)
  }

  // Connect output signals
  io.enq.ready := !isFull
  io.deq.valid := !isEmpty
  io.deq.bits := Mux(!isEmpty, ram(deqptr), 0.U.asTypeOf(datatype))
}

 */
/*import chisel3._
import chisel3.util._

class Fifo[T <: Data](datatype: T, depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = new Bundle {
      val valid = Input(Bool())
      val bits = Input(datatype)
      val ready = Output(Bool())
    }
    val deq = new Bundle {
      val valid = Output(Bool())
      val bits = Output(datatype)
      val ready = Input(Bool())
    }
  })

  // Storage
  val ram = Mem(depth, datatype)
  val enqptr = RegInit(0.U(log2Ceil(depth).W))
  val deqptr = RegInit(0.U(log2Ceil(depth).W))

  // This is the key change - explicitly track element count
  val count = RegInit(0.U(log2Ceil(depth+1).W))

  // Define operations
  val doEnq = io.enq.valid && io.enq.ready
  val doDeq = io.deq.valid && io.deq.ready

  // Update count based on operations
  switch(Cat(doEnq, doDeq)) {
    is(1.U) { count := count - 1.U }  // Dequeue only
    is(2.U) { count := count + 1.U }  // Enqueue only
    // Both or neither: count remains unchanged
  }

  // Write logic
  when(doEnq) {
    ram(enqptr) := io.enq.bits
    enqptr := Mux(enqptr === (depth-1).U, 0.U, enqptr + 1.U)
  }

  // Read logic
  when(doDeq) {
    deqptr := Mux(deqptr === (depth-1).U, 0.U, deqptr + 1.U)
  }

  // Status based directly on element count
  val isEmpty = count === 0.U
  val isFull = count === depth.U

  // Connect outputs
  io.enq.ready := !isFull
  io.deq.valid := !isEmpty
  io.deq.bits := Mux(!isEmpty, ram(deqptr), 0.U.asTypeOf(datatype))
}*/