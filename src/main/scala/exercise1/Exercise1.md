Exercise 1
==========

The objective of exercise 1 is to understand how to connect Chisel components with Decoupled interfaces,
and adapt to modules which have "demanding" interfaces and flow control patterns.

The Multipler component used in this design has two input interfaces which are both demanding, and
which have different demands for incoming data.  To compute the square, both inputs of the multiplier
should receive the same input value, however connecting all the interfaces together as shown in the 
lecture notes results in a construct which itself is demanding, and therefore cannot be connected to
another demanding interface.

Therefore the goal is to create a design which connects to the exercise1 *dataIn* port which provides
data to the two Mult input ports in a permissive way.
