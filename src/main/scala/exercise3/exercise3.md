Exercise 2
==========

The objective of exercise 3 is to create your second library component.  This block is called Crossbar,
and it accepts data from multiple input ports and sends data to multiple output ports.  Crossbar builds
off the Distributor you created for exercise 2, combining it with the RRArbiter component from the Chisel
library.

Exercise 3 also demonstrates how we can build scalable components by instantiating arrays of components.
To create an array of components, use a *for comprehension* to create the array:

```scala
  val arb = for (s <- 0 until numOut) yield Module(new RRArbiter(dtype, numIn))
```

The newly created modules can then be accessed as any other Scala array element:

```scala
  arb(2).io.in(k) <> my_input
  arb(2).io.out <> out(2) 
```

