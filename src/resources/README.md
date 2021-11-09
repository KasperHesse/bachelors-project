# Memory initialization files
The folders `meminit_` contain memory and register initialization files that can be used for simulation purposes.

**meminit_test** contains a number of register initialization files that can be used to test
execution logic and memory load/store without any prior immediate loading. 
Vector registers are initialized in a way such that `reg(0)(0) = fixed(0)`, `reg(0)(1) = fixed(1)`, ...
`reg(0)(depth-1) = fixed(depth-1)` and `reg(1)(0) = fixed(depth)`. In general, `reg(w)(d) = w*depth+d`.

**meminit_default** contains register init files much alike meminit_test, but also contains memory init files that will initialize the F-vector to have a number of downwards force vectors, setting up the topology optimization problem being solved.

**meminit** is the main directory being used for simulations. In this directory, memory and register initialization files created by [SimulationMemInit](../main/scala/utils/SimulationMemInit.scala) are placed.