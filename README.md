# Hardware accelerator for topology optimization
This repository contains a hardware accelerator for topology optimization, designed as part of my bachelor project at DTU (Technical University of Denmark). 
The accelerator is implemented as an application-specific instruction set processor (ASIP), performing topology optimization by the Solid Isotropic Material with Penalization (SIMP) approach. 
For an introduction to topology optimization, see [[1](https://www.topopt.mek.dtu.dk/Apps-and-software/A-99-line-topology-optimization-code-written-in-MATLAB)]. 

The thesis, which is the primary source of information on the accelerator, can be found in [thesis.pdf](thesis.pdf).

# Setup
The accelerator is designed in [Chisel](https://github.com/chipsalliance/chisel3), a modern hardware construction language embedded in Scala. 
To use Chisel/Scala, you must first have a working Java installation. 
This project has been built and tested on Java 11 (OpenJDK 11.0.8) on a Windows machine.

With Java setup, download and install [sbt](https://www.scala-sbt.org/download.html) and clone the repository into a directory of your choice.
I recommend using IntelliJ to work with Chisel projects. 
The accelerator has been built targeting a [Cyclone V GT development board](https://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=167&No=843), but it should work with any FPGA.

# Configuration
To configure the accelerator, such as changing the number of PE's or size of the design domain, change the relevant value in `src/utils/Config.scala`. 
Make sure not to remove the call to `checkRequirements`, as this ensures that all settings are interoperable.

# Creating the accelerator
The top level component is defined in the class `TopLevel`, and it is created by running the `Top` object. 
The top level components takes four configuration parameters, which should be set by editing `Top.scala` before creating the accelerator.

# Programs
The accelerator comes with three full programs for topology optimization on a [6x6x6](src/resources/programs/top_6x6x6.txt), [10x10x10](src/resources/programs/top_10x10x10.txt) and [8x10x12](src/resources/programs/top_8x10x12.txt) grid. 
When generating the verilog for a given program, remember to also change the values of `NELX`, `NELY` and `NELZ` in [the configuration](src/utils/Config.scala).

# Running tests
The accelerator comes with a number of tests to verify that modules work as expected.

Running all tests included with the accelerator takes a **long** time, so I don't recommend doing this. 

Also, some of the earliest tests are known to fail, since I have made several major refactors of the test setup. 
One day, I will get around to bringing the earliest tests in line. 
In the meantime, the tests in [TopLevelStageSpec](src/test/scala/TopLevelStageSpec.scala) can be used to run sections of the full topology optimization program in [top.txt](src/resources/programs/top.txt). 
Once the first test has been run, you can update the parameter `memInitName` to initialize memory with the results of a prior simulation.



