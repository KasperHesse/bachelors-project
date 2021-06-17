# Hardware accelerator for topology optimization
This repository contains a hardware accelerator for topology optimization, designed as part of my bachelor's project at DTU. 
The accelerator is implemented as an application-specific instruction set processor (ASIP), performing topology optimization by the Single Isotropic Material with Penalization (SIMP) approach. 
For an introduction to topology optimization, see [[1](https://www.topopt.mek.dtu.dk/Apps-and-software/A-99-line-topology-optimization-code-written-in-MATLAB)].

# Setup
The accelerator is designed in [Chisel](https://github.com/chipsalliance/chisel3), a modern hardware construction language embedded in Scala. 
To use Chisel/Scala, you must first have a working Java installation. This project has been built and tested on Java 11 (OpenJDK 11.0.8) on a Windows machine.

With Java setup, download and install [sbt](https://www.scala-sbt.org/download.html), the scala build tool, and clone the repository into a directory of your choice.

I recommend using IntelliJ to work with Chisel projects, as it's just really easy.

The accelerator has been built targeting a [Cyclone V GT development board](https://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=167&No=843), but it should work with any FPGA.

# Configuration
To configure the accelerator, such as changing the number of PE's or size of the design domain, change the relevant value in `src/utils/Config.scala`. Make sure not to remove the call to `checkRequirements`, as this ensures that all settings are interoperable.

# Creating an accelerator
The top level component is defined in the class `TopLevel`, and it is created by running the `Top` object. The top level components takes four configuration parameters, which should be set by editing `Top.scala` before creating the accelerator.
## CLI
To create a Verilog description of the accelerator, open a shell, navigate to the root folder of the project and run
```text
sbt "runMain Top"
```
This will create a Verilog version of the accelerator, and place the generated file in `target/gen/TopLevel.v`
## IntelliJ
Open the project, navigate to `Top.scala` and press the green arrow next to the object name.

# Running tests
The accelerator comes with a number of tests to verify that modules work as expected.
## CLI
To run all the tests for a given module, open a shell and run
```test
sbt "testOnly <package>.<className>"
```
Where `<className>``is the name of a class found in `src/test/scala`. To run a specific test instead of all test suites for a class, run
```text
sbt 'testOnly <package>.<className> -- -z "<testName>" '
```
where `<testName>` is the string located inside the `it should` clause for that test.

## IntelliJ
To run a test, open IntelliJ and navigate to the test file you wish to run. You can run all tests in a class by pressing the green arrow up top, or a specific test by selecting the arrow next to a test's name.


