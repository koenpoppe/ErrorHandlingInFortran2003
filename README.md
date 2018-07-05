# Error Handling in Fortran 2003 [1]

## Abstract

Although the Fortran programming language is evolving steadily, it still lacks a framework for error handling -- not to be confused with floating point exceptions. Therefore, the commonly used techniques for handling errors did not change much since the early days and do not benefit from the new features of Fortran 2003. After discussing some historical approaches, a Fortran 2003 framework for error handling is presented. This framework also proved to be valuable in the context of unit testing and the design-by-contract (DBC) paradigm.

## Compilation

This package requires both
* a XSLT processor (required for preprocessing) and
* a Fortran compiler which supports at least a subset of the Fortran 2003 standard.

The software has been succesfully tested with
* NAG Fortran Compiler Release 5.3(856) and
* GNU Fortran (GCC) 4.7.0 20111129 (experimental).
* GNU Fortran (GCC) 5.2.0 2015
* GNU Fortran (GCC) 6.3.0 2016
* Intel Fortran Compiler 12.1.5 20120612 (experimental)

For these compilers, compiler flags are included in the Makefile and compilation is as simple as running `make`. This will, amongst others, determine which kinds of integers, real and complex values are supported and preproces the package so its functionality can be used for each of them.

Other compilers might require some additional modifications to the Makefile in order to build the package. If the support for Fortran 2003 is not complete, certain features can be turned of using the following defines:
* FC_NO_FINAL_SUPPORT: disables the finalisation of error type objects. Note that this also disables the feature of detecting ignored errors.
* FC_NO_ALLOCATABLE_DTCOMP: disables allocatable derived type components by replacing them with module variables. Note that this breaks some of the complex error operations like chaining.

Please inform the authors if other compiler/system combinations would cause problems with the software.

## Quick start guide

0. Open the Makefile and select the Fortran compiler to use (see *FC=*)

1. Compile the framework by running *make*; The resulting module files and library is stored in "<your_compiler>_build"
  ```
  cd Framework
  make
  ```

2. import the framework in the program/module with a use statement to 
  ```
  use error_handling
  ```

3. add the relevant 'Framework/\<compiler\>_build/' directory to the paths in which the compiler searches for module files and libraries (typically '-L...'), and

4. link with the 'liberror_handling' library (usually done with '-lerror_handling').

## Examples

A set of example programs that illustrate the use of the framework is included in the source tree. Running all of them can be done by executing *make* in that directory:
  ```
  cd Examples
  make
  ```

## References

1. Koen Poppe, Ronald Cools and Bart Vandewoestyne. 2012. Error handling in Fortran 2003.
  * [SIGPLAN Fortran Forum 31, 2 (July 2012), 7-19](http://dx.doi.org/10.1145/2338786.2338787)
  * [KULeuven Lirias](https://lirias.kuleuven.be/handle/123456789/353834)
