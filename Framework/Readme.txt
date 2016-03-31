README

2012-04-10 KP - Initial version


COMPILATION
-----------

This package requires both
- a XSLT processor (required for preprocessing) and
- a Fortran compiler which supports at least a subset of the Fortran 2003 standard.

The software has been succesfully tested with
- NAG Fortran Compiler Release 5.3(856) and
- GNU Fortran (GCC) 4.7.0 20111129 (experimental).

For these compilers, compiler flags are included in the Makefile and compilation is as simple as running `make`. This will, amongst others, determine which kinds of integers, real and complex values are supported and preproces the package so its functionality can be used for each of them.

Other compilers might require some additional modifications to the Makefile in order to build the package. If the support for Fortran 2003 is not complete, certain features can be turned of using the following defines:
- FC_NO_FINAL_SUPPORT: disables the finalisation of error type objects. Note that this also disables the feature of detecting ignored errors.
- FC_NO_ALLOCATABLE_DTCOMP: disables allocatable derived type components by replacing them with module variables. Note that this breaks some of the complex error operations like chaining.

Please inform the authors if other compiler/system combinations would cause problems with the software.



QUICK START GUIDE
-----------------

To start working with the error handling package

1. compile the package as described in Appendix~\ref{sec:feh:install},

2. import the framework in your code by adding the

	use error_handling
	
   statement to the program/module,
   
3. add the relevant 'Framework/<compiler>_build/' directory to the paths in which the compiler searches for module files and libraries (typically '-L...'), and

4. link with the 'liberror_handling' library (usually done with '-lerror_handling').
