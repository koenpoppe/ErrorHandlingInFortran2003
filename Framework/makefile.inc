###############################################################################
# As compilers continue to implement Fortran 2003 features, the following
# macro's are available:
# - FC_NO_SAME_TYPE_AS_SUPPORT, disables same_type_as primitive
# - FC_NO_FINAL_SUPPORT, disables finalisation, this disables the ability to detect unhanded errors
# - FC_NO_ALLOCATABLE_DTCOMP, disables allocatable components of a derived type
# - FC_FIXED_LENGTH_CHARACTERSTRINGS, avoids problems with variable length character strings that need to be allocated with SOURCE= argument.

###############################################################################
# NAG Fortran Compiler Release 5.3(842) - First fully supported version
# NAG Fortran Compiler Release 5.3(856)
FFLAGS_nagfor=-f2003 -colour -v -fpp
FFLAGS_nagfor+=-O0 -C=all -g90

# <temp>
FFLAGS_nagfor+=-DFC_FIXED_LENGTH_CHARACTERSTRINGS
FFLAGS_nagfor+=-DFC_NO_ALLOCATABLE_DTCOMP
FFLAGS_nagfor+=-DFC_NO_DT_CONSTRUCTOR
# </temp>

# For verifying the FC_.* options
alt:
	$(MAKE) ALT=1
	$(MAKE) ALT=2
	$(MAKE) ALT=3
	$(MAKE) ALT=4
	$(MAKE) ALT=5

FFLAGS_nagfor_alt1=-DFC_NO_SAME_TYPE_AS_SUPPORT
FFLAGS_nagfor_alt2=-DFC_NO_FINAL_SUPPORT
FFLAGS_nagfor_alt3=-DFC_NO_ALLOCATABLE_DTCOMP
FFLAGS_nagfor_alt4=-DFC_FIXED_LENGTH_CHARACTERSTRINGS
FFLAGS_nagfor_alt5=-DFC_NO_ALLOCATABLE_DTCOMP -DFC_FIXED_LENGTH_CHARACTERSTRINGS
FFLAGS_nagfor+=$(FFLAGS_nagfor_alt$(ALT))

###############################################################################
# GNU Fortran (GCC) 5.2.0 2015

FFLAGS_gfortran=-std=f2003 -Wall -cpp -ffree-line-length-none -pedantic -Wimplicit-interface -Wunderflow -fbounds-check -fimplicit-none
FFLAGS_gfortran+=-O0 -g -fbacktrace
FFLAGS_gfortran+=-DFC_NO_FINAL_SUPPORT -DFC_NO_ALLOCATABLE_DTCOMP -DFC_FIXED_LENGTH_CHARACTERSTRINGS

###############################################################################
# ifort (IFORT) 12.1.0 20111011 - partially
# ifort (IFORT) 12.1.2 20111128 - problem with derived type constructors
# ifort (IFORT) 12.1.5 20120612 - requires "-standard-semantics" for assignments to allocatable arrays
FFLAGS_ifort=-std03 -check -warn -fpp
FFLAGS_ifort+=-standard-semantics
FFLAGS_ifort+=-O0 -g
FFLAGS_ifort+=-DFC_FIXED_LENGTH_CHARACTERSTRINGS
FFLAGS_ifort+=-DFC_NO_ALLOCATABLE_DTCOMP
FFLAGS_ifort+=-DFC_NO_DT_CONSTRUCTOR

# Must be last of the flags, so use FFLAGS_ifort:= ... $(FFLAGS_ifort) to add more options
FFLAGS_ifort+=-free -Tf

###############################################################################
