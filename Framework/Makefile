FC=nagfor_beta

# nagfor      = NAG Fortran Compiler Release 5.2(752)
# nagfor_beta = NAG Fortran Compiler Release 5.3(820),5.2(762)
FFLAGS_nagfor=-f2003 -colour -O0 -C=all -fpp -v -mdir $(LIBDIR) -I $(LIBDIR)
FFLAGS_nagfor_beta=$(FFLAGS_nagfor) -DFC_SUPP_FINAL -DFC_SUPP_SAME_TYPE_AS

# gfortran_beta = GNU Fortran (GCC) 4.7.0 20110326 (experimental)
FFLAGS_gfortran=-std=f2008 -cpp -J $(LIBDIR)
FFLAGS_gfortran_beta=$(FFLAGS_gfortran)

# g95 = G95 (GCC 4.0.3 (g95 0.92!) Jun  3 2009)
FFLAGS_g95=-std=f2003 -fmod=$(LIBDIR)

# ifort = ifort (IFORT) 11.0 20080930
FFLAGS_ifort= -module $(LIBDIR) -fpp -g -check -warn -free -Tf



FFLAGS=$(FFLAGS_$(FC))
LIBDIR=Build_$(FC)/
LIBRARY=$(LIBDIR)liberror_handling.a

OBJECTS= \
	error_handling_error.o \
	error_handling_common_errors.o \
	error_handling_common_wrappers.o \
	error_handling_legacy.o \
	design_by_contract.o \
	error_handling_unit_test.o \
	error_handling.o

first: $(LIBRARY)

$(LIBDIR): clean
	mkdir -p -v $(LIBDIR)

$(LIBRARY): $(LIBDIR) $(OBJECTS)
	$(AR) -r -u $@ $(OBJECTS)

%.o: %.f03
	$(FC) -c $(FFLAGS) $^

clean:
	$(RM) *.o *.mod

veryclean: clean
	$(RM) $(LIBRARY)
	$(RM) -r $(LIBDIR)

all: 
	make FC=nagfor
	make FC=nagfor_beta
	make FC=ifort
