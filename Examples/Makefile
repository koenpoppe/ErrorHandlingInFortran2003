first: all

###############################################################################
# Fortran compiler

# FC=nagfor_beta
# FC=gfortran_beta
# FC=ifort_beta
FC=gfortran

FPP=$(FC)

# Library
LIBRARY_PATH=../Framework/
include $(LIBRARY_PATH)makefile.inc
LIBRARY_DIR=$(LIBRARY_PATH)$(FC)_build/
LIBRARY=-lerror_handling

# Extra flags
FFLAGS_gfortran_beta+=-J $(LIBRARY_DIR)
FFLAGS_ifort_beta:=-I $(LIBRARY_DIR) $(FFLAGS_ifort_beta)
FFLAGS_nagfor_beta+= -I $(LIBRARY_DIR)
FFLAGS_gfortran+=-J $(LIBRARY_DIR)

FFLAGS=$(FFLAGS_$(FC))

# Command to get version
FC_VERSION_CMD_gfortran_beta=$(FC) --version
FC_VERSION_CMD_ifort_beta=$(FC) --version
FC_VERSION_CMD_nagfor_beta=$(FC) -V
FC_VERSION_CMD_gfortran=$(FC) --version
FC_VERSION_CMD=$(FC_VERSION_CMD_$(FC))

###############################################################################
.PHONY: clean cleanpreprocs cleandrivers cleanlogs all
MAKEFLAGS+= --no-print-directory

LDFLAGS= -L$(LIBRARY_DIR) $(LIBRARY)
%.o: %.f03
	$(FC) -c $(DEFINES) $(FFLAGS) $^

clean:
	$(RM) *.o *.mod *.g90

###############################################################################
# The available examples and the selected one
EXAMPLES=username_selection nversion_algorithm fragile_input lin_solve unittest_test common_wrappers
EXAMPLE=unittest_test

# The corresponding drivers
DRIVERS=$(addsuffix _driver,$(EXAMPLES))
DRIVER=$(EXAMPLE)_driver

# Error handling version selection: old or new
VERSION=new
ifeq ($(VERSION),new)
	DEFINES=-DERROR_HANDLING
else
	DEFINES=
endif

# How to preprocess the files
EXAMPLE_PREPROC=$(EXAMPLE)_$(VERSION).f03
DRIVER_PREPROC=$(DRIVER)_$(VERSION).f03
EXAMPLES_PREPROC=$(addsuffix _$(VERSION).f03,$(EXAMPLES))
DRIVERS_PREPROC=$(addsuffix _$(VERSION).f03,$(DRIVERS))

preprocs: $(EXAMPLE_PREPROC) $(DRIVER_PREPROC)
$(EXAMPLE_PREPROC): $(EXAMPLE).f03
	$(FPP) -o tmp_$@ $(DEFINES) $(FFLAGS) -F $^
	cat tmp_$@ | grep -v "^# " > $@
	$(RM) tmp_$@
$(DRIVER_PREPROC): $(DRIVER).f03
	$(FPP) -o tmp_$@ $(DEFINES) $(FFLAGS) -F $^
	cat tmp_$@ | grep -v "^# " > $@
	$(RM) tmp_$@

cleanpreprocs:
	$(RM) $(EXAMPLES_PREPROC)
	$(RM) $(DRIVERS_PREPROC)

# How to compile one example
DRIVER_PROG=$(DRIVER)_$(VERSION)
DRIVERS_PROG=$(addsuffix _$(VERSION),$(DRIVERS))

one: $(DRIVER_PROG)
$(DRIVER_PROG): $(EXAMPLE).o $(DRIVER).o
	$(FC) -o $@ $^ $(LDFLAGS)
cleandrivers:
	$(RM) $(DRIVERS_PROG)

# How to run
run: $(DRIVER_PROG)
	./$(DRIVER_PROG)

# How to debug a run?
debug: $(DRIVER_PROG)
	gdb -tui ./$(DRIVER_PROG)

# How to store the results of one run
DRIVER_LOG=$(DRIVER_PROG).txt
DRIVERS_LOG=$(addsuffix .txt,$(DRIVERS_PROG))

store: $(DRIVER_LOG)
$(DRIVER_LOG): $(DRIVER_PROG)
	./$(DRIVER_PROG) | tee $(DRIVER_LOG)

full: store preprocs

cleanlogs:
	$(RM) $(DRIVERS_LOG)

# Documentation
SOURCE_LIST=error_handling.f03
doc: $(SOURCE_LIST)
	@echo "FortranDOC is processing your source files"
	@echo $(SOURCE_LIST) | tr ' ' '\n' > docsourcelist 
	@FortranDOC
	@rm docsourcelist

# How to clean up the mess
veryclean: clean cleandrivers cleanpreprocs cleanlogs cleanothers

# Collectives
ALLTARGET=run
all:
	$(foreach EX,$(EXAMPLES), $(MAKE) $(ALLTARGET) VERSION=$(VERSION) EXAMPLE=$(EX) ; )
allversions_safe:
	$(MAKE) clean all ALLTARGET=$(ALLTARGET) VERSION=old
	$(MAKE) clean all ALLTARGET=$(ALLTARGET) VERSION=new
allversions_unsafe:
	$(MAKE) all ALLTARGET=$(ALLTARGET) VERSION=old
	$(MAKE) all ALLTARGET=$(ALLTARGET) VERSION=new

allversions:
	$(MAKE) allversions_safe ALLTARGET=one
allpreprocs:
	$(MAKE) allversions_safe ALLTARGET=preprocs
alllogs:
	$(MAKE) allversions_safe ALLTARGET=store
allfull:
	$(MAKE) allversions_safe ALLTARGET=full
    
allclean:
	$(MAKE) veryclean VERSION=old
	$(MAKE) veryclean VERSION=new

REGRESSION_FILENAME="regression/$(FC)_$(shell date +'%Y%m%d').txt"
regression: veryclean framework all
	@echo $(REGRESSION_FILENAME)
	@$(FC_VERSION_CMD) 1>  $(REGRESSION_FILENAME)
	@$(FC_VERSION_CMD) 2>> $(REGRESSION_FILENAME)
	@date >> $(REGRESSION_FILENAME)
	@$(foreach EX,$(DRIVERS_PROG), echo "\n=== $(EX) ====================================" >> $(REGRESSION_FILENAME) ; ./$(EX) >> $(REGRESSION_FILENAME) ; )

# Testing purpose
flags:
	@echo "$(VERSION): $(FFLAGS)"
allflags:
	$(MAKE) allversions_unsafe ALLTARGET=flags

# Others
unittest_test: unittest_test.o unittest_test_driver.o
	$(FC) -o $@ $^ $(LDFLAGS)


dbc_test: dbc_test.o
	$(FC) -o $@ $^ $(LDFLAGS)

test: dbc_test
	./dbc_test

cleanothers:
	$(RM) unittest_test dbc_test

framework:
	cd ../Framework; make