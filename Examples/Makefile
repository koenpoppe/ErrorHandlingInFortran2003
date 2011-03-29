first: one

.PHONY: clean cleanpreprocs cleandrivers cleanlogs all
MAKEFLAGS+= --no-print-directory

# Fortran compiler and flags
FC=nagfor_beta
FPP=nagfor
FFLAGS=-f2003 -colour -O0 -C=all -fpp -v
LDFLAGS=
%.o: %.f03
	$(FC) -c $(FFLAGS) $^

clean:
	$(RM) *.o *.mod

# The available examples and the selected one
EXAMPLES=username_selection nversion_algorithm fragile_input lin_solve
EXAMPLE=fragile_input

# The corresponding drivers
DRIVERS=$(addsuffix _driver,$(EXAMPLES))
DRIVER=$(EXAMPLE)_driver

# Error handling version selection: old or new
VERSION=new
ifeq ($(VERSION),new)
	DEFINES=-DEXCEPTION_HANDLING
else
	DEFINES=
endif
FFLAGS+=$(DEFINES)

# How to preprocess the files
EXAMPLE_PREPROC=$(EXAMPLE)_$(VERSION).f03
DRIVER_PREPROC=$(DRIVER)_$(VERSION).f03
EXAMPLES_PREPROC=$(addsuffix _$(VERSION).f03,$(EXAMPLES))
DRIVERS_PREPROC=$(addsuffix _$(VERSION).f03,$(DRIVERS))

preprocs: $(EXAMPLE_PREPROC) $(DRIVER_PREPROC)
$(EXAMPLE_PREPROC): $(EXAMPLE).f03
	$(FPP) -o tmp_$@ $(FFLAGS) -F $^
	cat tmp_$@ | grep -v "^# " > $@
	$(RM) tmp_$@
$(DRIVER_PREPROC): $(DRIVER).f03
	$(FPP) -o tmp_$@ $(FFLAGS) -F $^
	cat tmp_$@ | grep -v "^# " > $@
	$(RM) tmp_$@

cleanpreprocs:
	$(RM) $(EXAMPLES_PREPROC)
	$(RM) $(DRIVERS_PREPROC)

# How to compile one example
DRIVER_PROG=$(DRIVER)_$(VERSION)
DRIVERS_PROG=$(addsuffix _$(VERSION),$(DRIVERS))

one: $(DRIVER_PROG)
$(DRIVER_PROG): exception_handling.o $(EXAMPLE).o $(DRIVER).o
	$(FC) -o $@ $(LDFLAGS) $^
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
SOURCE_LIST=exception_handling.f03
doc: $(SOURCE_LIST)
	@echo "FortranDOC is processing your source files"
	@echo $(SOURCE_LIST) | tr ' ' '\n' > docsourcelist 
	@FortranDOC
	@rm docsourcelist

# How to clean up the mess
veryclean: clean cleandrivers cleanpreprocs cleanlogs

# Collectives
ALLTARGET=one
all:
	$(MAKE) $(ALLTARGET) VERSION=$(VERSION) EXAMPLE=fragile_input
	$(MAKE) $(ALLTARGET) VERSION=$(VERSION) EXAMPLE=nversion_algorithm
	$(MAKE) $(ALLTARGET) VERSION=$(VERSION) EXAMPLE=username_selection
	$(MAKE) $(ALLTARGET) VERSION=$(VERSION) EXAMPLE=lin_solve
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


# Testing purpose
flags:
	@echo "$(VERSION): $(FFLAGS)"
allflags:
	$(MAKE) allversions_unsafe ALLTARGET=flags