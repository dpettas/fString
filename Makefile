



COMPILER  = INTEL
#COMPILER=GNU


GFFLAGS = -cpp -O3 -g -m64 -fcray-pointer -ffree-line-length-none 
IFFLAGS = -fpp -O3 -g -traceback 


COMP = $(strip $(COMPILER))

ifeq ($(COMP),GNU)
FC     = gfortran-7
FFLAGS = $(GFFLAGS)
else ifeq ($(COMP),INTEL)
FC     = ifort
FFLAGS = $(IFFLAGS)
endif


build:
	@rm   -rf lib/$(COMP)
	@mkdir -p lib/$(COMP)
	@$(FC) -c $(FFLAGS) ./src/Str.f90 -o lib/$(COMP)/fString.o 
	@mv  fstring.mod lib/$(COMP)
	@ar rcs lib/$(COMP)/libfstring.a lib/$(COMP)/fString.o 
	@rm -rf lib/$(COMP)/fString.o 
cleanall:
	@rm -rf lib
