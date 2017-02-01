

FC = gfortran


ifeq ($(FC),gfortran)

  #FFLAGS = -O2
  FFLAGS = -O0 -Wall -fcheck=all
  #FFLAGS = -O0 -Wall -fcheck=all -fbacktrace -ffpe-trap=zero,overflow,underflow
  #FFLAGS = -O0 -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=all -fbacktrace
  #FFLAGS = -O3 -Wall -Wextra -Wimplicit-interface -fcheck=all -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal

endif
ifeq ($(FC),ifort)

  FFLAGS = -O2
#FLAGS = -Ofast

endif

# add include and library paths on Mac (for Macports paths)
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  LDINC = -I/opt/local/include
  LDLIB = -L/opt/local/lib
endif

# add hdf5 support 
LDFLAGS = -lhdf5 -lhdf5_fortran


.PHONY: clean docs


hdf5_test.x: HDF5_utils.o main.o 
	$(FC) $(FFLAGS) $(LDINC) $(LDLIB) $(LDFLAGS) -o $@ $^

main.o: HDF5_utils.o

%.o : %.f90
	$(FC) $(FFLAGS) $(LDINC) -c -o $@ $<

docs:
	cd docs; doxygen HDF5_utils.doxy

clean:
	rm -f *.o *.mod *.x
