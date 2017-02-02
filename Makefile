

FC = gfortran


GIT_VERSION := $(shell git describe --abbrev=4 --dirty --always --tags)

ifeq ($(FC),gfortran)

  #FFLAGS = -O2
  FFLAGS = -O0 -Wall -fcheck=all
  #FFLAGS = -O0 -Wall -fcheck=all -fbacktrace -ffpe-trap=zero,overflow,underflow
  #FFLAGS = -O0 -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=all -fbacktrace
  #FFLAGS = -O3 -Wall -Wextra -Wimplicit-interface -fcheck=all -fbacktrace -ffpe-trap=zero,overflow,underflow,denormal

  FFLAGS += -cpp -DVERSION=\"$(GIT_VERSION)\"

endif
ifeq ($(FC),ifort)

  FFLAGS = -O2
  #FFLAGS = -Ofast

  FFLAGS += -fpp -DVERSION=\"$(GIT_VERSION)\"

endif



# add hdf5 support
LDLIBS = -lhdf5_fortran -lhdf5

# add include and library paths on Mac (for Macports paths)
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  LDINC = -I/opt/local/include
  LDFLAGS = -L/opt/local/lib
endif

# add include and library paths on HIPAS
UNAME_N := $(shell uname -n)
ifeq ($(UNAME_N),hipas.lpl.arizona.edu)
  LDINC = -I/usr/local/include
  LDFLAGS = -L/usr/local/lib
  LDLIBS += -lrt -lz -ldl -lm -Wl,-rpath -Wl,/usr/local/lib

  #LDINC = -I/usr/local/include
  #LDLIB = -L/usr/lib64 -L/usr/local/lib

  #LDINC = -I/usr/local/hdf5/include
  #LDLIB = -L/usr/lib64 -L/usr/local/hdf5/lib
endif



.PHONY: clean docs


hdf5_test.x: HDF5_utils.o main.o 
	$(FC) $(FFLAGS) $(LDINC) -o $@ $^ $(LDFLAGS) $(LDLIBS)

main.o: HDF5_utils.o

%.o : %.f90
	$(FC) $(FFLAGS) $(LDINC) -c -o $@ $<

docs:
	cd docs; doxygen HDF5_utils.doxy

clean:
	rm -f *.o *.mod *.x
