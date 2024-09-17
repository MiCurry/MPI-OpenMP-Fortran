FLAGS = -g -fbacktrace -fbounds-check -std=f2003 -Wall -Wunused-dummy-argument

all: cspeed.o
	mpifort $(FLAGS) -o fortran_mpi_example fortran_mpi.F90 cspeed.o

cspeed: cspeed.c
	mpicc $(FLAGS) -c cspeed.c
