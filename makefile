FLAGS = -fbacktrace -fbounds-check -std=f2003 -Wall -Wunused-dummy-argument

mpi: cspeed.o
	mpifort $(FLAGS) -o fortran_mpi fortran_mpi.F90 cspeed.o

hybrid: cspeed.o
	mpifort -fopenmp $(FLAGS) -o fortran_hybrid fortran_mpi.F90 cspeed.o

cspeed: cspeed.c
	mpicc $(FLAGS) -c cspeed.c
