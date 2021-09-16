# Makefile for splitfold program
include make.macro

ALL: splitfold.x rotateuv.x

splitfold.x: splitfold.f90
	$(F90) $(FFLAGS) splitfold.f90 -o splitfold.x


rotateuv.x: rotateuv.f90
	$(F90) $(FFLAGS) rotateuv.f90 -o rotateuv.x

clean:
	\rm  -rf *.x *.o *.mod *~
