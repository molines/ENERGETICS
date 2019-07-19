# MAkefile for bat_fit
NETCDF_ROOT=/usr
NCDF = -I$(NETCDF_ROOT)/include -L$(NETCDF_ROOT)/lib -lnetcdff -lnetcdf



F90=gfortran
FFLAGS= -O  $(NCDF) -fno-second-underscore -ffree-line-length-256 

ALL: bat_fit mergebat


bat_fit : bat_fit.f90
	$(F90) $(FFLAGS) bat_fit.f90 -o bat_fit

bat_patch : bat_patch.f90
	$(F90) $(FFLAGS) bat_patch.f90 -o bat_patch


mergebat : mergebat.f90
	$(F90) $(FFLAGS) mergebat.f90 -o mergebat


clean:
	rm -f bat_fit mergebat *~ *.mod *.o 
