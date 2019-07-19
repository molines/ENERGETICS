# MAkefile for bat_fit
include make.macro

EXEC = bat_fit.exe mergebat.exe bat_patch.exe
ALL: $(EXEC)


bat_fit.exe : bat_fit.f90
	$(F90) $(FFLAGS) bat_fit.f90 -o bat_fit.exe

bat_patch.exe : bat_patch.f90
	$(F90) $(FFLAGS) bat_patch.f90 -o bat_patch.exe


mergebat.exe : mergebat.f90
	$(F90) $(FFLAGS) mergebat.f90 -o mergebat.exe


clean:
	rm -f $(EXEC) *~ *.mod *.o 
