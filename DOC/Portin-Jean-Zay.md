# Note for the porting of the  modeling tools on Jean-Zay

## Environment:
 `$HOME` already set  
 `WORKDIR=/gpfsddn2/gpfswork/rech/fqx/rcli002`
 `DDIR=/gpfsssd/scratch/rech/fqx/rcli002`

## Versioning with git :

```
  export HTTPS_PROXY=https://prodprox.idris.fr:3128
  export https_proxy=https://prodprox.idris.fr:3128
  export HTTP_PROXY=http://prodprox.idris.fr:3128
  export http_proxy=http://prodprox.idris.fr:3128
```

 ` git clone https://github.com/meom-group/CDFTOOLS`  OK !

## Compilation with netcdf
 * Test with CDFTOOLS:

```
NETCDF=/gpfslocalsup/spack_soft/netcdf/4.7.0/intel-19.0.4-vtvxqookplcvgsso4wqgy4neczdipgfg/
NETCDFF=/gpfslocalsup/spack_soft/netcdf-fortran/4.4.5/intel-19.0.4-nm7vlsr6edc7yqkb2v7l4p6mu4dcgkoy/

NETCDF_INCDIR = $(NETCDF)/include
NETCDF_LIBDIR = $(NETCDF)/lib
NETCDFF_INCDIR = $(NETCDFF)/include
NETCDFF_LIBDIR = $(NETCDFF)/lib
NETCDF_LDFLAGS = -L$(NETCDF_LIBDIR) -lnetcdf -L$(NETCDFF_LIBDIR) -lnetcdff
NCDF = -I$(NETCDF_INCDIR) -I$(NETCDFF_INCDIR) $(NETCDF_LDFLAGS) $(NETCDFF_LDFLAGS)
```
   > compilation OK 

 * Test with SOSIE:
  * need to modify Makefile because netcdff and netcdf not in same location on JeanZay.
   > compilation OK after modif

## Compilation of XIOS 
  * `svn co http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-2.5 xios-2.5_trunk`
  * Compilation OK  with the set of [arch-X64_JEANZAY.*](../TOOLS/FCM_XIOS) files but some warnings:

  ```
  ld: warning: libhdf5_hl.so.100, needed by /gpfslocalsup/spack_soft/netcdf-fortran/4.4.5/intel-19.0.4-nm7vlsr6edc7yqkb2v7l4p6mu4dcgkoy/lib/libnetcdff.so, may conflict with libhdf5_hl.so.10
  ld: warning: libhdf5.so.103, needed by /gpfslocalsup/spack_soft/netcdf-fortran/4.4.5/intel-19.0.4-nm7vlsr6edc7yqkb2v7l4p6mu4dcgkoy/lib/libnetcdff.so, may conflict with libhdf5.so.10
  ld: warning: libhdf5.so.103, needed by /gpfslocalsup/spack_soft/netcdf-fortran/4.4.5/intel-19.0.4-nm7vlsr6edc7yqkb2v7l4p6mu4dcgkoy/lib/libnetcdff.so, may conflict with libhdf5.so.10

  ```
 

## Compilation of NEMO (through DCM4)

   ```
   cd $DEVGIT
   git clone https://github.com/meom-group/DCM DCM_4.0`
   ```

 * Test with NNATL12 (light) config
  * `dcm_mkconfdir_local NNATL12-JZ001`
 * build arch-JEAN-ZAY.fcm

## Tests with NNATL12 (light configuration)
 * I install the very same configuration running on `occigen` (`NNATL12-MP14.4`).
 * Compilation OK, runtools very similar to the occigen run tools. SLURM header slightly modified to mention the partition (cpu_dev or cpu_gct3). Also, number of task per node is now 40 as a maximum.
 * With classical mpi task layout (1 xios per node, 39 nemo per node), the job goes through til the last time step, but then freezes after the restart has been written.
 * A deep analysis of this problem shows that the freezing condition comes from a call to `xios_context_finalize`. `NEMO` task that are connected to xios_server on the **same node** enter in a dead-lock when calling the `xios_context_finalize`
 * A work around that works was to put all the `xios_server.exe` on a single node, without any `nemo4.exe` on the same node: in this case, no freezing observed! 
 * The problem can be reproduced with light xios test programs, and XIOS gurus (Olga, RÃmi) are tackling this blocking problem.
 * The option of having xios dedicated nodes is valid only if we can have only a few xios by nodes! (not clear how to perform such a placement).
 * This problem persists but can be resolved in our case, placing xios_server on dedicated nodes. See below how to perform such a placement. 

## Test with eNATL36X configuration (forced mode).
 * Compilation with DCM OK (!) (`eNATL36X-JZ1`)
 * Retrieving data files from occigen with a ssh tunnel through `ige-meom-cal1.u-ga.fr`. 

### Many issues at run time:
  * Need to use depopulated core computation (see details later on).
  * Weird messages of file not found for forcing files, indicating that the link to workdir is missing on some compute nodes (this problem was not present with the NNATL12 tests) ( _Need to make a ticket_)
  * Violent explosion in the Med Sea 1/2 hour after cold start, even using a (very) small time step (2s !). The analysis shows spurious values near the bottom on the TS initial conditions.
  * Hacking sosie3 was needed to overcome this problem:
    1. add a very deep dummy level in the source fields (a copy of last level), in order to deal with target deptht greater than the max source deptht.
    1. no smoothing in the vertical drowning 
    1. update of the source mask after the vertical drowning and before the horizontal drowning: If original mask is kept  the effect of the vertical drowing is lost ( _to be discussed_ )
  * Once initial condition was improved, the simulation ran for a full day with 20 sec time step. 

### Issues when using the  40 cores per nodes
  * I did many tests for running NEMO+xios on multiples nodes using the 40 cores of each nodes. All tests are crashing with a memory error (which may differ from test to test (?) ): 


  ```
  *** Error in `/gpfsssd/scratch/rech/fqx/rcli002/TMPDIR_eNATL36X-JZ001/./nemo4.exe': malloc(): memory corruption: 0x00000000052dd9e0 ***
  *** Error in `/gpfsssd/scratch/rech/fqx/rcli002/TMPDIR_eNATL36X-JZ001/./nemo4.exe': malloc(): memory corruption: 0x000000000538b020 ***
  ```

   or

  ```
  *** Error in `/gpfsssd/scratch/rech/fqx/rcli002/TMPDIR_eNATL36X-JZ001/./nemo4.exe': corrupted double-linked list: 0x00000000053a3760 ***
  *** Error in `/gpfsssd/scratch/rech/fqx/rcli002/TMPDIR_eNATL36X-JZ001/./nemo4.exe': free(): corrupted unsorted chunks: 0x000000000537f520 ***
  *** Error in `/gpfsssd/scratch/rech/fqx/rcli002/TMPDIR_eNATL36X-JZ001/./nemo4.exe': corrupted size vs. prev_size: 0x00000000052e6ba0 ***
  ```

   All the error seems to happen in a mppsum statement :

  ```
  libmpifort.so.12.  00002AAAAB9B6F10  mpi_allreduce_        Unknown  Unknown
  nemo4.exe          0000000000F95873  lib_mpp_mp_mppsum        5164  lib_mpp.f90
  nemo4.exe          0000000000FD5C95  mppini_mp_mpp_ini        1008  mppini.f90
  nemo4.exe          0000000000FE03BC  mppini_mp_mpp_ini         890  mppini.f90
  nemo4.exe          0000000000FFCD97  mppini_mp_mpp_ini         147  mppini.f90
  nemo4.exe          000000000041EE1D  nemogcm_mp_nemo_i         352  nemogcm.f90
  nemo4.exe          000000000041C6D3  nemogcm_mp_nemo_g         138  nemogcm.f90
  nemo4.exe          000000000041C66D  MAIN__                     18  nemo.f90

  ```

   It is worth noting that this problem was not present with the light version NNATL12.

  * I was able to run the configuration using up to 37 cores per nodes but no more (38 39 and 40 cores/node tests failed with errors as shown above).
  * In some rare intents, failing situation brought to a freeze in mppini.f90, when computing the best domain decomposition, up to time out but with no error messages.

  * I end up with a modification of `mppsum_a_int` subroutine in `lib_mpp.f90`. The faulting call to this routine is issued in mpp_ini when looking for land
eliminataion. Instead of using a temporary rcv buffer, I dedided to use `MPI_IN_PLACE` so that the modified lines are:

  ```
   SUBROUTINE mppsum_a_int( cdname, ptab, kdim, kcom )
      !!----------------------------------------------------------------------
      CHARACTER(len=*),                   INTENT(in   ) ::   cdname  ! name of the calling subroutine
      INTEGER          , INTENT(inout) ::   ptab(:)   ! array or pointer of arrays on which the boundary condition is applied
      INTEGER, OPTIONAL, INTENT(in   ) ::   kdim        ! optional pointer dimension
      INTEGER, OPTIONAL, INTENT(in   ) ::   kcom        ! optional communicator
      !
      INTEGER :: ipi, ii, ierr
      INTEGER :: ierror, ilocalcomm
      INTEGER          , ALLOCATABLE   ::   work(:)
      !!-----------------------------------------------------------------------
      !
      IF( narea == 1 .AND. numcom == -1 ) CALL mpp_report( cdname, ld_glb = .TRUE. )
      !
      ilocalcomm = mpi_comm_oce
      IF( PRESENT(kcom) )   ilocalcomm = kcom
      !
      IF( PRESENT(kdim) ) then
         ipi = kdim
      ELSE
         ipi = SIZE(ptab,1)   ! 1st dimension
      ENDIF
      !
      ALLOCATE(work(ipi))
      IF( ln_timing ) CALL tic_tac(.TRUE., ld_global = .TRUE.)
!     CALL mpi_allreduce( ptab(:), work, ipi, mpi_integer, mpi_sum, ilocalcomm, ierror )
      CALL mpi_allreduce( MPI_IN_PLACE,ptab(:), ipi, mpi_integer, mpi_sum, ilocalcomm, ierror )
      IF( ln_timing ) CALL tic_tac(.FALSE., ld_global = .TRUE.)
!     DO ii = 1, ipi
!        ptab(ii) = work(ii)
!     ENDDO

      DEALLOCATE(work)

   END SUBROUTINE mppsum_a_int
  ```

   * note that `lib_mpp.F90` contains a generic code for all mppsum subroutines... and I had to take `lib_mpp.f90` from `BLD/ppsrc/nemo`. Need to check id this modif is valid for any `mpi_allreduce`.

## Placing xios_server on separated nodes :
  The base line to perform such a placement (nemo4.exe and xios_server.exe on different nodes) is to take advantage of the `SLURM_HOSTFILE`environement variable, combined with the `--multiprog` option of srun.
  * `SLURM_HOSTFILE` point to a `hostfile` file containing as many lines as the total number of MPI tasks (nemo +xios), each line corresponding the a node name.
  * `--multiprog taskfile` option describes the number of task corresponding respectively to nemo or xios. For example :

   ``` 0-999 ./nemo4.exe
       1000-1029 ./xios_server.exe
   ```

  indicate that there will be 1000 nemo4.exe task (from  mpirank 0 to mpirank 999) and 30 xios_server.exe tasks (from mpirank 1000 to mpirank 1029, therefore the total number of tasks being 1030. 

For this example, the hostfile should contains 1030 lines, matching the 1030 MPI tasks. On Jean-Zay there are 40 cores on any computing node, so that a specific node name can be repeated up to 40 times.  Playing with the order of the nodes in the hostfile, we can mimic the binding by node (-m cyclic). This technique is implemented through a bash function (`runcode_mpmd_dp`) available in the [function_4_jean-zay.sh](../DCM_ENERGETICS/RUNTOOLS/lib/function_4_jean-zay.sh)  RUNTOOLS function.

Using this technique, we were able to run various segments of a simulation, without any freezing when finalizing xios.

## Run crash after some days/month of simulation:
  * We experience a crash of the run after some months of simulation. The crash was intercepted by stpctl procedure, but the only information we get back was that T and S were -infinity and SSH  was NaN...
  * Trying to debug this point we recompile the code with -fpe0 option and traceback, and we saw that a floating point invalid error was raised in ice_thd.  
  * We tried to adapt the time step, but without any positive results (decreasing the time-step was causing the program to carsh even earlier !). 
  * Playing around with the namelist_ice, we found that setting :
 
  ```
  &namdyn
  ln_dynRHGADV     = .true.
  /
  &namdyn_rhg
  ln_aEVP       = .true.         !     adaptive rheology (Kimmritz et al. 2016 & 2017)
  /
  ```

  permits to avoid the crash. 

  * more tests need to be performed in order to tackle this problem cleanly, but so far we try to produce a year of simulation ( 2004) with this setting.


 


