PROGRAM bat_fit
  !!======================================================================
  !!                     ***  PROGRAM  bat_fit  ***
  !!=====================================================================
  !!  ** Purpose :  adjust the coastline/bathymetry (bathy) according to 
  !!                a special mask (combine) deduced from external runoff file.
  !!
  !!  ** Method  : Mask can have 0 ( land in both)
  !!                             1 ( ocean in both, no runoff)
  !!                             2 ( ocean in runoff,  land in original file )
  !!                             3 ( ocean in both, runoff points)
  !!                where combine is 2 set bathy to 35 m 
  !!                where combine is 0 set bathy to 0 [fill fjords]
  !!
  !! History :  1.0  : 07/2019  : J.M. Molines : 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------
  USE netcdf
  !!----------------------------------------------------------------------
  !! ENERGETICS, MEOM 2019
  !! $Id$
  !! Copyright (c) 2019, J.-M. Molines
  !! Software governed by the CeCILL licence 
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER(KIND=4)  :: npiglo, npjglo, itmp
  INTEGER(KIND=4)  :: ncidb, ierr, id, ncidm, idbat

  REAL(KIND=4)                              :: zdep_min = 35.e0  !m
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: zcombine, zbathy

  CHARACTER(LEN=80) :: cf_combine = 'eCREG36_combine_greenland_0004.nc'
  CHARACTER(LEN=80) :: cv_combine = 'Bathymetry'  ! fault of BMGTOOLS :(
  CHARACTER(LEN=80) :: cf_bathy   = 'eCGREG36_bathy_gebco2014_v2.2_0004.nc'
  CHARACTER(LEN=80) :: cv_bathy   = 'Bathymetry'
  CHARACTER(LEN=80) :: cf_copy    

  !!----------------------------------------------------------------------
  ! First copy bathy file and work on the copy
  cf_copy=TRIM(cf_bathy)//'_copy'
  CALL system ('cp '//TRIM(cf_bathy)//' '//TRIM(cf_copy) )
  cf_bathy=cf_copy
  
  ! READ bathy :
  ierr = NF90_OPEN(cf_bathy,NF90_WRITE,ncidb)
  ierr = NF90_INQ_DIMID(ncidb,'x', id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidb,id,len=npiglo )
  ierr = NF90_INQ_DIMID(ncidb,'y', id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidb,id,len=npjglo )

  PRINT *, 'NPIGLO = ', npiglo
  PRINT *, 'NPJGLO = ', npjglo
  ALLOCATE( zbathy(npiglo, npjglo) )
  ierr = NF90_INQ_VARID(ncidb, cv_bathy,idbat)
  ierr = NF90_GET_VAR(ncidb,idbat,zbathy)

  ! Now Read Combine mask
  ierr = NF90_OPEN(cf_combine,NF90_NOWRITE,ncidm)
  ierr = NF90_INQ_DIMID(ncidm,'x', id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidm,id,len=itmp )
        IF ( itmp /= npiglo ) STOP 'Inconsistent npiglo'
  ierr = NF90_INQ_DIMID(ncidm,'y', id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidm,id,len=itmp )
        IF ( itmp /= npjglo ) STOP 'Inconsistent npjglo'

  ALLOCATE( zcombine(npiglo, npjglo) )
  ierr = NF90_INQ_VARID(ncidm, cv_combine,id)
  ierr = NF90_GET_VAR(ncidm,id,zcombine)
  ierr = NF90_CLOSE(ncidm)  ! no more things to do with this file

  WHERE ( zcombine == 2 ) zbathy=zdep_min
  WHERE ( zcombine == 0 ) zbathy=0.e0

  ierr = NF90_PUT_VAR(ncidb,idbat, zbathy)
  ierr = NF90_CLOSE(ncidb)
  
END PROGRAM bat_fit
