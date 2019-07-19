PROGRAM bat_patch
  !!======================================================================
  !!                     ***  PROGRAM  bat_path  ***
  !!=====================================================================
  !!  ** Purpose :  Combine to netcdf file, by replacing a sub-area with
  !!                data form smaller file.
  !!
  !!  ** Method  :  Open a copy of the original file, read variable
  !!                Open and read patch variables ( position of patch given
  !!                somehow).
  !!                Apply patch on the sub-area
  !!                Write back patched variable
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

  INTEGER(KIND=4)  :: narg, iargc,ijarg
  INTEGER(KIND=4)  :: npiglo, npjglo
  INTEGER(KIND=4)  :: npipat, npjpat
  INTEGER(KIND=4)  :: ncidb, ierr, id, ncidp, idbat
  INTEGER(KIND=4)  :: iimin, iimax, ijmin, ijmax

  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: zpatch, zbathy

  CHARACTER(LEN=80) :: cf_patch 
  CHARACTER(LEN=80) :: cv_patch
  CHARACTER(LEN=80) :: cf_bathy
  CHARACTER(LEN=80) :: cv_bathy
  CHARACTER(LEN=80) :: cf_copy    
  CHARACTER(LEN=80) :: cdum

  !!----------------------------------------------------------------------
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,"USAGE : bat_patch -b BATHY-file -p PATCH-file -w imin imax jmin jmax "
     PRINT *,"                -v VAR-to-patch"
     PRINT *
     PRINT *,'   PURPOSE:'
     PRINT *,'     Patch a sub area of the bathy domain with a patch file.'
     PRINT *
     PRINT *,'   ARGUMENTS:'
     PRINT *,'     -b BATHY-file : original bathy file to patch. A working copy'
     PRINT *,'               will be used.'
     PRINT *,'     -p Patch-file : file with patched variables (same names).'
     PRINT *,'     -w imin imax jmin jmax : domain to patch. Must correspond to the size'
     PRINT *,'               of the domain in PATCH-file.'
     PRINT *,'     -v VAR-to-patch : variable name used for patching.'
     STOP
  ENDIF

  ijarg=1
  DO WHILE (ijarg <= narg )
    CALL getarg(ijarg,cdum) ; ijarg=ijarg+1
    SELECT CASE (cdum)
    CASE ('-b') ; CALL getarg(ijarg,cf_bathy) ; ijarg=ijarg+1
    CASE ('-p') ; CALL getarg(ijarg,cf_patch) ; ijarg=ijarg+1
    CASE ('-v') ; CALL getarg(ijarg,cv_patch) ; ijarg=ijarg+1 ; cv_bathy = cv_patch
    CASE ('-w') 
       ;        ; CALL getarg(ijarg,cdum) ; ijarg=ijarg+1 ; READ(cdum,*) iimin
       ;        ; CALL getarg(ijarg,cdum) ; ijarg=ijarg+1 ; READ(cdum,*) iimax
       ;        ; CALL getarg(ijarg,cdum) ; ijarg=ijarg+1 ; READ(cdum,*) ijmin
       ;        ; CALL getarg(ijarg,cdum) ; ijarg=ijarg+1 ; READ(cdum,*) ijmax
    CASE DEFAULT 
        STOP ' Unknown option'
    END SELECT
  ENDDO

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

  ! Now Read patch
  ierr = NF90_OPEN(cf_patch,NF90_NOWRITE,ncidp)
  ierr = NF90_INQ_DIMID(ncidp,'x', id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidp,id,len=npipat )
        IF ( npipat /= iimax -iimin + 1 ) STOP 'I-Patch size does not fit'
  ierr = NF90_INQ_DIMID(ncidp,'y', id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidp,id,len=npjpat )
        IF ( npjpat /= ijmax -ijmin + 1 ) STOP 'J-Patch size does not fit'

  ALLOCATE( zpatch(npipat, npjpat) )
  ierr = NF90_INQ_VARID(ncidp, cv_patch,id)
  ierr = NF90_GET_VAR(ncidp,id,zpatch)
  ierr = NF90_CLOSE(ncidp)  ! no more things to do with this file

  zbathy(iimin:iimax,ijmin:ijmax) = zpatch

  ierr = NF90_PUT_VAR(ncidb,idbat, zbathy)
  ierr = NF90_CLOSE(ncidb)
  
END PROGRAM bat_patch
