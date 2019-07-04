PROGRAM merge_bat
  !!======================================================================
  !!                     ***  PROGRAM  merge_bat  ***
  !!=====================================================================
  !!  ** Purpose :  merge two bathyemtric files, after extending the domain
  !!               This is a specific hard coded program
  !!
  !!  ** Method  :  
  !!
  !! History :  1.0 : 06/06/19 : J.M. Molines
  !!----------------------------------------------------------------------
  USE netcdf

  IMPLICIT NONE

  INTEGER :: npiglo, npjglo
  INTEGER :: ncid1, ncid2
  INTEGER :: ierr, id, id1
  INTEGER :: ijlimit=624


  CHARACTER(LEN=80) :: cf_in1='eNATL36X_bathy_gebco2014_v2.0_merg.nc'  ! Only the southern part is corrected.
  CHARACTER(LEN=80) :: cf_in2='eNATL36_bathy_gebco2014_v2.1.nc4'       ! The northern part only, corrected

  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: bathy1
  !-------------------------------------------------------------------------

  ierr = NF90_OPEN(cf_in1,NF90_WRITE,ncid1)
  ierr = NF90_INQ_DIMID(ncid1,'x',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid1,id,len=npiglo)
  ierr = NF90_INQ_DIMID(ncid1,'y',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid1,id,len=npjglo)

  ALLOCATE(bathy1(npiglo,npjglo) )

  ! Read corrected southern part
  ierr = NF90_INQ_VARID(ncid1,'Bathymetry',id1)
  ierr = NF90_GET_VAR(ncid1,id1,bathy1(:,1:ijlimit),start=(/1,1/), count=(/npiglo,ijlimit/) )

  ! read corrected northern parg
  ierr = NF90_OPEN(cf_in2,NF90_NOWRITE,ncid2)
  ierr = NF90_INQ_VARID(ncid2,'Bathymetry',id)
  ierr = NF90_GET_VAR(ncid2,id,bathy1(:,ijlimit+1:npjglo),start=(/1,1/), count=(/npiglo,npjglo-ijlimit/) )
  ierr = NF90_CLOSE(ncid2)

  ! write full bathy
  ierr  = NF90_PUT_VAR(ncid1,id1, bathy1 )

  ierr = NF90_CLOSE(ncid1)
  ierr = NF90_CLOSE(ncid2)

  DEALLOCATE (bathy1)

END PROGRAM merge_bat
