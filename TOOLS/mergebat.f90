PROGRAM merge_bat

USE netcdf
IMPLICIT NONE

INTEGER :: npiglo, npjglo
INTEGER :: ncid1, ncid2
INTEGER :: ierr, id, id1


CHARACTER(LEN=80) :: cf_in1='eNATL36X_bathy_gebco2014_v2.0_merg.nc'
CHARACTER(LEN=80) :: cf_in2='eNATL36_bathy_gebco2014_v2.1.nc4'

REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: bathy1, bathy2

ierr = NF90_OPEN(cf_in1,NF90_WRITE,ncid1)
ierr = NF90_INQ_DIMID(ncid1,'x',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid1,id,len=npiglo)
ierr = NF90_INQ_DIMID(ncid1,'y',id) ; ierr = NF90_INQUIRE_DIMENSION(ncid1,id,len=npjglo)

ALLOCATE(bathy1(npiglo,npjglo) )

ierr = NF90_INQ_VARID(ncid1,'Bathymetry',id1)
ierr = NF90_GET_VAR(ncid1,id1,bathy1(:,1:624),start=(/1,1/), count=(/npiglo,624/) )

ierr = NF90_OPEN(cf_in2,NF90_NOWRITE,ncid2)
print *, NF90_STRERROR(ierr)
ierr = NF90_INQ_VARID(ncid2,'Bathymetry',id)
print *, NF90_STRERROR(ierr)
ierr = NF90_GET_VAR(ncid2,id,bathy1(:,625:npjglo),start=(/1,1/), count=(/npiglo,npjglo-624/) )
print *, NF90_STRERROR(ierr)
ierr = NF90_CLOSE(ncid2)
print *, NF90_STRERROR(ierr)

ierr  = NF90_PUT_VAR(ncid1,id1, bathy1 )

ierr = NF90_CLOSE(ncid1)
ierr = NF90_CLOSE(ncid2)


END PROGRAM merge_bat
