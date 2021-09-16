PROGRAM rotateuv
  !!======================================================================
  !!                     ***  PROGRAM rotateuv  ***
  !!=====================================================================
  !!  ** Purpose : take u10 and v10 from WRF output and convert to 
  !!               u10zo and v10me along geographical coordinates
  !!
  !!  ** Method  : use SINALPHA and COSBETA from WRF geo_em.d01.nc file.
  !!
  !! History :  1.0  : 08/2021  : J.M. Molines : 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER(KIND=4)  :: ji, jj, jt
  INTEGER(KIND=4), PARAMETER :: jptblk=10
  INTEGER(KIND=4)  :: narg, ijarg, iargc
  INTEGER(KIND=4)  :: ncgeom, id, ierr
  INTEGER(KIND=4)  :: ncidu, idu
  INTEGER(KIND=4)  :: ncidug, idug
  INTEGER(KIND=4)  :: ncidv, idv
  INTEGER(KIND=4)  :: ncidvg, idvg
  INTEGER(KIND=4)  :: npi, npj, npt
  INTEGER(KIND=4)  :: itmp, nerror, iit, ijtnxt

  REAL(KIND=4), DIMENSION(:,:)  , ALLOCATABLE :: sinalpha, cosalpha
  REAL(KIND=4), DIMENSION(:,:)  , ALLOCATABLE :: u10, v10
  REAL(KIND=4), DIMENSION(:,:)  , ALLOCATABLE :: rlon, rlat
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: ugeo, vgeo

  REAL(KIND=8), DIMENSION(:)  , ALLOCATABLE :: dtim

  CHARACTER(LEN=255)  :: cldum
  CHARACTER(LEN=255)  :: cf_uwrf
  CHARACTER(LEN=255)  :: cv_uwrf = 'U10'
  CHARACTER(LEN=255)  :: cf_vwrf
  CHARACTER(LEN=255)  :: cv_vwrf = 'V10'
  CHARACTER(LEN=255)  :: cf_ugeo = 'ugeo.nc'
  CHARACTER(LEN=255)  :: cf_vgeo = 'vgeo.nc'
  CHARACTER(LEN=255)  :: cf_geom = 'geo_em.d01.nc'


  !!----------------------------------------------------------------------
  narg = iargc ()

  IF ( narg == 0 ) THEN
     PRINT *,' usage :  rotateuv -u U10-wrf -v V10-wrf -ugeo U10-geo -vgeo V10-geo'
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       Take u10 and v10 from WRF output and convert to  u10zo and v10me'
     PRINT *,'       along geographical coordinates.'
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'       -u U10-wrf : WRF u10 file. '
     PRINT *,'       -u U10-wrf : WRF u10 file. '
     PRINT *,'       -ugeo u10-geo : geographic  u10 file. '
     PRINT *,'       -vgeo v10-geo : geographic  v10 file. '
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'       none so far' 
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       geo_em.d01.nc holding SINALPHA COSALPHA  for WRF grid' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file : given as arguments of -ugeo and -vgeo options.'
     PRINT *,'         variables : u10g  v10g'
     PRINT *,'      '
     PRINT *,'      '
     STOP
  ENDIF

  ijarg = 1 
  DO WHILE ( ijarg <= narg )
     CALL getarg(ijarg, cldum ) ; ijarg=ijarg+1
     SELECT CASE ( cldum )
     CASE ( '-u'   ) ; CALL getarg(ijarg, cf_uwrf ) ; ijarg=ijarg+1
     CASE ( '-v'   ) ; CALL getarg(ijarg, cf_vwrf ) ; ijarg=ijarg+1
     CASE ( '-ugeo') ; CALL getarg(ijarg, cf_ugeo ) ; ijarg=ijarg+1
     CASE ( '-vgeo') ; CALL getarg(ijarg, cf_vgeo ) ; ijarg=ijarg+1
     CASE DEFAULT    ; PRINT *, ' ERROR : ', TRIM(cldum),' : unknown option.'; STOP 1
     END SELECT
  ENDDO

  ! read sinalpha and cosalpha in cf_geom
  ierr = NF90_OPEN(cf_geom,NF90_NOWRITE, ncgeom)
  ierr = NF90_INQ_DIMID(ncgeom,'west_east'  , id) ; ierr = NF90_INQUIRE_DIMENSION(ncgeom, id, len=npi)
  ierr = NF90_INQ_DIMID(ncgeom,'south_north', id) ; ierr = NF90_INQUIRE_DIMENSION(ncgeom, id, len=npj)

  ALLOCATE (sinalpha(npi,npj), cosalpha(npi,npj) )
  ierr = NF90_INQ_VARID(ncgeom,'SINALPHA', id   ) ; ierr = NF90_GET_VAR(ncgeom, id, sinalpha, start=(/1,1,1/), count=(/npi,npj,1/) )
  ierr = NF90_INQ_VARID(ncgeom,'COSALPHA', id   ) ; ierr = NF90_GET_VAR(ncgeom, id, cosalpha, start=(/1,1,1/), count=(/npi,npj,1/) )

  ierr = NF90_CLOSE(ncgeom)

  ! open u10 v10 files and sanity check on domain size
  nerror=0
  !  U10
  ! =====
  ierr = NF90_OPEN(cf_uwrf,NF90_NOWRITE,ncidu)
  ierr = NF90_INQ_DIMID(ncidu,'x',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidu, id, len=itmp)
  IF ( itmp /= npi ) THEN
    PRINT *, '  **** ERROR : geo and u10 dimension x mismatch'
    nerror=nerror+1
  ENDIF

  ierr = NF90_INQ_DIMID(ncidu,'y',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidu, id, len=itmp)
  IF ( itmp /= npj ) THEN
    PRINT *, '  **** ERROR : geo and u10 dimension y mismatch'
    nerror=nerror+1
  ENDIF

  ierr = NF90_INQ_DIMID(ncidu,'time_counter',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidu, id, len=npt)
  !  V10
  ! =====
  ierr = NF90_OPEN(cf_vwrf,NF90_NOWRITE,ncidv)
  ierr = NF90_INQ_DIMID(ncidv,'x',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidv, id, len=itmp)
  IF ( itmp /= npi ) THEN
    PRINT *, '  **** ERROR : geo and v10 dimension x mismatch'
    nerror=nerror+1
  ENDIF
  ierr = NF90_INQ_DIMID(ncidu,'y',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidu, id, len=itmp)
  IF ( itmp /= npj ) THEN
    PRINT *, '  **** ERROR : geo and v10 dimension y mismatch'
    nerror=nerror+1
  ENDIF
  ierr = NF90_INQ_DIMID(ncidv,'time_counter',id ) ; ierr = NF90_INQUIRE_DIMENSION(ncidu, id, len=itmp)
  IF ( itmp /= npt ) THEN
    PRINT *, '  **** ERROR : u10 and v10 dimension time mismatch'
    nerror=nerror+1
  ENDIF
  ! Stop if mismatch encounterd
  IF ( nerror /= 0 ) THEN
    PRINT *, '***** ',nerror,' found !'
    STOP
  ENDIF

  ALLOCATE (u10(npi,npj) , v10(npi,npj)  )
  ALLOCATE (ugeo(npi,npj,jptblk), vgeo(npi,npj,jptblk) )
  ALLOCATE (rlon(npi,npj), rlat(npi,npj) )
  ALLOCATE (dtim(npt) )

  ierr = NF90_INQ_VARID(ncidu,'nav_lon'     ,id ) ; ierr = NF90_GET_VAR(ncidu,id,rlon)
  ierr = NF90_INQ_VARID(ncidu,'nav_lat'     ,id ) ; ierr = NF90_GET_VAR(ncidu,id,rlat)
  ierr = NF90_INQ_VARID(ncidu,'time_counter',id ) ; ierr = NF90_GET_VAR(ncidu,id,dtim)

  ierr = NF90_INQ_VARID(ncidu,cv_uwrf, idu )
  ierr = NF90_INQ_VARID(ncidv,cv_vwrf, idv )

  CALL CreateOutput
  iit=0; ijtnxt=1
  DO jt = 1, npt
     iit=iit+1
     IF ( MOD(jt,10) == 0 ) PRINT *, 'JT= ',jt, iit
     ierr = NF90_GET_VAR(ncidu,idu,u10(:,:),start=(/1,1,jt/), count=(/npi,npj,1/) )
     ierr = NF90_GET_VAR(ncidv,idu,v10(:,:),start=(/1,1,jt/), count=(/npi,npj,1/) )
     DO jj= 1, npj
       DO ji=1, npi
        ! Ugeo = [ROTATION] x Uwrf ! matrix product
        ugeo(ji,jj,iit) =  cosalpha(ji,jj) * u10(ji,jj) + sinalpha(ji,jj) * v10(ji,jj)
        vgeo(ji,jj,iit) = -sinalpha(ji,jj) * u10(ji,jj) + cosalpha(ji,jj) * v10(ji,jj)
       ENDDO
     ENDDO
     IF ( iit == jptblk ) THEN
       ierr = NF90_PUT_VAR(ncidug, idug, ugeo, start=(/1,1,ijtnxt/), count=(/npi,npj,iit/) )
       ierr = NF90_PUT_VAR(ncidvg, idvg, vgeo, start=(/1,1,ijtnxt/), count=(/npi,npj,iit/) )
       ijtnxt=ijtnxt+iit
       iit = 0
     ENDIF
  ENDDO
  IF ( iit /= 0 ) THEN
       ierr = NF90_PUT_VAR(ncidug, idug, ugeo(:,:,1:iit), start=(/1,1,ijtnxt/), count=(/npi,npj,iit/) )
       ierr = NF90_PUT_VAR(ncidvg, idvg, vgeo(:,:,1:iit), start=(/1,1,ijtnxt/), count=(/npi,npj,iit/) )
       iit = 0
  ENDIF
  ierr = NF90_CLOSE( ncidug)
  ierr = NF90_CLOSE( ncidvg)


CONTAINS
  SUBROUTINE CreateOutput
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE CreateOuput  ***
    !!
    !! ** Purpose :   create ugeo and vgeo file
    !!
    !! ** Method  :   netcdf primitive, global variables
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4) :: idx, idy, idt
    INTEGER(KIND=4) :: idlonu, idlonv, idtim
    INTEGER(KIND=4) :: idlatu, idlatv
    !!----------------------------------------------------------------------
    ! UGEO
    ierr = NF90_CREATE(cf_ugeo,NF90_NETCDF4,ncidug)
 
    ierr = NF90_DEF_DIM(ncidug,'x',npi, idx )
    ierr = NF90_DEF_DIM(ncidug,'y',npj, idy )
    ierr = NF90_DEF_DIM(ncidug,'time_counter',NF90_UNLIMITED, idt )
    
    ierr = NF90_DEF_VAR(ncidug,'nav_lon',NF90_FLOAT,(/idx,idy/), idlonu, chunksizes=(/npi,200/), deflate_level=1 )
    ierr = NF90_PUT_ATT(ncidug,idlonu,'units','degrees_east')
    ierr = NF90_DEF_VAR(ncidug,'nav_lat',NF90_FLOAT,(/idx,idy/), idlatu, chunksizes=(/npi,200/), deflate_level=1 )
    ierr = NF90_PUT_ATT(ncidug,idlatu,'units','degrees_north')
    ierr = NF90_DEF_VAR(ncidug,'time_counter', NF90_DOUBLE, (/idt/), idtim )
    ierr = NF90_PUT_ATT(ncidug,idtim,'calendar','gregorian')
    ierr = NF90_PUT_ATT(ncidug,idtim,'units','seconds since 1900-01-01 00:00:00')
    ierr = NF90_PUT_ATT(ncidug,idtim,'time_origin','1900-01-01 00:00:00')
    
    ierr = NF90_DEF_VAR(ncidug,'ugeo', NF90_FLOAT,(/idx,idy,idt/), idug, chunksizes=(/npi,200,1/), deflate_level=1 )
    ierr = NF90_PUT_ATT(ncidug,idug,'units','m s-1')
    ierr = NF90_ENDDEF(ncidug)
    ierr = NF90_PUT_VAR(ncidug,idlonu,rlon)
    ierr = NF90_PUT_VAR(ncidug,idlatu,rlat)
    ierr = NF90_PUT_VAR(ncidug,idtim, dtim)

    ! VGEO
    ierr = NF90_CREATE(cf_vgeo,NF90_NETCDF4,ncidvg)
    ierr = NF90_DEF_DIM(ncidvg,'x',npi, idx )
    ierr = NF90_DEF_DIM(ncidvg,'y',npj, idy )
    ierr = NF90_DEF_DIM(ncidvg,'time_counter',NF90_UNLIMITED, idt )

    ierr = NF90_DEF_VAR(ncidvg,'nav_lon',NF90_FLOAT,(/idx,idy/), idlonv, chunksizes=(/npi,200/), deflate_level=1 )
    ierr = NF90_PUT_ATT(ncidvg,idlonv,'units','degrees_east')
    ierr = NF90_DEF_VAR(ncidvg,'nav_lat',NF90_FLOAT,(/idx,idy/), idlatv, chunksizes=(/npi,200/), deflate_level=1 )
    ierr = NF90_PUT_ATT(ncidvg,idlatv,'units','degrees_north')
    ierr = NF90_DEF_VAR(ncidvg,'time_counter', NF90_DOUBLE, (/idt/), idtim )
    ierr = NF90_PUT_ATT(ncidvg,idtim,'calendar','gregorian')
    ierr = NF90_PUT_ATT(ncidvg,idtim,'units','seconds since 1900-01-01 00:00:00')
    ierr = NF90_PUT_ATT(ncidvg,idtim,'time_origin','1900-01-01 00:00:00')

    ierr = NF90_DEF_VAR(ncidvg,'vgeo', NF90_FLOAT,(/idx,idy,idt/), idvg, chunksizes=(/npi,200,1/), deflate_level=1 )
    ierr = NF90_PUT_ATT(ncidvg,idvg,'units','m s-1')
    ierr = NF90_ENDDEF(ncidvg)
    ierr = NF90_PUT_VAR(ncidvg,idlonv,rlon)
    ierr = NF90_PUT_VAR(ncidvg,idlatv,rlat)
    ierr = NF90_PUT_VAR(ncidvg,idtim, dtim)

  END SUBROUTINE CreateOutput  

END PROGRAM rotateuv
