PROGRAM splitfold
  !!======================================================================
  !!                     ***  PROGRAM  splifold ***
  !!=====================================================================
  !!  ** Purpose : Post process sosie files obtained when interpolating
  !!               on a regular grid:
  !!                 1. shift data for E-W peridicity at 0 E
  !!                 2. Extract data on limited valid domain
  !!
  !!  ** Method  : Read the raw file, shift, select and write smaller file
  !!
  !! History :  1.0  : 08/2021  : J.M. Molines : 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------
  USE netcdf

  IMPLICIT NONE

  INTEGER(KIND=4)     :: jt,ji,jj                   ! dummy loop index
  INTEGER(KIND=4)     :: narg, ijarg, iargc
  INTEGER(KIND=4)     :: ncid, id, ierr
  INTEGER(KIND=4)     :: ncout, idx, idy, idt
  INTEGER(KIND=4)     :: idlon, idlat, idtim, idv
  INTEGER(KIND=4)     :: npi , npj, npt 
  INTEGER(KIND=4)     :: ifld, iimin, iimax, ijmin, ijmax
  INTEGER(KIND=4)     :: ijt
  INTEGER(KIND=4)     :: npio, npjo
  INTEGER(KIND=4), DIMENSION(1)   :: ifold

  REAL(KIND=4)                              ::  vmin, vmax
  REAL(KIND=4), DIMENSION(:)  , ALLOCATABLE ::  rlon1d, rlat1d, rlonshift
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE ::  v2d, v2dshift

  REAL(KIND=8), DIMENSION(:), ALLOCATABLE ::  dtime

  CHARACTER(LEN=255)  :: cf_sosie     ! name of input file
  CHARACTER(LEN=255)  :: cv_sosie     ! name of input variable
  CHARACTER(LEN=255)  :: cf_out='splitfold.nc'       ! name of the output file
  CHARACTER(LEN=255)  :: cldum        ! dummy char variable for parser

  !!----------------------------------------------------------------------
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' usage : splitfold -f SOSIE-file -v SOSIE-var [-o OUT-file] ...'
     PRINT *,'         .... [-w imin imax jmin jmax] '
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'          Post process sosie files obtained when interpolating'
     PRINT *,'          on a regular grid:'
     PRINT *,'             1. shift data for E-W peridicity at 0 E'
     PRINT *,'             2. Extract data on limited valid domain '
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'       -f SOSIE-file : input sosie file obtainerd after interpolation on a '
     PRINT *,'            spheric regular grid.' 
     PRINT *,'       -v SOSIE-var : name of the SOSIE variable to be shifted '
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'       -o OUT-file : output file with the remaining valid area only, and'
     PRINT *,'            no E-W periodicity. Default name is ',TRIM(cf_out),'.'
     PRINT *,'       -w imin imax jmin jmax : IJ windows use to clip the final domain'
     PRINT *,'            If not provided, try to infer it automatically.'
     PRINT *,'            TRICK : use automatic on one t2 file, then force clipping in order'
     PRINT *,'            to have all the files with the same domain.'
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       none so far' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file :  Name given with -o argument.'
     PRINT *,'         variables :  same as in the input file '
     PRINT *,'      '
     PRINT *,'      '
     STOP
  ENDIF

  iimin=-1 ; iimax=-1 ; ijmin=-1 ; ijmax=-1
  ijarg = 1 
  DO WHILE ( ijarg <= narg )
     CALL getarg(ijarg, cldum ) ; ijarg=ijarg+1
     SELECT CASE ( cldum )
     CASE ( '-f'   ) ; CALL getarg(ijarg, cf_sosie ) ; ijarg=ijarg+1
     CASE ( '-v'   ) ; CALL getarg(ijarg, cv_sosie ) ; ijarg=ijarg+1
        ! options
     CASE ( '-o'   ) ; CALL getarg(ijarg, cf_out   ) ; ijarg=ijarg+1
     CASE ( '-w'   ) ; CALL getarg(ijarg, cldum    ) ; ijarg=ijarg+1 ; READ(cldum,*) iimin
        ;          ; CALL getarg(ijarg, cldum    ) ; ijarg=ijarg+1 ; READ(cldum,*) iimax
        ;          ; CALL getarg(ijarg, cldum    ) ; ijarg=ijarg+1 ; READ(cldum,*) ijmin
        ;          ; CALL getarg(ijarg, cldum    ) ; ijarg=ijarg+1 ; READ(cldum,*) ijmax
     CASE DEFAULT    ; PRINT *, ' ERROR : ', TRIM(cldum),' : unknown option.'; STOP 1
     END SELECT
  ENDDO

  ierr = NF90_OPEN(cf_sosie, NF90_NOWRITE, ncid)
  ierr = NF90_INQ_DIMID( ncid,'lon'         , id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npi)
  ierr = NF90_INQ_DIMID( ncid,'lat'         , id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npj)
  ierr = NF90_INQ_DIMID( ncid,'time_counter', id ) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npt)

  PRINT *,'  NPI = ', npi
  PRINT *,'  NPJ = ', npj
  PRINT *,'  NPT = ', npt

  ALLOCATE ( rlon1d(npi), rlat1d(npj) )
  ALLOCATE ( rlonshift(npi) )
  ALLOCATE ( v2d(npi,npj), v2dshift(npi,npj) )
  ALLOCATE ( dtime(npt) )

  ! Work on input file
  ierr = NF90_INQ_VARID( ncid, 'lon', id) ; ierr = NF90_GET_VAR(ncid, id, rlon1d )
  ierr = NF90_INQ_VARID( ncid, 'lat', id) ; ierr = NF90_GET_VAR(ncid, id, rlat1d )
  ierr = NF90_INQ_VARID( ncid, 'time_counter', id) ; ierr = NF90_GET_VAR(ncid, id, dtime )
  PRINT *,' BEFORE SHIFT : '
  PRINT 999, rlon1d(1:5),'...',rlon1d(npi-4:npi)

  ! look for 180 index
  WHERE ( rlon1d > 180. ) rlon1d = rlon1d - 360.
  PRINT *,' AFTER 360 deg offset : '
  PRINT 999, rlon1d(1:5),'...',rlon1d(npi-4:npi)

  ifold=MINLOC(rlon1d)
  ifld = ifold(1)
  PRINT *, 'IFOLD : ', ifld
  PRINT *, '       i   ',ifld -1,'   ', ifld,'   ', ifld+1
  PRINT *, '   ... lon... : ',rlon1d (ifld -1 ), rlon1d(ifld), rlon1d(ifld+1) 

  rlonshift(     1        : npi-ifld+1) = rlon1d( ifld : npi   )
  rlonshift( npi-ifld+2   : npi       ) = rlon1d(   1  : ifld-1)

  PRINT 999, rlonshift(1:5),'...',rlonshift(npi-4:npi)

  ierr = NF90_INQ_VARID(ncid,cv_sosie, id)

  IF ( iimin == -1 ) THEN
     ijt=1  ! work on first frame for geometry 

     ierr = NF90_GET_VAR (ncid, id, v2d, start=(/1,1,ijt/), count=(/npi,npj,1/)  )
     v2dshift(   1         : npi-ifld+1   ,:) = v2d(ifld :   npi  ,:)
     v2dshift(npi-ifld+2   : npi          ,:) = v2d(  1  : ifld-1 ,:)

     !look for ijmin starting from North
     ijmax=npj
     DO jj=npj,1, -1  
        vmin=MINVAL( v2dshift(:,jj) )
        vmax=MAXVAL( v2dshift(:,jj) )
        !   print *, jj, vmin, vmax
        IF ( vmax <= 0 ) THEN
           ijmin = jj+1
           EXIT
        ENDIF
     ENDDO

     ! looking for imin, imax
     DO ji=1,npi
        vmax=MAXVAL(v2dshift(ji, :) )
        IF ( vmax > 0 ) THEN
           iimin = ji-1
           EXIT
        ENDIF
     ENDDO

     DO ji=iimin+2, npi
        vmax=MAXVAL(v2dshift(ji, ijmin +1:npj ) )
        IF ( vmax <=  0 ) THEN
           iimax = ji+1
           EXIT
        ENDIF
     ENDDO
  ENDIF
  npio= iimax-iimin+1
  npjo= ijmax-ijmin+1

  PRINT *, 'IMAX= ', iimax
  PRINT *, 'IMIN= ', iimin
  PRINT *, '   NPI-OUT =', npio
  PRINT *, 'JMAX= ', ijmax
  PRINT *, 'JMIN= ', ijmin
  PRINT *, '   NPJ-OUT =', npjo

  !create output file
  ierr = NF90_CREATE(cf_out,NF90_NETCDF4, ncout)
  ierr = NF90_DEF_DIM(ncout,'x',npio,idx)
  ierr = NF90_DEF_DIM(ncout,'y',npjo,idy)
  ierr = NF90_DEF_DIM(ncout,'time_counter',NF90_UNLIMITED,idt)

  ierr = NF90_DEF_VAR(ncout,'lon',NF90_FLOAT,(/idx/),idlon ,chunksizes=(/npio/),deflate_level=1)
  ierr = NF90_DEF_VAR(ncout,'lat',NF90_FLOAT,(/idy/), idlat,chunksizes=(/npjo/),deflate_level=1 )
  ierr = NF90_DEF_VAR(ncout,'time_counter',NF90_DOUBLE,(/idt/), idtim )

  ierr = NF90_DEF_VAR(ncout,cv_sosie, NF90_FLOAT,(/idx,idy,idt/), idv ,chunksizes=(/npio,200,1/), deflate_level=1 )
  ierr = NF90_ENDDEF(ncout)

  ierr = NF90_PUT_VAR(ncout,idlon, rlonshift(iimin:iimax) )
  ierr = NF90_PUT_VAR(ncout,idlat, rlat1d(ijmin:ijmax) )
  ierr = NF90_PUT_VAR (ncout,idtim, dtime, start=(/1/), count=(/npt/) )

  DO jt=1, npt
     IF (MOD(jt, 75) == 0 ) PRINT *, ' JT = ', jt
     ierr = NF90_GET_VAR (ncid, id, v2d, start=(/1,1,jt/), count=(/npi,npj,1/)  )
     v2dshift(   1         : npi-ifld+1   ,:) = v2d(ifld :   npi  ,:)
     v2dshift(npi-ifld+2   : npi          ,:) = v2d(  1  : ifld-1 ,:)
     ierr = NF90_PUT_VAR (ncout,idv, v2dshift(iimin:iimax,ijmin:ijmax), start=(/1,1,jt/), count=(/npio,npjo,1/) )
  ENDDO
  ierr = NF90_CLOSE(ncid)
  ierr = NF90_CLOSE(ncout)


999 FORMAT (5f10.4,a,5f10.4)

END PROGRAM splitfold
