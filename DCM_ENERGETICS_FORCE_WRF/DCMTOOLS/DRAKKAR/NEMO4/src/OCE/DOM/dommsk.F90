MODULE dommsk
   !!======================================================================
   !!                       ***  MODULE dommsk   ***
   !! Ocean initialization : domain land/sea mask 
   !!======================================================================
   !! History :  OPA  ! 1987-07  (G. Madec)  Original code
   !!            6.0  ! 1993-03  (M. Guyon)  symetrical conditions (M. Guyon)
   !!            7.0  ! 1996-01  (G. Madec)  suppression of common work arrays
   !!             -   ! 1996-05  (G. Madec)  mask computed from tmask
   !!            8.0  ! 1997-02  (G. Madec)  mesh information put in domhgr.F
   !!            8.1  ! 1997-07  (G. Madec)  modification of kbat and fmask
   !!             -   ! 1998-05  (G. Roullet)  free surface
   !!            8.2  ! 2000-03  (G. Madec)  no slip accurate
   !!             -   ! 2001-09  (J.-M. Molines)  Open boundaries
   !!   NEMO     1.0  ! 2002-08  (G. Madec)  F90: Free form and module
   !!             -   ! 2005-11  (V. Garnier) Surface pressure gradient organization
   !!            3.2  ! 2009-07  (R. Benshila) Suppression of rigid-lid option
   !!            3.6  ! 2015-05  (P. Mathiot) ISF: add wmask,wumask and wvmask
   !!            4.0  ! 2016-06  (G. Madec, S. Flavoni)  domain configuration / user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dom_msk       : compute land/ocean mask
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE usrdef_fmask   ! user defined fmask
   USE bdy_oce        ! open boundary
   !
   USE in_out_manager ! I/O manager
   USE iom            ! IOM library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp        ! Massively Parallel Processing library
#if defined key_drakkar
   USE fldread , ONLY : FLD_N    ! for the case shlat2d
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_msk    ! routine called by inidom.F90

   !                            !!* Namelist namlbc : lateral boundary condition *
   REAL(wp)        :: rn_shlat   ! type of lateral boundary condition on velocity
   LOGICAL, PUBLIC :: ln_vorlat  !  consistency of vorticity boundary condition 
   !                                            with analytical eqs.

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dommsk.F90 11536 2019-09-11 13:54:18Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dom_msk( k_top, k_bot )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE dom_msk  ***
      !!
      !! ** Purpose :   Compute land/ocean mask arrays at tracer points, hori-
      !!      zontal velocity points (u & v), vorticity points (f) points.
      !!
      !! ** Method  :   The ocean/land mask  at t-point is deduced from ko_top 
      !!      and ko_bot, the indices of the fist and last ocean t-levels which 
      !!      are either defined in usrdef_zgr or read in zgr_read.
      !!                The velocity masks (umask, vmask, wmask, wumask, wvmask) 
      !!      are deduced from a product of the two neighboring tmask.
      !!                The vorticity mask (fmask) is deduced from tmask taking
      !!      into account the choice of lateral boundary condition (rn_shlat) :
      !!         rn_shlat = 0, free slip  (no shear along the coast)
      !!         rn_shlat = 2, no slip  (specified zero velocity at the coast)
      !!         0 < rn_shlat < 2, partial slip   | non-linear velocity profile
      !!         2 < rn_shlat, strong slip        | in the lateral boundary layer
      !!
      !!      tmask_i : interior ocean mask at t-point, i.e. excluding duplicated
      !!                rows/lines due to cyclic or North Fold boundaries as well
      !!                as MPP halos.
      !!      tmask_h : halo mask at t-point, i.e. excluding duplicated rows/lines
      !!                due to cyclic or North Fold boundaries as well as MPP halos.
      !!
      !! ** Action :   tmask, umask, vmask, wmask, wumask, wvmask : land/ocean mask 
      !!                         at t-, u-, v- w, wu-, and wv-points (=0. or 1.)
      !!               fmask   : land/ocean mask at f-point (=0., or =1., or 
      !!                         =rn_shlat along lateral boundaries)
      !!               tmask_i : interior ocean mask 
      !!               tmask_h : halo mask
      !!               ssmask , ssumask, ssvmask, ssfmask : 2D ocean mask
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(:,:), INTENT(in) ::   k_top, k_bot   ! first and last ocean level
      !
      INTEGER  ::   ji, jj, jk     ! dummy loop indices
      INTEGER  ::   iif, iil       ! local integers
      INTEGER  ::   ijf, ijl       !   -       -
      INTEGER  ::   iktop, ikbot   !   -       -
      INTEGER  ::   ios, inum
      INTEGER ::   ii0, ii1, ij0, ij1
      REAL(wp), ALLOCATABLE, DIMENSION(:,:) ::   zwf   ! 2D workspace
      !!
      NAMELIST/namlbc/ rn_shlat, ln_vorlat
      NAMELIST/nambdy/ ln_bdy ,nb_bdy, ln_coords_file, cn_coords_file,         &
         &             ln_mask_file, cn_mask_file, cn_dyn2d, nn_dyn2d_dta,     &
         &             cn_dyn3d, nn_dyn3d_dta, cn_tra, nn_tra_dta,             &
         &             ln_tra_dmp, ln_dyn3d_dmp, rn_time_dmp, rn_time_dmp_out, &
         &             cn_ice, nn_ice_dta,                                     &
         &             ln_vol, nn_volctl, nn_rimwidth
#if defined key_drakkar
      REAL(wp) :: zshlat           !: working variable
      REAL(wp), DIMENSION(:,:) , ALLOCATABLE :: zshlat2d
      CHARACTER(LEN=255)  :: cn_dir
      LOGICAL  :: ln_shlat2d
      TYPE(FLD_N) :: sn_shlat2d
      !!
      NAMELIST/namlbc_drk/ ln_shlat2d, cn_dir, sn_shlat2d
#endif
      !!---------------------------------------------------------------------
      !
      REWIND( numnam_ref )              ! Namelist namlbc in reference namelist : Lateral momentum boundary condition
      READ  ( numnam_ref, namlbc, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namlbc in reference namelist' )
      REWIND( numnam_cfg )              ! Namelist namlbc in configuration namelist : Lateral momentum boundary condition
      READ  ( numnam_cfg, namlbc, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namlbc in configuration namelist' )
      IF(lwm) WRITE ( numond, namlbc )

#if defined key_drakkar
      REWIND( numnam_ref )              ! Namelist namlbc in reference namelist : Lateral momentum boundary condition
      READ  ( numnam_ref, namlbc_drk, IOSTAT = ios, ERR = 905 )
905   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namlbc_drk in reference namelist' )
      REWIND( numnam_cfg )              ! Namelist namlbc in configuration namelist : Lateral momentum boundary condition
      READ  ( numnam_cfg, namlbc_drk, IOSTAT = ios, ERR = 906 )
906   IF( ios >  0 )   CALL ctl_nam ( ios , 'namlbc_drk in configuration namelist' )
      IF(lwm) WRITE ( numond, namlbc_drk )
#endif
      
      IF(lwp) THEN                  ! control print
         WRITE(numout,*)
         WRITE(numout,*) 'dommsk : ocean mask '
         WRITE(numout,*) '~~~~~~'
         WRITE(numout,*) '   Namelist namlbc'
         WRITE(numout,*) '      lateral momentum boundary cond.    rn_shlat  = ',rn_shlat
         WRITE(numout,*) '      consistency with analytical form   ln_vorlat = ',ln_vorlat 
      ENDIF
      !
      IF(lwp) WRITE(numout,*)
      IF     (      rn_shlat == 0.               ) THEN   ;   IF(lwp) WRITE(numout,*) '   ==>>>   ocean lateral  free-slip'
      ELSEIF (      rn_shlat == 2.               ) THEN   ;   IF(lwp) WRITE(numout,*) '   ==>>>   ocean lateral  no-slip'
      ELSEIF ( 0. < rn_shlat .AND. rn_shlat < 2. ) THEN   ;   IF(lwp) WRITE(numout,*) '   ==>>>   ocean lateral  partial-slip'
      ELSEIF ( 2. < rn_shlat                     ) THEN   ;   IF(lwp) WRITE(numout,*) '   ==>>>   ocean lateral  strong-slip'
      ELSE
         CALL ctl_stop( 'dom_msk: wrong value for rn_shlat (i.e. a negalive value). We stop.' )
      ENDIF
#if defined key_drakkar
      IF ( ln_shlat2d ) THEN
         IF(lwp) WRITE(numout,*) '         READ shlat as a 2D coefficient in a file '
         ALLOCATE (zshlat2d(jpi,jpj) )
         rn_shlat = 9999.  ! set rn_shlat to a dummy value to force fmask modif
         CALL iom_open(TRIM(cn_dir)//'/'//TRIM(sn_shlat2d%clname), inum)
         CALL iom_get (inum, jpdom_data,       sn_shlat2d%clvar, zshlat2d, 1) !
         CALL iom_close(inum)
      ENDIF
#endif

      !  Ocean/land mask at t-point  (computed from ko_top and ko_bot)
      ! ----------------------------
      !
      tmask(:,:,:) = 0._wp
      DO jj = 1, jpj
         DO ji = 1, jpi
            iktop = k_top(ji,jj)
            ikbot = k_bot(ji,jj)
            IF( iktop /= 0 ) THEN       ! water in the column
               tmask(ji,jj,iktop:ikbot  ) = 1._wp
            ENDIF
         END DO  
      END DO
      ii0 = 3644   ;   ii1 = 3644
      ij0 = 3746   ;   ij1 = 3746
      tmask( mi0(ii0):mi1(ii1) , mj0(ij0):mj1(ij1), : ) =  0.0_wp
      !
      ! the following call is mandatory
      ! it masks boundaries (bathy=0) where needed depending on the configuration (closed, periodic...)  
      CALL lbc_lnk( 'dommsk', tmask  , 'T', 1._wp )      ! Lateral boundary conditions

     ! Mask corrections for bdy (read in mppini2)
      REWIND( numnam_ref )              ! Namelist nambdy in reference namelist :Unstructured open boundaries
      READ  ( numnam_ref, nambdy, IOSTAT = ios, ERR = 903)
903   IF( ios /= 0 )   CALL ctl_nam ( ios , 'nambdy in reference namelist' )
      REWIND( numnam_cfg )              ! Namelist nambdy in configuration namelist :Unstructured open boundaries
      READ  ( numnam_cfg, nambdy, IOSTAT = ios, ERR = 904 )
904   IF( ios >  0 )   CALL ctl_nam ( ios , 'nambdy in configuration namelist' )
      ! ------------------------
      IF ( ln_bdy .AND. ln_mask_file ) THEN
         CALL iom_open( cn_mask_file, inum )
         CALL iom_get ( inum, jpdom_data, 'bdy_msk', bdytmask(:,:) )
         CALL iom_close( inum )
         DO jk = 1, jpkm1
            DO jj = 1, jpj
               DO ji = 1, jpi
                  tmask(ji,jj,jk) = tmask(ji,jj,jk) * bdytmask(ji,jj)
               END DO
            END DO
         END DO
      ENDIF
         
      !  Ocean/land mask at u-, v-, and f-points   (computed from tmask)
      ! ----------------------------------------
      ! NB: at this point, fmask is designed for free slip lateral boundary condition
      DO jk = 1, jpk
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1   ! vector loop
               umask(ji,jj,jk) = tmask(ji,jj  ,jk) * tmask(ji+1,jj  ,jk)
               vmask(ji,jj,jk) = tmask(ji,jj  ,jk) * tmask(ji  ,jj+1,jk)
            END DO
            DO ji = 1, jpim1      ! NO vector opt.
               fmask(ji,jj,jk) = tmask(ji,jj  ,jk) * tmask(ji+1,jj  ,jk)   &
                  &            * tmask(ji,jj+1,jk) * tmask(ji+1,jj+1,jk)
            END DO
         END DO
      END DO
      CALL lbc_lnk_multi( 'dommsk', umask, 'U', 1., vmask, 'V', 1., fmask, 'F', 1. )      ! Lateral boundary conditions
 
      ! Ocean/land mask at wu-, wv- and w points    (computed from tmask)
      !-----------------------------------------
      wmask (:,:,1) = tmask(:,:,1)     ! surface
      wumask(:,:,1) = umask(:,:,1)
      wvmask(:,:,1) = vmask(:,:,1)
      DO jk = 2, jpk                   ! interior values
         wmask (:,:,jk) = tmask(:,:,jk) * tmask(:,:,jk-1)
         wumask(:,:,jk) = umask(:,:,jk) * umask(:,:,jk-1)   
         wvmask(:,:,jk) = vmask(:,:,jk) * vmask(:,:,jk-1)
      END DO


      ! Ocean/land column mask at t-, u-, and v-points   (i.e. at least 1 wet cell in the vertical)
      ! ----------------------------------------------
      ssmask (:,:) = MAXVAL( tmask(:,:,:), DIM=3 )
      ssumask(:,:) = MAXVAL( umask(:,:,:), DIM=3 )
      ssvmask(:,:) = MAXVAL( vmask(:,:,:), DIM=3 )


      ! Interior domain mask  (used for global sum)
      ! --------------------
      !
      iif = nn_hls   ;   iil = nlci - nn_hls + 1
      ijf = nn_hls   ;   ijl = nlcj - nn_hls + 1
      !
      !                          ! halo mask : 0 on the halo and 1 elsewhere
      tmask_h(:,:) = 1._wp                  
      tmask_h( 1 :iif,   :   ) = 0._wp      ! first columns
      tmask_h(iil:jpi,   :   ) = 0._wp      ! last  columns (including mpp extra columns)
      tmask_h(   :   , 1 :ijf) = 0._wp      ! first rows
      tmask_h(   :   ,ijl:jpj) = 0._wp      ! last  rows (including mpp extra rows)
      !
      !                          ! north fold mask
      tpol(1:jpiglo) = 1._wp 
      fpol(1:jpiglo) = 1._wp
      IF( jperio == 3 .OR. jperio == 4 ) THEN      ! T-point pivot
         tpol(jpiglo/2+1:jpiglo) = 0._wp
         fpol(     1    :jpiglo) = 0._wp
         IF( mjg(nlej) == jpjglo ) THEN                  ! only half of the nlcj-1 row for tmask_h
            DO ji = iif+1, iil-1
               tmask_h(ji,nlej-1) = tmask_h(ji,nlej-1) * tpol(mig(ji))
            END DO
         ENDIF
      ENDIF
      !
      IF( jperio == 5 .OR. jperio == 6 ) THEN      ! F-point pivot
         tpol(     1    :jpiglo) = 0._wp
         fpol(jpiglo/2+1:jpiglo) = 0._wp
      ENDIF
      !
      !                          ! interior mask : 2D ocean mask x halo mask 
      tmask_i(:,:) = ssmask(:,:) * tmask_h(:,:)


      ! Lateral boundary conditions on velocity (modify fmask)
      ! ---------------------------------------  
      IF( rn_shlat /= 0 ) THEN      ! Not free-slip lateral boundary condition
         !
         ALLOCATE( zwf(jpi,jpj) )
         !
         DO jk = 1, jpk
            zwf(:,:) = fmask(:,:,jk)         

#if defined key_drakkar
            IF (  ln_shlat2d ) THEN   ! use 2D shlat
               DO jj = 2, jpjm1
                  DO ji = fs_2, fs_jpim1   ! vector opt.
                     IF( fmask(ji,jj,jk) == 0. ) THEN
                        fmask(ji,jj,jk) = zshlat2d(ji,jj) * MIN( 1._wp, MAX( zwf(ji+1,jj), zwf(ji,jj+1),   &
                           &                                              zwf(ji-1,jj), zwf(ji,jj-1)  )  )
                     ENDIF
                  END DO
               END DO
               DO jj = 2, jpjm1
                  IF( fmask(1,jj,jk) == 0._wp ) THEN
                     zshlat = zshlat2d(1,jj)
                     fmask(1  ,jj,jk) = zshlat * MIN( 1._wp , MAX( zwf(2,jj), zwf(1,jj+1), zwf(1,jj-1) ) )
                  ENDIF
                  IF( fmask(jpi,jj,jk) == 0._wp ) THEN
                     zshlat = zshlat2d(jpi,jj)
                     fmask(jpi,jj,jk) = zshlat * MIN( 1._wp , MAX( zwf(jpi,jj+1), zwf(jpim1,jj), zwf(jpi,jj-1) ) )
                  ENDIF
               END DO
               DO ji = 2, jpim1
                  IF( fmask(ji,1,jk) == 0._wp ) THEN
                     zshlat = zshlat2d(ji,1)
                     fmask(ji, 1 ,jk) = zshlat * MIN( 1._wp , MAX( zwf(ji+1,1), zwf(ji,2), zwf(ji-1,1) ) )
                  ENDIF
                  IF( fmask(ji,jpj,jk) == 0._wp ) THEN
                     zshlat = zshlat2d(ji,jpj)
                     fmask(ji,jpj,jk) = zshlat * MIN( 1._wp , MAX( zwf(ji+1,jpj), zwf(ji-1,jpj), zwf(ji,jpjm1) ) )
                  ENDIF
               END DO
            ELSE
#endif
            DO jj = 2, jpjm1
               DO ji = fs_2, fs_jpim1   ! vector opt.
                  IF( fmask(ji,jj,jk) == 0._wp ) THEN
                     fmask(ji,jj,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(ji+1,jj), zwf(ji,jj+1),   &
                        &                                           zwf(ji-1,jj), zwf(ji,jj-1)  )  )
                  ENDIF
               END DO
            END DO
            DO jj = 2, jpjm1
               IF( fmask(1,jj,jk) == 0._wp ) THEN
                  fmask(1  ,jj,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(2,jj), zwf(1,jj+1), zwf(1,jj-1) ) )
               ENDIF
               IF( fmask(jpi,jj,jk) == 0._wp ) THEN
                  fmask(jpi,jj,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(jpi,jj+1), zwf(jpim1,jj), zwf(jpi,jj-1) ) )
               ENDIF
            END DO         
            DO ji = 2, jpim1
               IF( fmask(ji,1,jk) == 0._wp ) THEN
                  fmask(ji, 1 ,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(ji+1,1), zwf(ji,2), zwf(ji-1,1) ) )
               ENDIF
               IF( fmask(ji,jpj,jk) == 0._wp ) THEN
                  fmask(ji,jpj,jk) = rn_shlat * MIN( 1._wp , MAX( zwf(ji+1,jpj), zwf(ji-1,jpj), zwf(ji,jpjm1) ) )
               ENDIF
            END DO
#if defined key_drakkar
            ENDIF   ! 2D shlat
#endif
#if defined key_agrif 
            IF( .NOT. AGRIF_Root() ) THEN 
               IF ((nbondi ==  1).OR.(nbondi == 2)) fmask(nlci-1 , :     ,jk) = 0.e0      ! east 
               IF ((nbondi == -1).OR.(nbondi == 2)) fmask(1      , :     ,jk) = 0.e0      ! west 
               IF ((nbondj ==  1).OR.(nbondj == 2)) fmask(:      ,nlcj-1 ,jk) = 0.e0      ! north 
               IF ((nbondj == -1).OR.(nbondj == 2)) fmask(:      ,1      ,jk) = 0.e0      ! south 
            ENDIF 
#endif 
         END DO
         !
         DEALLOCATE( zwf )
#if defined key_drakkar
         IF ( ln_shlat2d ) THEN
           DEALLOCATE (zshlat2d)
         ENDIF
#endif
         !
         CALL lbc_lnk( 'dommsk', fmask, 'F', 1._wp )      ! Lateral boundary conditions on fmask
         !
         ! CAUTION : The fmask may be further modified in dyn_vor_init ( dynvor.F90 ) depending on ln_vorlat
         !
      ENDIF
      
      ! User defined alteration of fmask (use to reduce ocean transport in specified straits)
      ! -------------------------------- 
      !
      CALL usr_def_fmask( cn_cfg, nn_cfg, fmask )
      !
   END SUBROUTINE dom_msk
   
   !!======================================================================
END MODULE dommsk
