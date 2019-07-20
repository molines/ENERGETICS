# Making the model grids
## Ocean 
### Grid file ( coordinates )
  The procedure used to produce the ocean grid is very similar to the procdedure used for eNATL60: using the 3.6 nesting tools with a refinement of 3 from ORCA12 grid.
The region was slightly larger than the targetted grid and is refered as version 2 (v2) of the domain. [Version 1 was a very crude cut from MERCATOR ORCA36 grid].

 `create_coordinates.exe` was used with the following namelist :

 ```
&coarse_grid_files
    parent_coordinate_file = 'ORCA12_coordinates.nc'
    parent_meshmask_file   = 'ORCA12.L46-MAL101.1_mesh_zgr.nc'
/
&nesting
    imin = 2257
    imax = 3975
    jmin = 1362
    jmax = 2723
    rho  = 3
    rhot = 3
    bathy_update = true
    updated_parent_file = 'bathy_updated.nc'
/

 ```

 This produced `eNATL36X_coordinates_v2.0.nc` file, which is used for creating the bathymetry (`create_bathy.exe`)

#### *Resizing of the grid*
  In order to optimize the size of the domain, we extract a subdomain from domain v2.0. We aim at having the final domain in such a way that a coarsening of the horizontal mesk by a factor of 3x3 
leads to an exact subdomain of the ORCA12 grid.  In order to do so from v2 to v3 :

  ```
  ncks -F -d  x,39,5054 -d y,3,3749  V2-file v3-file
  ```

  Doing so, gridsize is 5016 x 3747 and matching points with ORCA12 are:  

  ```
  G36(   2,   2) = ORCA12(2271, 1364)
  G36(5015,3746) = ORCA12(3942, 2612)
  ```

 The **OPERATIONAL** version of coordinates is `eNATL36X_coordinates_time_v3.0.nc`


### Bathymetry
  Bathymetry is created with the nesting tools (`create_bathy.exe`), using the following namelist:

  ```
&bathymetry
    new_topo = true
    elevation_database = 'GEBCO_2014_2D.nc'
    elevation_name = 'elevation'
    smoothing = false
    smoothing_factor = 0.6
    nb_connection_pts = 3
    removeclosedseas = true
    type_bathy_interp = 2        !
/
  ```

#### v2.0
  The [GEBCO_2014_2D.nc](http://www.gebco.net/data_and_products/gridded_bathymetry_data/) (version 20150318) file was used (in the mean-time a more recent version went out ...:( ).   
  This produced `eNATL36_bathy_gebco2014_v2.0.nc` bathymetric files. 

#### v2.1
  In this data set, the coast line needs to be corrected. This was done with
[BMGTOOLS](https://archimer.ifremer.fr/doc/00195/30646/) using a high resolution coastline file, taken from the [NOAA site](https://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html).
  In order to better handle big files, I splitted the original files (bathy and coordinates) into 20 subdomain (5 x 4) using the [splitfile2](https://github.com/molines/JMMTOOLS/blob/master/TOOLS/splitfile2.f90) personal tool. Then each subdomain was corrected, trying to best follow the coasline. This procedure ends up with version 2.1 of the bathymetry (`eNATL36_bathy_gebco2014_v2.1.nc`). 

#### v2.2
 In this file, there were still some unconnected points. So I wrote a new tool ([bat_mark_pool.exe](https://github.com/molines/eNATL60/blob/master/TOOLS/bat_mark_pool.f90)) for the tracking of un-connected points. The cleaned bathymetry after this last treatment is version 2.2 (`eNATL36_bathy_gebco2014_v2.2.nc`). The runoff file was made on the basis of this v2.2 bathymetry ([see more details](./runoff_making.md) )

#### v2.3
 This is just v2.2 with  Corner rise seamounts deeper than 800 m

#### v2.3.1
 This is v2.3 but with the Greenland coast adapted to fit CREG12 runoff expanded to 1/36.

#### v3.3
 This is v2.3 retailed to v3 domain, and with Hudson Bay filled in. Some adjustement near open boudaries

#### v3.3.1
 This is v2.3.1  retailed to v3 domain, nothing else.

#### v3.3.2
 This is v3.3.1 with Hudson Bay filled in and some adjusments near open boudaries. **OPERATIONAL**


### Vertical grid 

  We wil take 150 levels already tested by P. Colombo. The corresponding `namdom` block of namelist is :  
> _Note that we choose a minimum depth of 35m, just like eNATL60, in view of putting tides in the simulation_

  ```
  !-----------------------------------------------------------------------
  &namdom        !   space and time domain (bathymetry, mesh, timestep)
  !-----------------------------------------------------------------------
       nn_bathy    =    1      !  compute (=0) or read (=1) the bathymetry file
       rn_bathy    =    0.     !  value of the bathymetry. if (=0) bottom flat at jpkm1
       nn_closea   =    0      !  remove (=0) or keep (=1) closed seas and lakes (ORCA)
       nn_msh      =    0      !  create (/=0) a mesh file(s) or not (=0)
                               !  if not 0 can be in [1 - 6 ] for drakkar usually 6
       rn_hmin     =   35.     !  min depth of the ocean (>0) or min number of ocean level (<0)
       rn_e3zps_min=   25.     !  partial step thickness is set larger than the minimum of
       rn_e3zps_rat=    0.2    !  rn_e3zps_min and rn_e3zps_rat*e3t, with 0<rn_e3zps_rat<1
                               !
       rn_rdt      =  300.     !  time step for the dynamics (and tracer if nn_acc=0)
       rn_atfp     =    0.1    !  asselin time filter parameter
       nn_acc      =    0      !  acceleration of convergence : =1      used, rdt < rdttra(k)
                                     !                          =0, not used, rdt = rdttra
       rn_rdtmin   = 1080.     !  minimum time step on tracers (used if nn_acc=1)
       rn_rdtmax   = 1080.     !  maximum time step on tracers (used if nn_acc=1)
       rn_rdth     =  800.     !  depth variation of tracer time step  (used if nn_acc=1)
       ln_crs      = .false.      !  Logical switch for coarsening module
       jphgr_msh   =       0               !  type of horizontal mesh
                                           !  = 0 curvilinear coordinate on the sphere read in coordinate.nc
                                           !  = 1 geographical mesh on the sphere with regular grid-spacing
                                           !  = 2 f-plane with regular grid-spacing
                                           !  = 3 beta-plane with regular grid-spacing
                                           !  = 4 Mercator grid with T/U point at the equator
       ppglam0     =  999999.d0            !  longitude of first raw and column T-point (jphgr_msh = 1)
       ppgphi0     =  999999.d0            ! latitude  of first raw and column T-point (jphgr_msh = 1)
       ppe1_deg    =  999999.d0            !  zonal      grid-spacing (degrees)
       ppe2_deg    =  999999.d0            !  meridional grid-spacing (degrees)
       ppe1_m      =  999999.d0            !  zonal      grid-spacing (degrees)
       ppe2_m      =  999999.d0            !  meridional grid-spacing (degrees)
       ppsur       =  -2013.96079017d0     !  ORCA r4, r2 and r05 coefficients
       ppa0        =    42.8600419214d0    ! (default coefficients)
       ppa1        =   -15.4142803532d0    !
       ppkth       =   184.2121644d0       !
       ppacr       =    80.00000000d0      !
       ppdzmin     =        999999.d0      !  Minimum vertical spacing
       pphmax      =        999999.d0      !  Maximum depth
       ldbletanh   =           .TRUE.      !  Use/do not use double tanf function for vertical coordinates
       ppa2        =  57.857101458d0       !  Double tanh function parameters
       ppkth2      =  96.05978744d0        !
       ppacr2      =  39.00000000d0        !
  /
  ```


### Domain_cfg file
  * Construction performed on occigen:
  * I used 20 nodes (560 cores). 10 nodes are not enough.
  * Rebuild the global file with JM tool `mergefile4.exe` (part of the `REBUILD_MPP` DCM tool).
   * Problem with 1D+time variables (z,t) (TBFixed..) Workaround: copy all _var_\_1d variables from a subdomain file (all the same).
  * Try to rebuild with rebuild_nemo .. 
   * Standard rebuild is done without deflation ( and domain_cfg.nc really takes advantage of deflation). 
   * Use a lot of memory (up to 30.2% of a login node). 
   * Finally crashed when writing bottom_level variable (Segmentation Fault) (??).
  * Apply `dcmtk_dom_doc.exe` tool in order to copy the namelist into the domain_cfg file.
  * **OPERATIONAL** file is `eNATL36X_domain_cfg_v3.3.2.nc` (same version than the operational bathymetric file).


## Atmosphere
