# BDY making-of document (and initial conditions).

  * 4 open boundaries : South, North, Hudson, Baltic.
  * For efficiency, Northern boundary might be divided into 2 parts (West : Baffin bay, East: Gin seas).
  * For generic BDY, we will use `bdy_coordinates` descriptions of the boundaries.
  * We plan to use a rimwidth of 10
  * We plan to take revised version of BDY which allows for higher order schemes at the boundaries. Hence a rim-0 must be defined:
    * The OpenBoundary must have 2 ocean points behind the velocity line.
  * We plan to use GLORYS12 daily output

## Getting the GLORYS12 data.
### Data request made at Mercator service desk:
  All index refer to ORCA12 horizontal grid, starting at (1,1).

#### ***_INITIAL CONDITIONS_***
   Monthly mean of January,2004

   ```
   IMIN IMAX JMIN JMAX
   2266 3979 1320 2745
   ```

#### ***_SOUTH BOUNDARY_***
  All boundary data come from glorys12-v1 daily output.

   ```
    40W 15E 11S 5S
   IMIN IMAX JMIN JMAX
   2976 3619 1356 1376
   ```

#### ***_NORTH BOUNDARY_***

   ```
   72W 30E  66N 72N
   IMIN IMAX JMIN JMAX
   2758 3514 2561 2646
   ```

#### ***_HUDSON BAY BOUNDARY_***

   ```
   75W 70W  61N 63N
   IMIN IMAX JMIN JMAX
   2636 2711 2481 2522
   ```

#### ***_BALTIC SEA BOUNDARY_***

   ```
   8E 14E  53N 58N
   IMIN IMAX JMIN JMAX
   3496 3574 2289 2419
   ```

### Data extraction
  According to the evolution of the services at Mercator, GLORYS12v1 data on the native grid are available on the 
[Mercator opendap server](http://tds.mercator-ocean.fr/thredds/catalog.html). The user must do the extraction by itself ( :( ).
 
  A python script example has been provided: [recup_barnier.py](../TOOLS/recup_barnier.py). This script used `xarray` module. Some typos were corrected (*i.e.* `decode_cf` instead of `cf_decode`) and a more generic version was created [get_glorys12.py](../TOOLS/get_glorys12.py). Unfortunatly, this script is not working already (MemoryError or DataSet error randomly).
 The tool has been updated to an operational level (see for example [get_glorys12_nbdy.py](../TOOLS/get_glorys12_nbdy.py) ).  After a first round of extraction we discover that one day is missing between the initial date and 2004-01-01 for gridT, gridU and icemod files. It is OK for grid2D, gridS and gridV. As far as the time-screening of the data is done with record index, this introduced a shift, and required some post adjustment.  On the other hand, `to_netcdf` xarray method is crashing when we force the output to have a time unlimited dimension. Therefore, the 
conversion is done afterward with a trivial `nco` based script ([add_unlim.sh](../TOOLS/add_unlim.sh)). _TBD : improve the time screening using dates instead of index._  

  Jul. 15 2019 : all BDY and Initial condition relevant glorys12 data on glorys grid (50 levels) are stored on occigen `/scratch/cnt0024/hmg2840/molines/BDY36/` (110Gb). (mirror on ige-meom-cal1:/mnt/meom/workdir/molines/BDY36).

### Processing GLORYS12 extraction:
 * Use [SOSIE](https://github.com/brodeau/sosie) for 3D interpolation on eNATL36X grid or subgrid (BDY). 
  For 3D boundary files,  we use the modified sosie in order to get rid of eventually spurious out of range values near the bottom.
 * Build the `bdy_coordinates` files.
  *  For South, Baffin and Gin seas bdy segments, (along constant J coordinates), we use the [bdy_coord_create.f90](../DCM_ENERGETICS/DCMTOOLS/DRAKKAR/NEMO4/tools/BDY_TOOLS/src/bdy_coord_create.f90) tool developped for this purpose. This tool takes nambdy_index namelist block to determine the position of the open boundary.  It takes into account the existence of a rim-0 as an option. (This rim-0 stuff is a new developpement in BDY code aiming at a more consistent way of dealing with higher order numerical schemes near the boundaries). 
  * For Hudson bay segment, we choose to have it unstructured, in order to make it perpendiculat to Hudson Strait. So we first build a `rim-file` using `BMGTOOLS`, which hold a map of the rim numbers for the T points. The starting point for this process is tha surface tmask file. Rim number are encoded from 100 to 100 + Rim_number, in order to keep track of the original mask.  Then, the bdy_coordinates file was created from the RIM file with the [bdy_mk_coordinates_from_file.f90](../DCM_ENERGETICS/DCMTOOLS/DRAKKAR/NEMO4/tools/BDY_TOOLS/src/bdy_mk_coordinates_from_file.f90).  Note that this program assumes that the unstructured BDY segment is a linear segment in the (I,J) space. It seems to be pretty much trickier to build the coordinates bdy file in the case of a curved BDY (different number of U V and T points... ).
 * Build the `bdy data` files.
  The program [bdy_mk_coordinates_from_file.f90](../DCM_ENERGETICS/DCMTOOLS/DRAKKAR/NEMO4/tools/BDY_TOOLS/src/bdy_mk_coordinates_from_file.f90) was also improved and parallelized [bdy_mk_coordinates_from_file._mpp.f90](../DCM_ENERGETICS/DCMTOOLS/DRAKKAR/NEMO4/tools/BDY_TOOLS/src/bdy_mk_coordinates_from_file_mpp.f90), in order to read either coordinates files or rim files for extracting the correct data from the 3D files created at the first step of this process. 

### Processing FES2014 tidal map.
  For using the tidal forcing, we need to provide bdy-files for tides, corresponding to the real and imaginary part of the tidal constituentis for sea level, and ocean velocities.  Starting from the bdy-coordinates file, we basically uses the same process used for T, S, U and V, described above.  The major trick was in the preparation of the tidal data from amplitude/phase data base. 

  Original files provided by F. Lyard are gridded fields ( 5761 x 2881 ) (1/16 deg Lon/lat). Each tidal constituent correspond to 1 file, with amplitudes and phases for elevation ( elevation_a, elevation_G), eastward velocity component ( eastward_a, eastward_G) and northward velocity component (northward_a, northward_G). There are 34 tidal components, coming from  FES2014b simulation.

  * Diurnal : K1, O1, J1, P1, Q1, S1 (6 waves)
  * Semi-diurnal : M2, S2, N2, K2, L2, 2N2, E2, Mu2, Nu2, La2, T2, MKS2, R2 (13 waves)
  * 1/3 diurnal : M3 (1 wave)
  * 1/4 diurnal : M4,  MN4, MS4, N4, S4 (5 waves)
  * 1/6 diurnal : M6 (1 wave)
  * 1/8 diurnal : M8 (1 wave)
  * Long-period : Msf, MSqm, Mf, Mm, Mtm, Sa, Ssa (7 waves)

 In our simulation we will limit the spectrum to the main tidal components (see later).

#### Transforming amplitude, phase to real/imaginary parts.
  This is done with the program tid_conv_ri from [JMM TIDAL_TOOLS](https://github.com/molines/TIDAL_TOOLS.git). 

  We process the original files (on FES2014b grid) and ends up with elevation, eastward and northward variables in separated files, containing `<var>_real`, `<var>_imag`.

#### Interpolation on eNATL36x Grid
  This is done using sosie and considering that elevation is a scalar, both real part and imaginary part, and (eastward, westward) forms a vector, needing a rotation to match the model I,J axis.

 In order to prepare the BDY data we interpolate the files on the subdomains already defined for the other variables (T S U V): South, North, Hudson, Baffin.

#### Adapting the results of the preprocessing to NEMO requirements
   In the namelist block `nambdy_tide`, the variable `filtide` define the root name of the bdydta tidal file.  The code  then will read `<filtide>_gridT.nc` for elevation, `<filtide>_gridU.nc` for eastward  velocities, and  `<filtide>_gridV.nc` for northward velocities. All used tidal components (real and imaginary part) must be in the same file. Variable names are `<Wave>_z1`, `<Wave>_z2` (real, imaginary respectively) for elevation.  Suffixes `_u1`, `_u2` for eastward velocities, `_v1`, `_v2` for northward velocities.

  Data file can be global or limited to BDY segments (as for other BDY data). In case of global file, variable `ln_bdytide_2ddta` is set to `true` in the namelist block `nambdy_tide`.  For the sake of homogeneity with other BDY data, we opt for files limited to the BDY segments.

 A last possibility is offered  in the namelist, wether files corresponds to complex conjugate or not. This point is not clear at this stage. I noticed that in the case of the `eNATL60` configuration, L. Brodeau used `ln_bdytide_conj = .false. `.

 So the adaption for NEMO is essentially a question of renaming variables, and concatenation of used waves into single gridT gridU and gridV files.
