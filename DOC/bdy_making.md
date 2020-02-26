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
  * For Hudson bay segment, we choose to have it unstructured, in order to make it perpendicular to Hudson Strait. So we first build a `rim-file` using `BMGTOOLS`, which hold a map of the rim numbers for the T points. The starting point for this process is the surface tmask file. Rim number are encoded from 100 to 100 + Rim_number, in order to keep track of the original mask.  Then, the bdy_coordinates file was created from the RIM file with the [bdy_mk_coordinates_from_file.f90](../DCM_ENERGETICS/DCMTOOLS/DRAKKAR/NEMO4/tools/BDY_TOOLS/src/bdy_mk_coordinates_from_file.f90).  Note that this program assumes that the unstructured BDY segment is a linear segment in the (I,J) space. It seems to be pretty much trickier to build the coordinates bdy file in the case of a curved BDY (different number of U V and T points... ).
 * Build the `bdy data` files.
  The program [bdy_mk_coordinates_from_file.f90](../DCM_ENERGETICS/DCMTOOLS/DRAKKAR/NEMO4/tools/BDY_TOOLS/src/bdy_mk_coordinates_from_file.f90) was also improved and parallelized [bdy_mk_coordinates_from_file._mpp.f90](../DCM_ENERGETICS/DCMTOOLS/DRAKKAR/NEMO4/tools/BDY_TOOLS/src/bdy_mk_coordinates_from_file_mpp.f90), in order to read either coordinates files or rim files for extracting the correct data from the 3D files created at the first step of this process. 
 * Note that for U and V we did not perform any rotation of the vectors as the GLORYS12 mesh is an ORCA grid just as the eNATL36 grid. 

### Processing FES2014 tidal map.
  For using the tidal forcing, we need to provide bdy-files for tides, corresponding to the real and imaginary part of the tidal constituentis for sea level, and ocean velocities.  Starting from the bdy-coordinates file, we basically uses the same process used for T, S, U and V, described above.  The major trick was in the preparation of the tidal data from amplitude/phase data base. 
#### Preprocessing of FES2014b cotidal data.

#### Prepare boundary files.
  * When using tides on the BDY, NEMO can read either a global map of the tidal constituents, in a single file, or read different files just like for the standard BDY. We choose to read different files for each BDY segment. For each segment, therefore there will be as many files as tidal constituents.
   * The name of the files and names of the variables are hard coded into NEMO (bdytides_init), so it is mandatotry to follow the naming rules:

  Constituent |  Field  |  Real   | Imaginary |  File name |
   ---------- |  ------ |  -----  | --------  |  --------  |
   \<WAVE>     |   SSH   |   z1    |    z2     | \<seg-id>\<WAVE>_gridT.nc |
   \<WAVE>     |   utide |   u1    |    u2     | \<seg-id>\<WAVE>_gridU.nc | 
   \<WAVE>     |   vtide |   v1    |    v2     | \<seg-id>\<WAVE>_gridV.nc | 

   \<WAVE> correspond to the name of the tidal constiruent (*e.g* M2, S2, K1 ...) and \<seg-id> is the keyword identifying the boundary segment. In our case it will be SOUTH, HUD,BAF and GIN.
   
