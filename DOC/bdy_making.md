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
 * Build the `bdy_coordinates` files.
 * Build the `bdy data` files.
