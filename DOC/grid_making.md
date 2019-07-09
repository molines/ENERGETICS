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


### Vertical grid 


## Atmosphere
