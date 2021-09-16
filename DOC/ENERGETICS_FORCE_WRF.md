# ENERGETICS: run forced with WRF output from coupled run (now2)
## Description:
This serie of experiments are performed using the exact same NEMO code as the coupled run (now2), but without coupling. For
atmospheric forcing we use the now2 WRF output, saved hourly. Therefore, from the code side, the only differences are within the
namelist where we disable atmospheric coupling and enable atmospheric forcing using NCAR bulk formulae.

A preparation work was necessary in order to make the Atmospheric forcing available to NEMO.

## Preparation of the WRF output for forcing NEMO.
### Choosing relevant WRF output fields.
We process the WRF2D output (hourly) in order to extract the following variables : (raw extraction is saved on Jean-zay:$STORE/
  * T2:

  ```
	float T2(time_counter, y, x) ;
		T2:long_name = "TEMP at 2 M" ;
		T2:units = "K" ;
		T2:online_operation = "average" ;
		T2:interval_operation = "40 s" ;
		T2:interval_write = "1 h" ;
		T2:cell_methods = "time: mean (interval: 40 s)" ;
		T2:coordinates = "time_centered nav_lat nav_lon" ;
  ```

  * Q2

  ```
	float Q2(time_counter, y, x) ;
		Q2:long_name = "QV at 2 M" ;
		Q2:units = "kg kg-1" ;
		Q2:online_operation = "average" ;
		Q2:interval_operation = "40 s" ;
		Q2:interval_write = "1 h" ;
		Q2:cell_methods = "time: mean (interval: 40 s)" ;
		Q2:coordinates = "time_centered nav_lat nav_lon" ;
  ```

  * U10

  ```
	float U10(time_counter, y, x) ;
		U10:long_name = "U at 10 M" ;
		U10:units = "m s-1" ;
		U10:online_operation = "average" ;
		U10:interval_operation = "40 s" ;
		U10:interval_write = "1 h" ;
		U10:cell_methods = "time: mean (interval: 40 s)" ;
		U10:coordinates = "time_centered nav_lat nav_lon" ;
  ```

  * V10

  ```
	float V10(time_counter, y, x) ;
		V10:long_name = "V at 10 M" ;
		V10:units = "m s-1" ;
		V10:online_operation = "average" ;
		V10:interval_operation = "40 s" ;
		V10:interval_write = "1 h" ;
		V10:cell_methods = "time: mean (interval: 40 s)" ;
		V10:coordinates = "time_centered nav_lat nav_lon" ;
  ```

  * PSFC

  ```
	float PSFC(time_counter, y, x) ;
		PSFC:long_name = "SFC PRESSURE" ;
		PSFC:units = "Pa" ;
		PSFC:online_operation = "average" ;
		PSFC:interval_operation = "40 s" ;
		PSFC:interval_write = "1 h" ;
		PSFC:cell_methods = "time: mean (interval: 40 s)" ;
		PSFC:coordinates = "time_centered nav_lat nav_lon" ;
  ```

  * GLW

  ```
	float GLW(time_counter, y, x) ;
		GLW:long_name = "DOWNWARD LONG WAVE FLUX AT GROUND SURFACE" ;
		GLW:units = "W m-2" ;
		GLW:online_operation = "average" ;
		GLW:interval_operation = "40 s" ;
		GLW:interval_write = "1 h" ;
		GLW:cell_methods = "time: mean (interval: 40 s)" ;
		GLW:coordinates = "time_centered nav_lat nav_lon" ;
  ```

  * GSW

  ```
	float GSW(time_counter, y, x) ;
		GSW:long_name = "NET SHORT WAVE FLUX AT GROUND SURFACE" ;
		GSW:units = "W m-2" ;
		GSW:online_operation = "average" ;
		GSW:interval_operation = "40 s" ;
		GSW:interval_write = "1 h" ;
		GSW:cell_methods = "time: mean (interval: 40 s)" ;
		GSW:coordinates = "time_centered nav_lat nav_lon" ;
  ```

  * RAIN

  ```
	float RAIN(time_counter, y, x) ;
		RAIN:long_name = "TIME-STEP TOTAL PRECIPITATION" ;
		RAIN:units = "mm" ;
		RAIN:online_operation = "average" ;
		RAIN:interval_operation = "40 s" ;
		RAIN:interval_write = "1 h" ;
		RAIN:cell_methods = "time: mean (interval: 40 s)" ;
		RAIN:coordinates = "time_centered nav_lat nav_lon" ;
  ```

  * SNOWNCV

  ```
	float SNOWNCV(time_counter, y, x) ;
		SNOWNCV:long_name = "TIME-STEP NONCONVECTIVE SNOW AND ICE" ;
		SNOWNCV:units = "mm" ;
		SNOWNCV:online_operation = "average" ;
		SNOWNCV:interval_operation = "40 s" ;
		SNOWNCV:interval_write = "1 h" ;
		SNOWNCV:cell_methods = "time: mean (interval: 40 s)" ;
		SNOWNCV:coordinates = "time_centered nav_lat nav_lon" ;
  ```

### Required processing:
  * NEMO requires either atmospheric forcing on a regular longitude/latitude grid (then using weight files for interpolation 'on the fly') or
atmospheric forcing already on the ocean grid.
    * We considered both options but at the end we decided to use the atmospheric forcing already on the ocean grid. The rationale for doing so is
that we want to reproduce the coupled run with as few differences as possible. Note that the coupling frequency in the coupled run was 1h and that
the forcing fields are also available at this frequency.
  * Some unit adjustment are necessary for fresh water flux (RAIN and SNOW) [from mm to kg/m2/s].
  * WRF wind component are given on the WRF grid (at M points). U10 is along I coordinate, and V10 is along J coordinate.
    * The local angle of the grid with respect to the North is given in the WRF file describing the grid (SINALPHA, COSALPHA). A specific program was written in order to obtain the u10 (W-E) and V10 (S-N) on the WRF grid, previous the interpolation on NEMO grid. Then during the SOSIE
interpolation procedure for vectorial fields, the ad-hoc rotation is performed in order to have u10 and v10 along the NEMO grid.
  * Specific tools of namelist are gathered under the [PREPARE_TOOLS](../DCM_ENERGETICS_FORCE_WRF/PREPARE_TOOLS) directory. 
    * A copy of the working directory used for preparing the forcing is saved on `jean-zay:STORE/ENERGETICS/PREPARE_FORCING`

### Interpolation on NEMO eNATL36x grid : SOSIE tool
  * We use [SOSIE](https://github.com/brodeau/sosie.git) at commit `8630038227c` (June, 9 2021).
  * When doing interpolation from an irregular grid to another irregular grid, sosie build a specific weight file (sosie_mapping). For eNATL36x
target grid, it takes several hours (> 10 h) to produce this mapping file. This is done once (the first time), then the file is re-used and 
interpolation is quite fast. Think about saving the mapping file for further use!

## Running the experiment
  * We start from restart files (issued by the couple run) September, 30 2004. ( so after 9 month of spinup ) 
  * **eNATL36X-JMFOR** : This experience  was able to  run over 10 days, then it severely crashed. Looking at the code in details, we found that 
the code version was not exactly the coupled one.
  * **eNATL36X-JMFOR2** : This experience actually use the very same code than the coupled one (double checked).  And it crashed the first day of
simulation !
    * A second experiment with the same code but DFS5.2 forcing was OK for at least one month of simulation.  
==> Clear indication that the problem comes from the forcing files
    * Looking at the forcing files then, we descover that there some areas (in particular Northern Black Sea and Azov Sea that have unrealistic
forcing fields (*e.g* t2 about 0&nbsp;K !) Original WRF files show almost regular t2 temperature (between 280 and 300&nbsp;K).   
==> Need to check the SOSIE procedure (WIP)...
    * Find a problem in land-sea mask: Grid file for WRF is named `geo_em.d01.nc`. It has various mask related variables. Basicaly,
2 of them were of interest :`LANDMASK` and `LU_INDEX`.  
   `LANDMASK` is 1 on ground and 0 on water, including continental waters such
as rivers or lakes.  An equivalent `SEAMASK` was built just by taking `1 - LANDMASK`, which follows NEMO mask convention 
(1 on the ocean, 0 on land). [But still the continental waters are marked as 1].   
  `LU_INDEX` stands for Land Usage Index. It uses 21 different index according to the kind of surface.  Ocean points are marked 17.
In order to avoid the problem of continental waters, I took the decision to build the land-sea mask from this file, setting to 1 
(ocean) all points whose usage is marked as 17.  Unfortunatly, in this `LU_INDEX`, Black Sea and Azov Sea are marked 21, which
correspondonds to lakes.  This is the source of the mistake for having unrealistic values over BSAS.  
  A new sosie interpolation was done using `SEAMASK` instead of `LU_INDEX = 17`.   




