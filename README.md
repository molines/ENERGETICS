# eNATL36
North Atlantic 1/36 model configuration 

Configuration prepared in order to be coupled with WRF at 1/12 degree.

## Specification sheet
### NEMO4 
   * revision
   * parametrization
   * domain decomposition
   * ice 
   * vertical grid (150 level) 
   * namelist
### WRF
### OASIS
### XIOS 
   * revision
   * I/O strategy 
   * xml files

## Input files: 
  NEMO4 requires a config file holding the 3D grid with the metrics. In order to prepare this config file, we need :
### Coordinates files
  * extracted from ORCA36 provided by Mercator (v1)
### Bathymetry
  * extracted from Mercator ORCA36 (v1.0)
    * poor coastline along all the continents
    * no Black Sea/Azov Sea 
  * redo a eNATL36 bathymetry following eNATL60 procedure.
    * from GEBCO last revision
    * BMGTOOLS for correction of the coastlines ( use of splitted files to make BMGTOOLS lighter).
### Runoff
  * Is it necessary in coupled mode ? 
### Initial conditions, BDY conditions 
  * From Mercator GLORYS12 ? which year ?
  * what about WRF ? (ERAinterim or ERA5 ? ) get data from ECMWF ?
### Coupling mask 
