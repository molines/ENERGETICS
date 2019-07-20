# ENERGETICS
 This is a computing Great Challenge on Jean Zay supercomputer aiming at runing a ocean/atmosphere coupled system with Ocean configuration at 1/36 deg, and an atmosphere at 1/12 on a North Atlantic domain.

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
  All input files required for this configuration are/will be available on the [MEOM-OPENDAP server](https://ige-meom-opendap.univ-grenoble-alpes.fr/thredds/catalog/meomopendap/extract/ENERGETICS/catalog.html)
  NEMO4 requires a config file holding the 3D grid with the metrics. In order to prepare this config file, we need :
### Coordinates files
  * extracted from ORCA36 provided by Mercator (v1)
  * infered from ORCA12 grid with nesting tools (v2) (see the [grid-Bathymetry making-of](DOC/grid_making.md) document.)
### Vertical grid:
  * use 150 level from Pedro Colombo 1/36 experiment (--> very good overflows at Denmark Strait)  (see the [grid-Bathymetry making-of](DOC/grid_making.md#vertical-grid) document.)
### Bathymetry
  * extracted from Mercator ORCA36 (v1.0)
    * poor coastline along all the continents
    * no Black Sea/Azov Sea 
  * redo a eNATL36 bathymetry following eNATL60 procedure.
    * from GEBCO last revision
    * BMGTOOLS for correction of the coastlines ( use of splitted files to make BMGTOOLS lighter).
    * see the [grid-Bathymetry making-of](DOC/grid_making.md) document.
### Runoff
  * Is it necessary in coupled mode ? (yes)
  * see the [runoff making-of](DOC/runoff_making.md) document.
### Initial conditions
  * From Mercator GLORYS12, 2004/01 monthly mean.
  * BDY conditions from GLORYS12 daily means. (see the [BDY making-of](DOC/bdy_making.md) document).
  * what about WRF ? (ERAinterim or ERA5 ? ) get data from ECMWF ?
### Coupling mask 
