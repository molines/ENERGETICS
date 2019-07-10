#!/bin/python
import xarray as xr
# isel method is use to select data by index
# GRID 2D
# 1 - open dataset
data = xr.open_dataset("http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-monthly-grid2D",decode_cf=True)
# 2 - Select area - time
ext_ind = data.isel({"x":slice(2266,3979),"y":slice(1320,2745),"time_counter":145})
# 3 - Write on disk
ext_ind.to_netcdf("GLORYS12V1_1mAV_200401_grid2D.nc")
# GRID T
data = xr.open_dataset("http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-monthly-gridT")
ext_ind = data.isel({"x":slice(2266,3979),"y":slice(1320,2745),"time_counter":145})
ext_ind.to_netcdf("GLORYS12V1_1mAV_200401_gridT.nc")
# GRID S
data = xr.open_dataset("http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-monthly-gridS")
ext_ind = data.isel({"x":slice(2266,3979),"y":slice(1320,2745),"time_counter":145})
ext_ind.to_netcdf("GLORYS12V1_1mAV_200401_gridS.nc")
# GRID U
data = xr.open_dataset("http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-monthly-gridU")
ext_ind = data.isel({"x":slice(2266,3979),"y":slice(1320,2745),"time_counter":145}).vozocrtx
ext_ind.to_netcdf("GLORYS12V1_1mAV_200401_gridU.nc")
# GRID V
data = xr.open_dataset("http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-monthly-gridV")
ext_ind = data.isel({"x":slice(2266,3979),"y":slice(1320,2745),"time_counter":145}).vomecrty
ext_ind.to_netcdf("GLORYS12V1_1mAV_200401_gridV.nc")
# ICEMOD
data = xr.open_dataset("http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-monthly-icemod")
ext_ind = data.isel({"x":slice(2266,3979),"y":slice(1320,2745),"time_counter":145})
ext_ind.to_netcdf("GLORYS12V1_1mAV_200401_icemod.nc")
