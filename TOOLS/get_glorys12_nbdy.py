#!/bin/python

# Python script for geting data from the Mercator opendap
#  revised version of the example provided by Mercator.
#  This script can be used for Initial condition and for BDY condition 
#  However, note that this is just an extraction of GLORY12v1, with 50 vertical levels.
#  Extracted data will be processed ahead for producing suitable BDY/Initial conditions

import os
import xarray as xr

# define general path
url="http://tds.mercator-ocean.fr/thredds/dodsC/"
src_set="glorys12v1-daily"

# define zoom  ( take care of index starting from 0 ! )
tgt_name="eNBDY12-v1"
#  2758 3514 2561 2646
imin=2758-1
imax=3514-1
jmin=2561-1
jmax=2646-1

time=145
tag="y2004m01.1d"
t1=4411
t2=4442


# isel method is use to select data by index
# GRID 2D
# 1 - open dataset
typset="grid2D"
dtaset=src_set+"-"+typset
fileout=tgt_name+"_"+tag+"_"+typset+".nc"

if not os.path.isfile(fileout):
    data = xr.open_dataset(url+dtaset,decode_cf=True)
    print "open "+url+dtaset
    # 2 - Select area - time
#    ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice("2004-01-01","2004-12-31"))
    ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2))
    print "selected area: ", imin, imax, jmin, jmax
    # 3 - Write on disk
    ext_ind.to_netcdf(fileout)

print fileout+"  done"

# WIP


# GRID T
typset="gridT"
var="votemper"
dtaset=src_set+"-"+typset
fileout=tgt_name+"_"+tag+"_"+var+".nc"

if not os.path.isfile(fileout):
    data = xr.open_dataset(url+dtaset,decode_cf=True)
    ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2))
    ext_ind.to_netcdf(fileout)

print fileout+"  done"

# GRID S
typset="gridS"
var="vosaline"
dtaset=src_set+"-"+typset
fileout=tgt_name+"_"+tag+"_"+var+".nc"

if not os.path.isfile(fileout):
    data = xr.open_dataset(url+dtaset,decode_cf=True)
    ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2))
    ext_ind.to_netcdf(fileout)

print fileout+"  done"

# GRID U
typset="gridU"
var="vozocrtx"
dtaset=src_set+"-"+typset
fileout=tgt_name+"_"+tag+"_"+var+".nc"

if not os.path.isfile(fileout):
    data = xr.open_dataset(url+dtaset,decode_cf=True)
    ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2))
    ext_ind.to_netcdf(fileout)

print fileout+"  done"

# GRID V
typset="gridV"
var="vomecrty"
dtaset=src_set+"-"+typset
fileout=tgt_name+"_"+tag+"_"+var+".nc"

if not os.path.isfile(fileout):
    data = xr.open_dataset(url+dtaset,decode_cf=True)
    ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2))
    ext_ind.to_netcdf(fileout)

print fileout+"  done"

# ICEMOD
typset="icemod"
dtaset=src_set+"-"+typset
fileout=tgt_name+"_"+tag+"_"+typset+".nc"

if not os.path.isfile(fileout):
    data = xr.open_dataset(url+dtaset,decode_cf=True)
    ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2))
    ext_ind.to_netcdf(fileout)

print fileout+"  done"
