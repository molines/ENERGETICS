#!/bin/python

# Python script for geting data from the Mercator opendap
#  revised version of the example provided by Mercator.
#  This script can be used for Initial condition and for BDY condition 
#  However, note that this is just an extraction of GLORY12v1, with 50 vertical levels.
#  Extracted data will be processed ahead for producing suitable BDY/Initial conditions

import os
import xarray as xr
import calendar



# define general path
url="http://tds.mercator-ocean.fr/thredds/dodsC/"
src_set="glorys12v1-daily"

# define zoom  ( take care of index starting from 0 ! )
tgt_name="eSBDY12-v1"
#  2976 3619 1356 1376
imin=2976-1
imax=3619
jmin=1356-1
jmax=1376

t1=4411 ####  01/01/2004

for year in range(2005,2016) :
  for month in range(1,13) :
     ndays=calendar.monthrange(year,month)[1]
     print ("y%4dm%02d : %02d " % ( year,month, ndays))
     s='{0:02d}'.format(month)
     tag="y"+str(year)+"m"+s+".1d"

     t2=t1+ndays

     typset="grid2D"
     dtaset=src_set+"-"+typset
     fileout=tgt_name+"_"+tag+"_"+typset+".nc"

     if not os.path.isfile(fileout):
        data = xr.open_dataset(url+dtaset,decode_cf=True)
        print "open "+url+dtaset
        ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2)).sossheig
        print "selected area: ", imin, imax, jmin, jmax
        ext_ind.to_netcdf(fileout)

     print fileout+"  done"

     # GRID T
     typset="gridT"
     var="votemper"
     dtaset=src_set+"-"+typset
     fileout=tgt_name+"_"+tag+"_"+var+".nc"

     if not os.path.isfile(fileout):
        data = xr.open_dataset(url+dtaset,decode_cf=True)
        ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2)).votemper
        ext_ind.to_netcdf(fileout)

     print fileout+"  done"

     # GRID S
     typset="gridS"
     var="vosaline"
     dtaset=src_set+"-"+typset
     fileout=tgt_name+"_"+tag+"_"+var+".nc"

     if not os.path.isfile(fileout):
        data = xr.open_dataset(url+dtaset,decode_cf=True)
        ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2)).vosaline
        ext_ind.to_netcdf(fileout)

     print fileout+"  done"

     # GRID U
     typset="gridU"
     var="vozocrtx"
     dtaset=src_set+"-"+typset
     fileout=tgt_name+"_"+tag+"_"+var+".nc"

     if not os.path.isfile(fileout):
        data = xr.open_dataset(url+dtaset,decode_cf=True)
        ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2)).vozocrtx
        ext_ind.to_netcdf(fileout)

     print fileout+"  done"

     # GRID V
     typset="gridV"
     var="vomecrty"
     dtaset=src_set+"-"+typset
     fileout=tgt_name+"_"+tag+"_"+var+".nc"

     if not os.path.isfile(fileout):
        data = xr.open_dataset(url+dtaset,decode_cf=True)
        ext_ind = data.isel(x=slice(imin,imax),y=slice(jmin,jmax),time_counter=slice(t1,t2)).vomecrty
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

     t1=t2
