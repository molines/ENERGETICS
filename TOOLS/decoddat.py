#!/usr/bin/env python
""" 
 This script allows to print the date of each time frame in the file
 assuming that the time unit are well documented.
"""
import os,sys
#sys.path.append("/home/jouanno/Tools/MyPython/")
from netCDF4 import Dataset as netcdf
import datetime
import netcdftime as nctime
import numpy as np
#
if len(sys.argv) == 2:
   cfile=sys.argv[-1]
else:
   print "USAGE :", sys.argv[0], "FILE"
   quit()

cfile=sys.argv[-1]
print " working for file :", cfile
ncfile=netcdf(cfile,'r')
time_counter=ncfile.variables['time_counter']
units=time_counter.getncattr('units')
cal='standard'
cal=time_counter.getncattr('calendar')
time=ncfile.variables['time_counter'][:]
cdftime=nctime.utime(units,calendar=cal)
dat=cdftime.num2date(time)
npl=len(dat)

for i in range(0,npl) :
    print i+1 , dat[i]
quit()

