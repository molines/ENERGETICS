#!/bin/bash


for f in precip*.nc ; do
  ncatted -a units,precip,m,c,'kg m-2 s-1' $f
done

for f in snow*.nc ; do
  ncatted -a units,snow,m,c,'kg m-2 s-1' $f
done
