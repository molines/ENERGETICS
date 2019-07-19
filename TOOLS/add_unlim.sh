#!/bin/bash

for f in *.nc ; do
   ncks -4 -L 1 --mk_rec_dmn time_counter $f ${f}4
done
