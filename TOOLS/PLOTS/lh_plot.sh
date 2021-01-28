#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=40
#SBATCH --ntasks-per-node=40
#SBATCH --threads-per-core=1
#SBATCH --hint=nomultithread
#SBATCH -A cli@cpu
#SBATCH -J JOB_
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=2:30:00
#SBATCH --exclusive

set -x

vp="-100 -10 20 70"
zoom="1 1  1369 1019"
figs=./fig_lh
var=LH
pal=bwr
charpal=nrl
proj=merc     # merc cyl 
bckgrd=shadedrelief   # none etopo shadedrelief bluemarble
vmax=-9999
vmin=-9999
width=7   # Plot frame in inches
height=6
res=i     # resolution of the coast line c l i h f 
klev=-1
dep=10
depv="deptht"


xstep=30
ystep=15

y1=2006
y2=2006

mkdir -p $figs

nmax=1
n=0
for y in $(seq $y1 $y2 ) ; do

for f in eNATL36X-ENERGETICS_y2006m03d15h00_wrf2D.nc ; do
   ff=$(basename $f )
   g=${ff%.nc} 
   if [ ! -f $figs/$g.png ] ; then
     ( 
      python_plot.py -i $g -v $var  -p $pal -proj $proj -xstep $xstep -ystep $ystep \
            -wij $zoom -wlonlat $vp  -d $figs -bckgrd $bckgrd -vmax $vmax -vmin $vmin \
            -figsz $width $height -res $res 
       ) &
      
    n=$(( n + 1 ))
    if [ $n = $nmax ] ; then
          wait
          n=0
    fi
   else 
     echo $g.png already done
   fi
done
done
wait
