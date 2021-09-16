#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=24
#SBATCH --threads-per-core=1
#SBATCH -A cli@cpu
#SBATCH --hint=nomultithread
#SBATCH -J WRF_split_var
#SBATCH -e zjob.e%j
#SBATCH -o zjob.o%j
#SBATCH --time=5:30:00
#SBATCH --exclusive


DTADIR=/gpfsstore/rech/cli/rcli002/ENERGETICS/eNATL36X_now/outputs/now02
y1=2004
y2=2006
freq=1h

n=0
TGTDIR=$DDIR/WRFATL12-now2/WRFATL12-now2-S/$freq/
cd $DTADIR

#for var in T2 Q2 U10 V10 RAIN SNOWNCV GLW GSW PSFC; do
for var in PSFC; do
   for y in $( seq $y1 $y2 ) ; do
     tgtdir=$TGTDIR/$y
     mkdir -p $tgtdir
     for m in {01..12} ; do
      for f in now02_${freq}_${y}${m}01_${y}${m}??_wrf2D.nc4 ; do
         g=$tgtdir/WRFATL12-now2_y${y}m${m}_${freq}_$var.nc
         ncks -O -4 -L 1 -v nav_lon,nav_lat,time_counter,$var $f $g &
         n=$(( n + 1 ))
         if [ $n = 36 ] ; then
           wait
           n=0
         fi
      done
     done
    done
   wait
done
wait

