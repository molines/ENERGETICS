# Journal of run eNATL36X-JZ003 

## Basic Code and modification
 For this run I used DCM/ENERGETICS which is build upon NEMOREF version https://forge.ipsl.jussieu.fr/nemo/svn/NEMO/branches/2019/dev_r10984_HPC-13_IRRMANN_BDY_optimization at rev 11432.

 Drakkar stuff is incorporated for the management of restart files as well as for the RT instrumentation for monitoring.

 * modification when switching to Prather advection scheme for the ice : comment lines to avoid mpp_wait ...

## Namelist changes according to segment

 | seg | nit000 | nitend | date_fin | rn_rdt |
 | ---: | ---:    | ---:    | ---:      | ---:    |
 | 1   | 1 |  16200 |  y2004m01d15 |  80. |
 | 2   | 16201 |  21600 |  y2004m01d20 |  80. |
 | 3   | 21601 |  31200 |  y2004m01d30 |  90. |
 | 4   | 31201 |  52800 |  y2004m02d29 |  120. |
 | 5   | 52801 |  70800 |  y2004m03d30 |  144. |
 | 6   | 70801 |  88800 |  y2004m04d29 |  144. |
 | 7   | 88801 |  93600 |  y2004m05d04 |  90. |
 | 8   | 93601 |  99000 |  y2004m05d09 |  80. |
 | 9   | 99001 |  103800 |  y2004m05d14 |  90. |
 | 10   | 103801 |  108600 |  y2004m05d19 |  90. |
 | 11   | 108601 |  113400 |  y2004m05d24 |  90. |
 | 12   | 113401 |  118200 |  y2004m05d29 |  90. |
 | 13   | 118201 |  139800 |  y2004m06d28 |  120. |
 | 14   | 139801 |  161400 |  y2004m07d28 |  120. |
 | 15   | 161401 |  183000 |  y2004m08d27 |  120. |
 | 16   | 183001 |  204600 |  y2004m09d26 |  120. |
 | 17   | 204601 |  226200 |  y2004m10d26 |  120. |
 | 18   | 226201 |  247800 |  y2004m11d25 |  120. |
 | 19   | 247801 |  273000 |  y2004m12d30 |  120. |


### OCE

### ICE
