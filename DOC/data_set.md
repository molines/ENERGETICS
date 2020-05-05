# DATA SET produced during the coupled run ENERGETICS

## Context
The ENERGETICS project, aimed at producing an ocean-atmosphere  coupled simulation at very high resolution ( 1/36 for the ocean and 1/12 for the atmosphere).
The coupled system turned to be unstable after a little more than 3 years, despite all the efforts made to make it stable.

Therefore, three different runs were performed, named after Sebastien nomenclature now00, now01 and now02
  * **now00** was run for 3 years (2004-2006), and after 3 years, we found many flaws in this simulation:
    * much too warm Caribbean Sea
    * imprint of the sea-ice coverage even when no ice present 
    * The sea-ice problem was found to be a bug in the coupling interface between the ice and the atmosphere. Once corrected we launch now01
  * **now01** was run for 16 months, starting always Jan. 2004. It was stopped because the warm drift in the tropics was still there, not suprisingly.
    * After this warm drift was detected, changes on the atmospheric boundary layer scheme were introduced (_To be described at minimum_), hence leading to now02 
  * **now02** was then run  for a little bit more than 3 years (38 months), always starting Jan. 2004. The model suffered numerical instability and exploded by mid March, 2007.
    * Many tests were performed by Sebastien in order to control the instability (_description of tests_), but none results in stabilizing the model.

Model output  are hourly (1h) for 2D fields or daily (1m) for 3D fields. The initial Data Plan was to save full 3D fields at high frequency (probably 3h). For storage space limitations, the idea was to save  only the last years of the simulation (says 2010-2013) at this high frequency. Unfortunatly, we were not able to go beyond 2006.

## Data set archived for now00 simulation
This data set is reduced and probably useless as a major bug was impacting the sea-ice coupling
  * 1m : from 200401 to 200612 for T U V icemod wrf2D

## Data set archived for now01 simulation
 * 1h : from 20040101 to 20050430  2D fields (surface) for files type T U V 2D wrf2D
 * 1d : from 20040101 to 20050430  3D fields for file type T U  V W icemod wrf2D wrf3D
 * 1m : from  200401 to  200504 file type  wrf2D

## Data set archived for now02 simulation
 * 1h : from 20040101 to 20061231 2D fields (surface) for files type T U V  wrf2D
 * 1d : from 20040101 to 20061231  3D fields for file type T U  V W icemod wrf2D wrf3D
 * 1m : wrf2D not for all month. **to be computed**
 * note 1 : 1h file for V is lost for month 200605
 * note 2 : 1d and 1h data are also available for 20070201 to 20070228 (January  and March are lost). 

## Variables contained in the different file type
### 2D T files

| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| tos(time_counter, y, x) | degC     | sea_surface_temperature    |
| sos(time_counter, y, x) | 1.e-3    | sea_surface_salinity       |
| zos(time_counter, y, x) |      m    | sea_surface_height_above_geoid            |
| mldr10_1(time_counter, y, x) |  m       | Mixed Layer Depth (dsigma = 0.01 wrt 10m)    |
| sbt(time_counter, y, x) | degC    |  sea bottom temperature   |
| wfo(time_counter, y, x) | kg/m/s2     | water_flux_out_of_sea_ice_and_sea_water (up)      |
| qsr_oce(time_counter, y, x) |  W/m2  | net_downward_shortwave_flux_at_sea_water_surface   |
| qns_oce(time_counter, y, x) |  W/m2  | non-solar heat flux at ocean surface (including E-P)             |
| qt_oce(time_counter, y, x) |   W/m2  | surface_downward_heat_flux_in_sea_water             |
| taum(time_counter, y, x) |     N/m2     | wind stress module       |
| precip(time_counter, y, x) |  kg/m2/s     | Total precipitation             |

### 2D U files

| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| e3u(time_counter, depthu, y, x) |  m    |   Vertical metric at U pt |
| uos(time_counter, y, x) |  m/s    |   Surface zonal velocity |
| uo(time_counter, depthu, y, x) |  m/s    |  Zonal velocity |
| tauuo(time_counter, y, x) |  N/m2    |  Zonal wind stress | 

### 2D V files

| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| e3v(time_counter, depthv, y, x) |  m    | Vertical metrics at V Pt | 
| vos(time_counter, y, x) |  m/s    | Surface meridional velocity |
| vo(time_counter, depthv, y, x) |  m/s    | Meridional Velocity | 
| tauvo(time_counter, y, x) | N/m2     |  Meridional wind stress  | 

### 2D 2D files

### 2D wrf2D files

| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| T2(time_counter, y, x) |      |
| TH2(time_counter, y, x) |      |
| Q2(time_counter, y, x) |      |
| RAINCV(time_counter, y, x) |      |
| RAINNCV(time_counter, y, x) |      |
| RAIN(time_counter, y, x) |      |
| SNOWNCV(time_counter, y, x) |      |
| GRAUPELNCV(time_counter, y, x) |      |
| HAILNCV(time_counter, y, x) |      |
| ESNOW(time_counter, y, x) |      |
| GLW(time_counter, y, x) |      |
| GSW(time_counter, y, x) |      |
| LH(time_counter, y, x) |      |
| LH_SEA(time_counter, y, x) |      |
| HFX(time_counter, y, x) |      |
| HFX_SEA(time_counter, y, x) |      |
| UST(time_counter, y, x) |      |
| UST_SEA(time_counter, y, x) |      |
| ZNT(time_counter, y, x) |      |
| U10(time_counter, y, x) |      |
| V10(time_counter, y, x) |      |
| WSPD10(time_counter, y, x) |      |
| UOCE(time_counter, y, x) |      |
| VOCE(time_counter, y, x) |      |
| QFX(time_counter, y, x) |      |
| QFX_SEA(time_counter, y, x) |      |
| SST(time_counter, y, x) |      |
| PSFC(time_counter, y, x) |      |
| TSK(time_counter, y, x) |      |
| SEAICE(time_counter, y, x) |      |
| DQDTICE(time_counter, y, x) |      |

### 3D T files

| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| e3t(time_counter, deptht, y, x) |      |
| thetao(time_counter, deptht, y, x) |      |
| so(time_counter, deptht, y, x) |      |
| tos(time_counter, y, x) |      |
| sos(time_counter, y, x) |      |
| zos(time_counter, y, x) |      |
| sstdcy(time_counter, y, x) |      |
| mldkz5(time_counter, y, x) |      |
| mldr10_1(time_counter, y, x) |      |
| sbt(time_counter, y, x) |      |
| heatc(time_counter, y, x) |      |
| saltc(time_counter, y, x) |      |
| wfo(time_counter, y, x) |      |
| qsr_oce(time_counter, y, x) |      |
| qns_oce(time_counter, y, x) |      |
| qt_oce(time_counter, y, x) |      |
| sfx(time_counter, y, x) |      |
| taum(time_counter, y, x) |      |
| precip(time_counter, y, x) |      |
| snowpre(time_counter, y, x) |      |

### 3D U files

| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| e3u(time_counter, depthu, y, x) |      |
| uos(time_counter, y, x) |      |
| uo(time_counter, depthu, y, x) |      |
| tauuo(time_counter, y, x) 

### 3D V files


| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| e3v(time_counter, depthv, y, x) |      |
| vos(time_counter, y, x) |      |
| vo(time_counter, depthv, y, x) |      |
| tauvo(time_counter, y, x) |      |

### 3D W files


| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| e3w(time_counter, depthw, y, x) |      |
| wo(time_counter, depthw, y, x) |      |
| kz(time_counter, depthw, y, x) |      |

### 3D wrf3D files

| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| nav_lat_grid_U(y_grid_U, x_grid_U) |      |
| nav_lon_grid_U(y_grid_U, x_grid_U) |      |
| lev_M(lev_M) |      |
| nav_lat_grid_V(y_grid_V, x_grid_V) |      |
| nav_lon_grid_V(y_grid_V, x_grid_V) |      |
| nav_lat_grid_M(y_grid_M, x_grid_M) |      |
| nav_lon_grid_M(y_grid_M, x_grid_M) |      |
| lev_W(lev_W) |      |
| U(time_counter, lev_M, y_grid_U, x_grid_U) |      |
| V(time_counter, lev_M, y_grid_V, x_grid_V) |      |
| W(time_counter, lev_W, y_grid_M, x_grid_M) |      |
| PH(time_counter, lev_W, y_grid_M, x_grid_M) |      |
| PHB(lev_W, y_grid_M, x_grid_M) |      |
| T(time_counter, lev_M, y_grid_M, x_grid_M) |      |
| MU(time_counter, y_grid_M, x_grid_M) |      |
| MUB(y_grid_M, x_grid_M) |      |
| P(time_counter, lev_M, y_grid_M, x_grid_M) |      |
| PB(lev_M, y_grid_M, x_grid_M) |      |
| PSFC(time_counter, y_grid_M, x_grid_M) |      |
| HGT(y_grid_M, x_grid_M) |      |
| QVAPOR(time_counter, lev_M, y_grid_M, x_grid_M) |      |
| QCLOUD(time_counter, lev_M, y_grid_M, x_grid_M) |      |
| QRAIN(time_counter, lev_M, y_grid_M, x_grid_M) |      |
| QICE(time_counter, lev_M, y_grid_M, x_grid_M) |      |
| QOTHER(time_counter, lev_M, y_grid_M, x_grid_M) |      |
| TSK(time_counter, y_grid_M, x_grid_M) |      |
| T2(time_counter, y_grid_M, x_grid_M) |      |
| TH2(time_counter, y_grid_M, x_grid_M) |      |
| Q2(time_counter, y_grid_M, x_grid_M) |      |
| CLDFRA(time_counter, lev_M, y_grid_M, x_grid_M) |      |
| ZNT(time_counter, y_grid_M, x_grid_M) |      |
| U10(time_counter, y_grid_M, x_grid_M) |      |
| V10(time_counter, y_grid_M, x_grid_M) |      |
| OLR(time_counter, y_grid_M, x_grid_M) |      |
| CK(time_counter, y_grid_M, x_grid_M) |      |
| CD(time_counter, y_grid_M, x_grid_M) |      |
| GLW(time_counter, y_grid_M, x_grid_M) |      |
| GSW(time_counter, y_grid_M, x_grid_M) |      |
| LH(time_counter, y_grid_M, x_grid_M) |      |
| HFX(time_counter, y_grid_M, x_grid_M) |      |
| UST(time_counter, y_grid_M, x_grid_M) |      |
| PBLH(time_counter, y_grid_M, x_grid_M) |      |
| QFX(time_counter, y_grid_M, x_grid_M) |      |
| RAINCV(time_counter, y_grid_M, x_grid_M) |      |
| RAINNCV(time_counter, y_grid_M, x_grid_M) |      |
| RAIN(time_counter, y_grid_M, x_grid_M) |      |
| SST(time_counter, y_grid_M, x_grid_M) |      |
| SWUPT(time_counter, y_grid_M, x_grid_M) |      |
| SWUPTC(time_counter, y_grid_M, x_grid_M) |      |
| SWDNT(time_counter, y_grid_M, x_grid_M) |      |
| SWDNTC(time_counter, y_grid_M, x_grid_M) |      |
| SWUPB(time_counter, y_grid_M, x_grid_M) |      |
| SWUPBC(time_counter, y_grid_M, x_grid_M) |      |
| SWDNB(time_counter, y_grid_M, x_grid_M) |      |
| SWDNBC(time_counter, y_grid_M, x_grid_M) |      |
| LWUPT(time_counter, y_grid_M, x_grid_M) |      |
| LWUPTC(time_counter, y_grid_M, x_grid_M) |      |
| LWDNT(time_counter, y_grid_M, x_grid_M) |      |
| LWDNTC(time_counter, y_grid_M, x_grid_M) |      |
| LWUPB(time_counter, y_grid_M, x_grid_M) |      |
| LWUPBC(time_counter, y_grid_M, x_grid_M) |      |
| LWDNB(time_counter, y_grid_M, x_grid_M) |      |
| LWDNBC(time_counter, y_grid_M, x_grid_M) |      |
| EXCH_H(time_counter, lev_W, y_grid_M, x_grid_M) |      |
| EXCH_M(time_counter, lev_W, y_grid_M, x_grid_M) |      |
| WSPD10(time_counter, y_grid_M, x_grid_M) |      |

### icemod 

| Variable                |  Units   | Definition                 |
|-------------------------|:--------:|----------------------------|
| simsk(time_counter, y, x) |      |
| simsk05(time_counter, y, x) |      |
| simsk15(time_counter, y, x) |      |
| snvolu(time_counter, y, x) |      |
| sithic(time_counter, y, x) |      |
| sivolu(time_counter, y, x) |      |
| siconc(time_counter, y, x) |      |
| sisali(time_counter, y, x) |      |
| siapnd(time_counter, y, x) |      |
| sivpnd(time_counter, y, x) |      |
| sst_m(time_counter, y, x) |      |
| sss_m(time_counter, y, x) |      |
| sitemp(time_counter, y, x) |      |
| sntemp(time_counter, y, x) |      |
| sittop(time_counter, y, x) |      |
| sitbot(time_counter, y, x) |      |
| sitsni(time_counter, y, x) |      |
| sivelu(time_counter, y, x) |      |
| sivelv(time_counter, y, x) |      |
| sivelo(time_counter, y, x) |      |
| utau_ai(time_counter, y, x) |      |
| vtau_ai(time_counter, y, x) |      |
| utau_oi(time_counter, y, x) |      |
| vtau_oi(time_counter, y, x) |      |
| sidive(time_counter, y, x) |      |
| sishea(time_counter, y, x) |      |
| sistre(time_counter, y, x) |      |
| normstr(time_counter, y, x) |      |
| sheastr(time_counter, y, x) |      |
| isig1(time_counter, y, x) |      |
| isig2(time_counter, y, x) |      |
| isig3(time_counter, y, x) |      |
| qt_oce_ai(time_counter, y, x) |      |
| qt_atm_oi(time_counter, y, x) |      |
| qtr_ice_top(time_counter, y, x) |      |
| qtr_ice_bot(time_counter, y, x) |      |
| qt_ice(time_counter, y, x) |      |
| qsr_ice(time_counter, y, x) |      |
| qns_ice(time_counter, y, x) |      |
| qemp_ice(time_counter, y, x) |      |
| albedo(time_counter, y, x) |      |
| hfxcndtop(time_counter, y, x) |      |
| hfxcndbot(time_counter, y, x) |      |
| hfxsensib(time_counter, y, x) |      |
| sfxice(time_counter, y, x) |      |
| vfxice(time_counter, y, x) |      |
| vfxsnw(time_counter, y, x) |      |

