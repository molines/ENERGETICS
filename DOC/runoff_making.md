# Preparing runoff file for eNATL36X
  In this simulation, we still emulate the river runoff as a precipitation spread over the rivermouth. This procedure requires to have a 2D file with 2 variables: (1) **sorunoff**  giving the amount of *precip* (kg/m2/s) to add at the rivermouth. This sorunoff variable is in general a monthly climatology. (2)  **socoefr** representing the rivermouths and used as a mask field for specific actions such as, for example, shutting off the SSS restoring on the rivermouth. For historical reason this socoefr variable has values between 0 and 0.5 (In fact either 0 or 0.5).

In this document we present the procedure used for producing the runoff file. ( It is almost the same procedure than the one used in eNATL60, but with some variant). In order not to spread the different pieces of code used for this purpose, all new code are still in the [eNATL60/TOOLS/](https://github.com/molines/eNATL60/tree/master/TOOLS) repository.

## Required information :
 1. Coordinates file for the domain
 1. Surface mask file.
 1. Bathymetry file of the domain (finaly used as a mask)
 1. [Dai and Trenberth dataset](http://www.cgd.ucar.edu/cas/catalog/surface/dai-runoff/)
    * we use the last update of the monthly dataset (coastal-stns-Vol-monthly.updated-Aug2014.nc)
    * this file is available on __cal1__ at `/mnt/meom/DATA_SET/RUNOFF`

## Required software :
 1. [BMGTOOLS](http://archimer.ifremer.fr/doc/00195/30646/) for editing the rivermouth file
 1. rnf_xxx tools available in this repository, under TOOLS directory. A Makefile is provided to compile the various fortran programs.
 1. Google Earth highly recommended  in order to acurately locate the rivermouths.

## General procedure:
 * Build a rivermouth file. This file is a 2D file initialized from surface *tmask* in which the zone where to spread the river runoff ('rivermouth') will be  defined, by specifying a unique code number corresponding to each river, identical to the Dai Trenberth coding. 
 * Mask the rivermouth file in order to only keep ocean points.
 * Compute runoff file and check the maximum per cell values ( mm/day). If the run-off exeed 150 mm/day (*arbitrary*), iterate on the rivermouth file in order to increase the spreading of the runoff (increase area).
 

## Roadmap :
### Initialise the river_mask file.
  I used `tmask_util` which was renamed `Bathymetry` for BMGTOOLS (!). 

### Prepare kml file with position of Dai-Trenberth river stations
 * This can be done using the bash script [rnf_mk_kml.sh](https://github.com/molines/eNATL60/blob/master/TOOLS/rnf_mk_kml.sh). The resulting kml file requires some hand editing because of some garbage characters coming from the Dai and Trenberth netcdf file. 
 * Then in GOOGLE-EARTH you can open this kml file and you will have the Dai-Trenberth stations appearing as pin points. This will be very usefull for next step.
 
### Build the rivermouth file using BMGTOOLS.
 * Note that BMGTOOLS requires that the coordinate file has a time axis.
 * Note that BMGTOOLS requires that the variable we are working with is named Bathymetry.
 * For big configuration, it is much easier to work with BMGTOOLS with subdomains. The program [splitfile2](https://github.com/molines/JMMTOOLS/blob/master/TOOLS/splitfile2.f90) can be used to explode the full domain into subdomains and then to merge the subdomains back to full domain. You need to explode both coordinates and data file in the same way :

 >  usage :  splitfile -f IN-file  [-s  x-size y-size] [-n i-size j-size] [-M ] 
       
      PURPOSE :
        This program can be used either for spliting an input file into smaller
        files, whose dimensions are specified, or to merge already splitted 
        files.  The interest of this tool is linked with editing tools  such as
        BMGTOOLS that cannot handle very big files. Each sub-file can then be 
        edited easily and the whole file rebuildt after edition.
            Splitted files have the same root name than the input files, with a
        suffix indicating a rank number, from 1 to the total number of spliited
        files, number 1 corresponding to the south-western most domain, last
        number to the north-eastern most domain.
             When using the merge option, the decomposition is read from the 
        rank 1 file and rebuilt file have IN-file_merged name.
        Split the input files into smaller files with dimensions x-size x y-size
       
      ARGUMENTS :
         -f IN-file : specify the name of the file to split or merge.
                      In case of merge, this is the root name without rank 
                      number. (eg for files like config_0001.nc, just pass 
                      config.)
       
      OPTIONS : 1 and only 1 option must be used.
         -s x-size y-size : specify the size of the subdomain for splitting
         -n i-size j-size : specify the number of subdomains in the I and J 
                            direction.
         -M :  merge splitted files. IN-file is the root name in this case.
         -c coordinates file : use coordinate file to patch nav_lon,nav_lat
              where there are land processors
       
      REQUIRED FILES :
         none
       
      OUTPUT : 
        netcdf files : 
              -s or -n options ROOT_file_nnnn.nc where nnnn is a 4 digit number 
                  indicating the rank of the subdomain. ROOT_file is the name
                  of the input file without the extension.
              -M option : IN_file_merg.nc
           
          variables : same than in IN-file
       
      SEE ALSO :
       BMGTOOLS
 
 * With BMGTOOLS define the zone where to apply the runoff for each river, with the help of google-earth images, to better locate the rivermouth.

 * This procedure may take a lot of time, but makes you visit the world !
 
 ### Mask the resulting rivermouth file.
  * When building the rivermouth file with BMGTOOLS, we do not take care of the coastline. So it is mandatory to mask the resulting file in order to keep only ocean points in the file. Otherwise an error will be done when computing the runoff amount on model cells.
  * This is done with [rnf_mask](../TOOLS/rnf_mask.f90) program, using the bathymetric file.
  
  >   usage :  rnf_mask -d DATA-file -b BATHY-file [-vd VAR-data] [-vb VAR-bathy]
           [-o FILE-out] 
       
      PURPOSE :
         Create masked copy of the input data file
       
      ARGUMENTS :
        -d DATA-file  : give the name of the data file
        -b BATHY-file : give the name of the bathymetry 
       
      OPTIONS :
        -vd VAR-data  : give the name of the variable in data file
             default is Bathymetry
        -vb VAR-bathy  : give the name of the variable in bathymetry file
             default is Bathymetry
        -o FILE-out: specify name of output file, instead of <BATHY-new>.wrk
       
       
      OUTPUT : 
        netcdf file : <DATA-file>.wrk or  <FILE-OUT> 
          variables : Bathymetry or VAR-data if specified

### Compute the river runoff
  * This is done with [rnf_compute_runoff36](../TOOLS/rnf_compute_runoff36.f90) program.
   * in its actual shape, the program assume hard coded name for files :
     - river_mouth.nc
     - coastal-stns-Vol-monthly.updated-Aug2014.nc for data file
     - coordinates.nc 
   * see the code for details and eventual changes !
   * This procedure produces a runoff.nc file and a screen display of the list of the rivers used in the file. In particular, on the screen output you have access to the runoff value (in mm/day, annual mean) used on each model cell for the rivers (last column): If values are in excess of 150 mm/day, we come back to rivermouth editing in order to better spread the runoff for faulty rivers.

### Special procedure for Greenland Runoff.
  We start from CREG12 runoff file (courtesy of Claude Talandier, LOPS), which was elaborate from J. Bamber data (Ref ?). Both climatological data and interannual data were provided ( and Greenland runoff show a tremendous positive trend since the late 90's), but so far we have only worked out the climatology.  
#### *Fit the common points between CREG12 and eNATL36X (T points)*
   **Following cropping is related to v2 domain!**

  ```
  CREG12(1:1580,1:913) = eNATL36X(193:4930, 1351:4087)

   ==> eCREG12 : ncks -F -d x,1,1580 -d y 1,913  CREG12 eCREG12
   ==> eCREG36 : ncks -F -d x,192,4931 -d y,1350,4085 eNATL36X  eCREG36
  ```
  A program ([rnf_12_36.exe](https://github.com/molines/eNATL60/blob/master/TOOLS/rnf_12_36.f90)) project the eCREG12 data on eCREG36 by expanding
one eCREG12 grid cell on 9 eCREG36 grid cells. 
  A combination `combine=tmask+2*socoefr` shows the points where eCREG36 runoff fall on eNATL36X land points.  
  In order to speed up the process of having correct Greenland runoff, **we decided to adapt the Greenland coastline to eCREG36.** 
  > Note that having a stable bathymetry (thus a stable coastline) is a blocking point before defining the vertical grid and create the domain_cfg file.  
 
 We worked on the eCREG36 subregion. Using BMGTOOLS for the `combine` variable we were able to edit this variable in order to fill in ocean points laying between the eCREG36 runoff and the actual coastline. At the end of this procedure the edited `combine` variable may have 3 possible values: 1 indicating ocean point w/o runoff, 2 indicating runoff on land, and 3 runoff on common ocean points. In order to work only for the Greenland coast, all combine values  off Greenland coast were set to 1.   
 The Bathymetry file was then fixed using the `combine` value, filling in the point corresponding to 0, and opening the points (setting a depth of 35m) corresponding to a combine value of 2. This
was done with the specific program [bat_fit.exe](../TOOLS/bat_fit.f90). With this procedure we end up with a corrected bathymetry/coastline around Greenland.   
 Always on eCREG36 sub-region, we then worked out the eCREG36_runoff file (sorunoff and socoefr variables) in order to keep only the Greenland contribution ( eCREG36_runoff_Greenland.nc). Then
a simple sum of the eCREG36_runoff_Greenland.nc and eCREG36_runoff_Dai_Trenberth produced the final runoff file (still on eCREG36 domain).  

> `ncflint -w 1.0,1.0  eCREG36_runoff_Greenland.nc eCREG36_runoff_Dai_Trenberth.nc eCREG36_runoff_final.nc`

 The final action was to patch eNATL36X files (Bathymetry and runoff) with the eCREG26 files. This was done with [bat_patch.exe](../TOOLS/bat_patch.f90)

## Resulting files:
 We end-up all this tedious procedure with the final bathymetry and runoff files (on domain V2):

 ```
 eNATL36X_bathy_gebco2014_v2.3.1.nc
 eNATL36X_runoff_v2.2.3.nc
 ```

V2 domain was retailed (see [grid_making](./grid_making.md#resizing-of-the-grid) report) to V3 that will be the operational domain. Hence, bathymetry and runoff were also retailed to V3, with Hudson bay filled in, 
and small adjustments near the open boundaries.

 ```
 eNATL36X_bathy_gebco2014_v3.3.2.nc
 eNATL36X_runoff_v3.3.2.nc
 ```


