# Note for the porting of the  modeling tools on Jean-Zay

## Environment:
 `$HOME` already set  
 `WORKDIR=/gpfsddn2/gpfswork/rech/fqx/rcli002`
 `DDIR=/gpfsssd/scratch/rech/fqx/rcli002`

## Versioning with git :

```
  export HTTPS_PROXY=https://prodprox.idris.fr:3128
  export https_proxy=https://prodprox.idris.fr:3128
  export HTTP_PROXY=http://prodprox.idris.fr:3128
  export http_proxy=http://prodprox.idris.fr:3128
```

 ` git clone https://github.com/meom-group/CDFTOOLS`  OK !

## Compilation with netcdf
 * Test with CDFTOOLS:

```
NETCDF=/gpfslocalsup/spack_soft/netcdf/4.7.0/intel-19.0.4-vtvxqookplcvgsso4wqgy4neczdipgfg/
NETCDFF=/gpfslocalsup/spack_soft/netcdf-fortran/4.4.5/intel-19.0.4-nm7vlsr6edc7yqkb2v7l4p6mu4dcgkoy/

NETCDF_INCDIR = $(NETCDF)/include
NETCDF_LIBDIR = $(NETCDF)/lib
NETCDFF_INCDIR = $(NETCDFF)/include
NETCDFF_LIBDIR = $(NETCDFF)/lib
NETCDF_LDFLAGS = -L$(NETCDF_LIBDIR) -lnetcdf -L$(NETCDFF_LIBDIR) -lnetcdff
NCDF = -I$(NETCDF_INCDIR) -I$(NETCDFF_INCDIR) $(NETCDF_LDFLAGS) $(NETCDFF_LDFLAGS)
```

## Compilation of XIOS 

## Compilation of NEMO (threough DCM4)

