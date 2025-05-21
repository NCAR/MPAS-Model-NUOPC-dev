# ESMX Build Instructions
## Derecho Modules
Load appropriate set of modules, the following are for building with GNU.
```
$ ml purge
$ ml ncarenv/24.12 gcc/12.4.0 ncarcompilers cray-mpich cmake openblas parallelio
$ ml esmf/8.8.1 netcdf-mpi/4.9.3 parallel-netcdf hdf5-mpi
```

## ESMX Build

```
$ AUTOCLEAN=true \
  PIO=${NCAR_ROOT_PARALLELIO} \
  PnetCDF_ROOT=${NCAR_ROOT_PARALLEL_NETCDF} \
  PnetCDF_MODULE_DIR=${NCAR_ROOT_PARALLEL_NETCDF}/include \
  USE_MPI_F08=0 \
  MPAS_HYDRO=true \
  ESMX_Builder -v --build-jobs=4 --build-type=debug
```

## Running ESMX
Setup a case, one could download a [MPAS testcase](https://www2.mmm.ucar.edu/projects/mpas/test_cases/v7.0/).
Make sure that `esmxRun.yaml, init_atmosphere_model` and `mpas_hydro` are present in the directory.

```
Run initialization executable
$ ./init_atmosphere_model
Run MPAS Hydro
$ mpirun -np 4 ./mpas_hydro
```
