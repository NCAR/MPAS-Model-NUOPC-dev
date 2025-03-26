# MPAS-Hydro README

A document for the planning and implementation of coupling WRF-Hydro with MPAS.
The coupling mechanism will use [Earth System Modeling Framework](https://earthsystemmodeling.org/)
(ESMF) and the [National Unified Operational Prediction Capability](https://earthsystemmodeling.org/nuopc)
(NUOPC) interoperability layer, also revered to as a cap.


## Goals
- NUOPC caps
  - two-way interfacing: WRF-Hydro runs the NoahMP land model
  - one-way interfacing: WRF-Hydro passes routing data
- Testcases


## MPAS and MPAS-Hydro Build Process
### Dependencies
- MPI
- Fortran NetCDF
- ParallelIO
- ESMF
- OpenBLAS
- CMake


### Build
#### Retrieve Code
```
Retrieve repository
$ git clone --branch mpas-hydro --recurse-submodules git@github.com:scrasmussen/MPAS-Model.git mpas-model
$ cd mpas-model
```

#### Derecho Modules
This is a working combination for GNU
```
$ module purge
$ module load ncarenv/24.12 gcc/12.4.0 ncarcompilers cray-mpich cmake
$ module load openblas parallelio esmf/8.8.0 netcdf-mpi/4.9.3 parallel-netcdf hdf5-mpi
```

<!-- #### ESMF Dependency -->
<!-- ``` -->
<!-- $ git clone --branch v8.8.0 --single-branch git@github.com:esmf-org/esmf.git -->

<!-- $ ESMF_INSTALL_PREFIX=/path/to/install \ -->
<!--   ESMF_COMPILER=intel \ -->
<!--   ESMF_C=icx \ -->
<!--   ESMF_CXX=icpx \ -->
<!--   ESMF_F90=ifx \ -->
<!--   ESMF_DIR=$(pwd) \ -->
<!--   make -j4 -->
<!-- ``` -->

#### Build Code
##### Make
NOTE: *Currently Preferred* MPAS Makefile modified to use mpas-hydro's CMake build system.
```
$ CORE=atmosphere \
  MPAS_HYDRO=true \
  USE_MPI_F08=false \
  make gnu -j 4
```

##### CMake
NOTE: *Test Only*
Note: the CMake `MPAS_HYDRO` option defaults to `OFF` so the user will want to
make sure to enable it enable it with `-DMPAS_HYDRO=ON`.
```
$ mkdir build
$ cd build
$ cmake ../ \
    -DMPAS_HYDRO=ON \
    -DCMAKE_Fortran_COMPILER=gfortran \
    -DCMAKE_C_COMPILER=gcc \
    -DCMAKE_CXX_COMPILER=g++ \
    -DPnetCDF_MODULE_DIR=$NCAR_ROOT_PARALLEL_NETCDF/include
```


## NUOPC Cap Exchange
- [ ] Add list of variables being exchanged for two and one way coupling


## Graphs
- [ ] create graphs of procedures, variables exchanged, data structures, process workflow


## Directory Structure
```
mpas-model/
├──src/
│   ├──mpas_hydro/
│   │   ├──src/
│   │   └──tests/
│   ├──core_atmosphere/
│   ├──core_init_atmosphere/
│   ├──core_landice/
│   ├──core_ocean/
│   ├──core_seaice/
│   ├──core_sw/
│   ├──core_test/
│   ├──driver/
│   ├──external/
│   ├──framework/
│   ├──operators/
│   └──tools/
└──testing_and_setup/
```
