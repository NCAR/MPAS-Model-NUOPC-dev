#!/bin/bash
. ${SPACK_ROOT}/share/spack/setup-env.sh
spack load mpich
spack load netcdf-c
spack load netcdf-fortran
spack load parallel-netcdf
spack load parallelio
spack load esmf
export MPICH_ROOT=$(spack location -i mpich)
export NETCDF_C_ROOT=$(spack location -i netcdf-c)
export NETCDF_FORTRAN_ROOT=$(spack location -i netcdf-fortran)
export PARALLEL_NETCDF_ROOT=$(spack location -i parallel-netcdf)
export PARALLELIO_ROOT=$(spack location -i parallelio)
export ESMF_ROOT=$(spack location -i esmf)
export LD_LIBRARY_PATH=${MPICH_ROOT}/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH=${NETCDF_C_ROOT}/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH=${NETCDF_FORTRAN_ROOT}/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH=${PARALLEL_NETCDF_ROOT}/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH=${PARALLELIO_ROOT}/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH=${ESMF_ROOT}/lib:${LD_LIBRARY_PATH}
export PNETCDF=${PARALLEL_NETCDF_ROOT}
