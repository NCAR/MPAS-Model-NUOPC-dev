# MPAS Containers

This directory contains containerized environments for MPAS.

## Purpose

Containers provide reproducible, isolated environments with pre-installed compilers, libraries, and software. This eliminates the need to install compilers, MPI libraries, and NetCDF libraries on your local system.

## Available Containers

| Container | Location | Purpose |
|-----------|----------|---------|
| [Docker Development Container](#docker-development-container) | [docker/dev/](docker/dev) | Pre-built development environment |

---

### Docker Development Container

The Docker Development Container includes:
- **ubuntu:rolling**: base image
- **gcc,g++,gfortran**: compilers
- **spack**: package manager
- **mpich**: MPI implementation
- **netcdf-c**: NetCDF C library
- **netcdf-fortran**: NetCDF Fortran library
- **parallel-netcdf**: PnetCDF library
- **parallelio**: PIO library
- **esmf**: Earth System Modeling Framework
- preconfigured environment variables

See [README](docker/dev/README.md) for more details.
For more information about Docker, see the [Docker Documentation](https://docs.docker.com/).
