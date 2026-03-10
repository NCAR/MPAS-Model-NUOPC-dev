# MPAS Dev Container: ubuntu@25.10-gcc@14.3-mpich@4.3

This image provides a ready-to-build MPAS development environment with GCC 14.3, MPICH 4.3, and scientific libraries installed through Spack.

For general MPAS project information, see the [MPAS Documentation](https://www.mmm.ucar.edu/models/mpas).

## Installed Toolchain

The Dockerfile currently builds from `ubuntu:rolling` and installs:

- GCC, G++, and GFortran @14.3
- Spack @1.1
- MPICH @4.3
- NetCDF-C @4.9
- NetCDF-Fortran @4.6
- Parallel-NetCDF @1.14
- ParallelIO @2.6
- ESMF @8.9

## Environment

Start the container with `bash -l` to load the Spack-managed environment automatically.

| Environment Variable | Description |
|----------------------|-------------|
| `SPACK_ROOT` | Path to the Spack installation |
| `MPICH_ROOT` | Path to the MPICH installation |
| `NETCDF_C_ROOT` | Path to the NetCDF C installation |
| `NETCDF_FORTRAN_ROOT` | Path to the NetCDF Fortran installation |
| `PARALLEL_NETCDF_ROOT` | Path to the Parallel-NetCDF installation |
| `PARALLELIO_ROOT` | Path to the ParallelIO installation |
| `ESMF_ROOT` | Path to the ESMF installation |
| `LD_LIBRARY_PATH` | Library search path including installed dependencies |
| `PNETCDF` | Path to the Parallel-NetCDF installation |

## Docker Usage

### Build the image

From `.devcontainer/ubuntu@25.10-gcc@14.3-mpich@4.3/docker/`:

```bash
docker build -t mpasdev-ubuntu-25.10-gcc-14.3-mpich-4.3 .
```

### Run an interactive shell

```bash
docker run --rm -it mpasdev-ubuntu-25.10-gcc-14.3-mpich-4.3 bash -l
```

### Run an interactive shell with mounted local MPAS-Model folder

From the repository root:

```bash
docker run --rm -it \
	-v "$PWD:/home/mpas-dev/MPAS-Model" \
	-w /home/mpas-dev/MPAS-Model \
	mpasdev-ubuntu-25.10-gcc-14.3-mpich-4.3 \
	bash -l
```

## Build MPAS

With the repository mounted at `/home/mpas-dev/MPAS-Model`, a typical atmosphere build is:

```bash
make -j4 gnu CORE="atmosphere"
```

For other cores, replace `CORE` value with `init_atmosphere`, `ocean`, `landice`, or `seaice`.

## Troubleshooting

**Environment variables are missing**

Start a login shell with `bash -l` so `/etc/profile.d/mpas_spack.sh` is sourced.

**The container exits immediately**

Run an interactive shell, `-it`.

**Docker is using too much disk space**

Inspect usage with `docker system df` and remove unused images or containers with `docker system prune`.
