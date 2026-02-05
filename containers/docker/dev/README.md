# MPAS Development Environment

This container includes all necessary libraries to build and develop MPAS.
For more details on MPAS, see [MPAS documentation](https://www.mmm.ucar.edu/models/mpas).

## Available Libraries

The following packages are installed via `spack` and loaded in the environment:

- **mpich**: MPI implementation
- **netcdf-c**: NetCDF C library
- **netcdf-fortran**: NetCDF Fortran library
- **parallel-netcdf**: PnetCDF library
- **parallelio**: PIO library
- **esmf**: Earth System Modeling Framework

The following environment variables are available from a `bash -l` shell

| Environment Variable  | Description                           |
|-----------------------|---------------------------------------|
| `SPACK_ROOT`          | Path to spack installation            |
| `SPACK_ROOT`          | Path to spack installation            |
| `MPICH_ROOT`          | Path to mpich installation            |
| `NETCDF_C_ROOT`       | Path to netcdf-c installation         |
| `NETCDF_FORTRAN_ROOT` | Path to netcdf-fortran installation   |
| `PARALLEL_NETCDF_ROOT`| Path to parallel-netcdf installation  |
| `PARALLELIO_ROOT`     | Path to parallelio installation       |
| `ESMF_ROOT`           | Path to esmf installation             |
| `LD_LIBRARY_PATH`     | Includes paths to installed libraries |
| `PNETCDF`             | Set to PARALLEL_NETCDF_ROOT           |

## Quick Start

### Building the Docker image

From the `containers/docker/dev` directory:

```bash
docker build -t mpas-dev .
```

### Running the Docker container

Basic interactive shell:

```bash
docker run -it mpas-dev
```

Additional docker run arguments:

| Argument                               | Description                                      |
|----------------------------------------|--------------------------------------------------|
| `-v <local_path>:/home/mpas-dev/<dir>` | Mount a local directory inside the container     |
| `--rm`                                 | Automatically remove the container when it exits |
| `--name <my-mpas-container>`           | Assign a name to the container                   |
| `-e VAR=value`                         | Set runtime environment variable                 |

### Checkout MPAS

Clone the MPAS repository:

```bash
git clone https://github.com/MPAS-Dev/MPAS-Model.git
cd MPAS-Model
```

### Build MPAS

For the atmosphere core:

```bash
make -j4 gnu CORE="atmosphere"
```

For other cores, replace "atmosphere" with "init_atmosphere", "ocean", "landice", or "seaice".

## Troubleshooting

**Container exits immediately:**
- Ensure you're using `-it` flags for interactive mode
- Use `-l` with bash to force login shell and load environment

**Out of disk space:**
- Spack can use significant disk space. Check with `docker system df`
- Remove unused images/containers: `docker system prune`

**Can't find compiler/libraries:**
- Make sure you're in a bash login shell: `docker run -it mpas-dev bash -l`
- Manually source the spack setup: `source /etc/profile.d/spack.sh`
