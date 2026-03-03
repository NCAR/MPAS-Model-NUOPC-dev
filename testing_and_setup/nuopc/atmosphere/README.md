# MPAS NUOPC Atm Test Script

## Overview

The `mpas_nuopc_atm_test.py` script automates the test process for the MPAS NUOPC Atmosphere component. It performs the following steps:

1. **Build MPAS** with NUOPC support
2. **Build ESMX** build esmx_mpas executable using the ESMX_Builder
3. **Run ESMX** atmonly usecase

## Prerequisites

Before running this script, ensure you have:

1. **ESMX_Builder** available in your PATH
   ```bash
   which ESMX_Builder
   ```
   If not found then add the ESMF installation directory to your PATH.

2. **Fortran compiler** (gnu, intel, or other supported compilers)
   - The script uses `gnu` by default via `--compiler gnu`
   - Override with `--compiler <target>` (example: `--compiler intel`)

3. **Build dependencies:**
   - Python
   - NetCDF
   - Parallel NetCDF (Pnetcdf)
   - Parallel I/O (PIO)
   - HDF5
   - ESMF/NUOPC libraries

## Usage

Navigate to the atmosphere directory and run the script:

```bash
cd testing_and_setup/nuopc/atmosphere
./mpas_nuopc_atm_test.py
```

By default, MPAS is built with compiler target `gnu` (equivalent to `--compiler gnu`) and uses `4` parallel build tasks (equivalent to `--build-tasks 4`).

To select a different compiler target for MPAS:

```bash
./mpas_nuopc_atm_test.py --compiler intel
```

To set a different number of MPAS build tasks:

```bash
./mpas_nuopc_atm_test.py --build-tasks 8
```

## Command-line options

- `--compiler <target>`: Compiler target passed to the MPAS `make` command (default: `gnu`).
- `--build-tasks <N>`: Number of parallel tasks used by MPAS `make -j` (default: `4`).

## Output

The script outputs the result of each step. Detailed output from each step can
be found in the `Log Directory`. Output from each ESMX run can be found in the
`Run Directory`.

```console
Building MPAS with NUOPC support...
✓ MPAS build succeeded: 75 second(s)

Building ESMX executable...
✓ ESMX build succeeded: 3 second(s)

Running ESMX atmonly executable...
✓ ESMX run succeeded: 1 second(s)
```

## ESMX Build Configuration

The ESMX build configuration can be found in the `esmx_build_atm.yml` file.

## Troubleshooting

### ESMX_Builder not found
- Ensure ESMF with ESMX is installed and the bin directory is in your PATH

### MPAS build fails
- Check MPAS build dependencies are installed
- Verify compiler (gfortran, ifort, etc.) is available
- Run `make clean` manually before trying again
- If using `--compiler`, ensure the selected target is supported by MPAS `make` (for example: `gnu`, `intel`)
- If using `--build-tasks`, reduce the value if you see resource/memory-related build failures.

### ESMX build fails
- Check that `esmx_build_atm.yml` configuration is correct
- Verify the MPAS build completed successfully and produced `libmpas_nuopc.a` and `mpas_nuopc_atm.mod`
- Check ESMF/NUOPC library is installed and `ESMFMKFILE` is set in the environment
