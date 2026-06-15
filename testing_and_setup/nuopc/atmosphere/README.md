# MPAS NUOPC Atm Test Script

## Overview

The `mpas_nuopc_atm_test.py` script automates the test workflow for the MPAS NUOPC Atmosphere component. It performs the following steps:

1. **Build MPAS** with NUOPC support
2. **Build ESMX** (`esmx_mpas`) using `ESMX_Builder`
3. **Run ESMX tests** defined in `all_tests.ini` (or a user-supplied config)

## Prerequisites

Before running this script, ensure you have:

1. **Fortran compiler** (gnu, intel, or other supported compilers)
   - Configure using `--compiler <target>` (default: `gnu`)

2. **MPI implementation**
   - The script executes tests via `mpirun`

3. **Build dependencies:**
   - Python
   - NetCDF
   - Parallel NetCDF (Pnetcdf)
   - HDF5
   - ESMF/NUOPC/ESMX libraries

   **Optional dependencies:**
   - Parallel I/O (PIO) - required for `--pio`

4. **Runtime dependencies:**
   - `nccmp` or `cdo` (recommended for NetCDF file comparison checks)

## Usage

Navigate to the testing directory and run the script:

```bash
cd testing_and_setup/nuopc/atmosphere
./mpas_nuopc_atm_test.py
```

The script supports the following options.
```bash
usage: mpas_nuopc_atm_test.py [-h] [--compiler COMPILER] [--build-tasks BUILD_TASKS] [--debug] [--clean-first] [--clean-only] [--failure-level FAILURE_LEVEL] [--no-checks] [--diff-nc {nccmp,cdo}] [--test-config TEST_CONFIG] [--pio]
```

### Command-line options

- `--compiler <target>`: Compiler target passed to the MPAS `make` command (default: `gnu`).
- `--build-tasks <N>`: Number of parallel tasks used by MPAS `make -j` (default: `4`).
- `--debug`: Enable debug mode for MPAS and ESMX builds (default: `False`).
- `--clean-first`: Clean MPAS/ESMX and per-test run directories before building/running.
- `--clean-only`: Clean MPAS/ESMX and per-test run directories, then exit without building/running.
- `--failure-level <N>`: Failure strictness for ESMF runtime handling (default: `1`).
- `--no-checks`: Disable post-run checks (default: checks enabled).
- `--diff-nc {nccmp,cdo}`: Specify which NetCDF diff utility to use for checks (auto-detected if omitted).
- `--test-config <file>`: Path to test INI configuration file (default: `all_tests.ini`).
- `--pio`: Include PIO in the build (requires `PIO` environment variable to be set).

## Test Configuration (`all_tests.ini`)

The test list is loaded from an INI file (`all_tests.ini` by default). Each section represents one ESMX test case.

### Supported section options

- `name`: Optional display name for the test. Defaults to the section name.
- `rundir`: Optional run directory name under `testing_and_setup/nuopc/atmosphere/run/`. Defaults to the section name.
- `esmxcfg`: Required ESMX runtime config file passed to `esmx_mpas`.
- `mpitsks`: Optional MPI task count for this test. If omitted, `DEFAULT` can provide it; otherwise the script falls back to `4`.
- `runinputs`: Optional multi-line list of configuration inputs, one entry per line.
- `runchecks`: Optional multi-line list of post-run checks, one entry per line.

#### `runinputs` format

Each line under `runinputs` must be formatted as follows:

```ini
runinputs =
   {"type": "copy", "src": "input/run_atm.yml"}
   {"type": "symlink", "src": "input/fd_mpas.yml"}
   {"type": "extract", "src": "https://example.com/case.tar.gz", "strip": 1}
```

Supported input types:

- `copy`: Copy local file/directory to the test run directory.
- `symlink`: Create a symbolic link in the test run directory.
- `extract`: Extract archive into the test run directory.
- `download`: Download file into the test run directory.

Supported keys per `runinputs` item:

- `src` (required): Local path or URL.
- `type` (optional): `copy`, `symlink`, `extract`, or `download` (default: `copy`).
- `strip` (optional): Number of leading path levels to strip during extraction.
- `download` (optional): Force download behavior (`true`/`false`). If omitted, URLs are auto-detected as downloads.
- `archive` (optional): Archive format override (`tar_gz`, `tar_bz2`, `tar`, `zip`). If omitted, format is inferred from file extension.

#### `runchecks` format

Each line under `runchecks` must be formatted as follows:

```ini
runchecks =
   {"type": "exists", "file1": "log.atmosphere.0000.out"}
   {"type": "diff", "file1": "baseline/namelist.atmosphere", "file2": "namelist.atmosphere"}
   {"type": "diff", "file1": "baseline/output.nc", "file2": "output.nc", "tolerance": 0.01}
```

Supported check types:

- `exists`: Verify a file exists in the run directory.
- `diff`: Compare two files (uses file extension to detect comparison type).
- `diff-cmp`: Binary file comparison.
- `diff-nc`: NetCDF file comparison (requires `nccmp` or `cdo`).

Supported keys per `runchecks` item:

- `file1` (required): Path to the file to check.
- `file2` (required for diff checks): Path to the baseline file for comparison.
- `type` (optional): `exists`, `diff`, `diff-cmp`, or `diff-nc` (default: `exists`).
- `tolerance` (optional): Tolerance for NetCDF numeric comparison (default: `0.0`).

## ESMX Build Configuration

The ESMX build configuration file is generated using a template, `templates/esmx_build.yml`. Build configuration options encased in `{{ option }}` are replaced when the test script executes.

## Output

The script outputs the result of each step. Detailed output from each step can
be found in the `Log Directory`. Output from each ESMX run can be found in the
`Run Directory`.

```console
================================================================================
MPAS NUOPC Atmosphere Tests (ESMX)
  Compiler:       gnu
  Build Tasks:    4
  Debug:          False
  Clean:          False
  Failure Level:  1
  Checks:         True
  Diff NC Util:   nccmp
  PIO:            False
  MPAS Root:      MPAS-Model
  Build Template: MPAS-Model/testing_and_setup/nuopc/atmosphere/templates/esmx_build.yml
  Test Config:    MPAS-Model/testing_and_setup/nuopc/atmosphere/all_tests.ini
  Log Directory:  MPAS-Model/testing_and_setup/nuopc/atmosphere/logs
  Run Directory:  MPAS-Model/testing_and_setup/nuopc/atmosphere/run
================================================================================

Loading test configuration...
✓ Configuration load succeeded: 2 test(s)

Building MPAS with NUOPC support...
✓ MPAS build succeeded: 77 second(s)

Building ESMX executable...
✓ ESMX build succeeded: 2 second(s)

Running ESMX atm_mountain_wave test case...
✓ atm_mountain_wave setup succeeded: 0 second(s)
✓ atm_mountain_wave run succeeded: 1 second(s)
✓ atm_mountain_wave checks succeeded: 2 second(s)

Running ESMX atmocn test case...
✓ atmocn setup succeeded: 0 second(s)
✓ atmocn run succeeded: 1 second(s)
✓ atmocn checks succeeded: 1 second(s)

All tests completed successfully!
```

## Troubleshooting

### Test configuration fails to load
- Ensure the file provided by `--test-config` exists.
- Ensure each test section includes `esmxcfg`.
- Ensure each `runinputs` entry is formatted correctly.
- Ensure each `runchecks` entry is formatted correctly.
- Check that `src` for `runinputs` exist.

### MPAS build fails
- Check MPAS build dependencies are installed.
- Verify compiler (gfortran, ifort, etc.) is available.
- Use `--clean-first` to clean an existing build.
- If using `--compiler`, ensure the selected target is supported by MPAS `make` (example: `gnu`, `intel`).
- If using `--build-tasks`, reduce the value if you see resource/memory-related failures.

### ESMX_Builder not found
- Check ESMF/NUOPC library is installed and `ESMFMKFILE` is set in the environment.
- Ensure ESMF with ESMX is installed and the bin directory is in your PATH.

### ESMX build fails
- Check that `templates/esmx_build.yml` exists and is valid.
- Verify the MPAS build completed successfully and produced `libmpas_nuopc.a` and `mpas_nuopc_atm.mod`.

### Test run fails
- Check run log for the specific test: `logs/run_<testname>.log`.
- Check files staged in the corresponding directory under `run/`.

### Checks fail or are skipped
- Checks require either `nccmp` or `cdo`. Install either utility or disable checks with `--no-checks`.
- Check run log for specific check failure details: `logs/run_<testname>.log`.
- Ensure baseline files (for diff checks) exist in the run directory.

### PIO build fails
- Ensure the `PIO` environment variable is set when using `--pio`.
- Check that PIO libraries are installed and the environment variable points to a valid installation.
