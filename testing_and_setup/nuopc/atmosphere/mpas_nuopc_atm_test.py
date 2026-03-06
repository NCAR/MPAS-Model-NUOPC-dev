#!/usr/bin/env python3

"""
MPAS NUOPC Atmosphere ESMX Test Script
This script builds and tests MPAS Atmosphere with NUOPC support
Usage: ./mpas_nuopc_atm_test.py
"""

import os
import sys
import time
import shutil
import subprocess
from pathlib import Path
import argparse
import tarfile
import zipfile
import tempfile
import urllib.request
import urllib.error
import configparser
import json


class SpinnerProgress:
    """Display a spinner while waiting for a process to complete."""

    def __init__(self, pid, start_time):
        self.pid = pid
        self.start_time = start_time
        self.spinner_chars = ['/', '-', '\\', '|']

    def wait(self):
        """Wait for process with spinner, return (success, elapsed_time)."""
        try:
            current_time = int(time.time())
            spinner_idx = (current_time - int(self.start_time)) % 4
            print(f'{self.spinner_chars[spinner_idx]}', end='', flush=True)
            while True:
                current_time = int(time.time())
                spinner_idx = (current_time - int(self.start_time)) % 4
                print(f'\b{self.spinner_chars[spinner_idx]}', end='', flush=True)
                # Check if process is still running
                ret = self.pid.poll()
                if ret is not None:
                    # Process finished
                    elapsed = int(time.time() - self.start_time)
                    print(f'\b', end='', flush=True)
                    return {'success': ret == 0, 'elapsed': elapsed}
                time.sleep(1)
        except KeyboardInterrupt:
            self.pid.terminate()
            self.pid.wait()
            raise


def run_command(cmd, cwd, env, log_file):
    """Run a command with spinner progress indication."""

    start_time = time.time()
    with open(log_file, 'w') as log:
        process = subprocess.Popen(
            cmd,
            cwd=cwd,
            env=env,
            stdout=log,
            stderr=subprocess.STDOUT,
            shell=True
        )

    spinner = SpinnerProgress(process, start_time)
    return spinner.wait()


class MPASBuild:
    """Class to manage MPAS build execution."""

    def __init__(self, buildroot, compiler, bldtsks, logfile):
        self.buildroot = buildroot
        self.compiler = compiler
        self.bldtsks = bldtsks
        self.logfile = logfile

    def clean(self):
        """Clean MPAS build."""

        return run_command(
            f'make clean CORE="atmosphere" NUOPC="true"',
            self.buildroot,
            os.environ.copy(),
            self.logfile
        )

    def build(self):
        """Build MPAS Atmosphere with NUOPC support."""

        return run_command(
            f'make {self.compiler} -j {self.bldtsks} CORE="atmosphere" NUOPC="true"',
            self.buildroot,
            os.environ.copy(),
            self.logfile
        )


class ESMXBuild:
    """Class to manage ESMX build execution."""

    def __init__(self, buildroot, esmxcfg, esmfexe, logfile):
        self.buildroot = buildroot
        self.builddir = buildroot / "build"
        self.installdir = buildroot / "install"
        self.esmxcfg = esmxcfg
        self.esmfexe = buildroot / "install" / "bin" / esmfexe
        self.logfile = logfile

    def clean(self):
        """Clean ESMX build."""

        start_time = time.time()
        if self.builddir.exists():
            shutil.rmtree(self.builddir)
        if self.installdir.exists():
            shutil.rmtree(self.installdir)
        elapsed = int(time.time() - start_time)
        return {'success': True, 'elapsed': elapsed}

    def build(self):
        """Build ESMX executable using ESMX_Builder."""

        return run_command(
            f'ESMX_Builder {self.esmxcfg}',
            self.buildroot,
            os.environ.copy(),
            self.logfile
        )


class Input:
    """Class to represent an input file or directory."""
    valid_options = ['src', 'type', 'download', 'archive', 'strip']

    def __init__(self, input_spec: dict):

        for key in input_spec.keys():
            if not key in Input.valid_options:
                raise ValueError(f"Input includes unknown key '{key}'")

        if 'src' in input_spec:
            self.src = input_spec['src']
        else:
            raise ValueError(f"Input specification must include 'src' key")
        self.type = input_spec.get('type', 'copy')
        self.strip = input_spec.get('strip', 0)
        if 'download' in input_spec:
            self.download = bool(input_spec['download'])
        else:
            self.download = self._detect_download()
        if 'archive' in input_spec:
            self.archive = input_spec['archive']
        else:
            self.archive = self._detect_archive()

    @classmethod
    def typ_copy(cls, path):
        """Create Input from a Path object, defaulting to 'copy' type."""
        return cls({'src': path, 'type': 'copy'})

    @classmethod
    def typ_symlink(cls, path):
        """Create Input from a Path object, defaulting to 'symlink' type."""
        return cls({'src': path, 'type': 'symlink'})

    @classmethod
    def typ_extract(cls, path, strip=0):
        """Create Input for an extract type, with optional extract_path."""
        return cls({'src': path, 'type': 'extract', 'strip': strip})

    @classmethod
    def typ_download(cls, url, extract=False):
        """Download input from external source."""
        return cls({'src': url, 'type': 'download'})

    def _detect_download(self):
        """Detect if file needs to be downloaded"""

        src_str = str(self.src).lower()
        return (src_str.startswith('http://') or
                src_str.startswith('https://'))

    def _detect_archive(self):
        """Detect archive format from file extension."""

        src_str = str(self.src).lower()
        if src_str.endswith('.tar.gz') or src_str.endswith('.tgz'):
            return 'tar_gz'
        elif src_str.endswith('.tar.bz2') or src_str.endswith('.tbz2'):
            return 'tar_bz2'
        elif src_str.endswith('.tar'):
            return 'tar'
        elif src_str.endswith('.zip'):
            return 'zip'
        else:
            return None

    def _strip_members(self, archive):
        """Strip levels from archive"""

        try:
            level = int(self.strip)
        except ValueError as e:
            raise ValueError(f"Cannot strip '{self.strip}' levels")

        stripped_files = []
        for member in archive.getmembers():
            p = Path(member.path)
            if len(p.parts) > level:
                stripped_path = Path(*p.parts[level:])
                if not stripped_path.is_absolute() and not stripped_path.match('..*'):
                    member.path = str(stripped_path)
                    stripped_files.append(member)
        return stripped_files

    def _download_input(self, dest_path):
        """Download file or directory from source to destination."""

        if isinstance(self.src, Path):
            src_path = self.src.resolve()
        else:
            src_path = Path(self.src).resolve()

        try:
            urllib.request.urlretrieve(self.src, dest_path / src_path.name )
        except urllib.error.URLError as e:
            raise urllib.error.URLError(f"Failed to download {self.src}: {e}") from e

    def _copy_input(self, dest_path):
        """Copy file or directory from source to destination."""

        if self.download:
            self._download_input(dest_path)
        else:
            if isinstance(self.src,Path):
                src_path = self.src.resolve()
            else:
                src_path = Path(self.src).resolve()
            if not src_path.exists():
                raise FileNotFoundError(f"Input source does not exist: {src_path}")

            if src_path.is_file():
                shutil.copy2(src_path, dest_path)
            elif src_path.is_dir():
                if dest_path.exists():
                    shutil.rmtree(dest_path)
                shutil.copytree(src_path, dest_path)
            else:
                raise ValueError(f"Source is neither file nor directory: {src_path}")

    def _symlink_input(self, dest_path):
        """Create symbolic link from source to destination."""

        if self.download:
            raise ValueError(f"Cannot create symbolic link for downloaded input: {self.src}")

        if isinstance(self.src, Path):
            src_path = self.src.resolve()
        else:
            src_path = Path(self.src).resolve()

        if not src_path.exists():
            raise FileNotFoundError(f"Input source does not exist: {src_path}")

        if os.path.islink(dest_path / src_path.name):
            os.unlink(dest_path / src_path.name)
        os.symlink(src_path, dest_path / src_path.name)

    def _extract_input(self, dest_path):

        if self.download:
            try:
                with tempfile.NamedTemporaryFile(delete=False) as tmp:
                    archive_path = Path(tmp.name)
                    urllib.request.urlretrieve(self.src, archive_path)
            except urllib.error.URLError as e:
                raise urllib.error.URLError(f"Failed to download {self.src}: {e}") from e
        else:
            archive_path = self.src

        try:
            if self.archive == 'tar_gz':
                with tarfile.open(archive_path, 'r:gz') as tar:
                    members = self._strip_members(tar)
                    tar.extractall(path=dest_path, members=members)
            elif self.archive == 'tar_bz2':
                with tarfile.open(archive_path, 'r:bz2') as tar:
                    members = self._strip_members(tar)
                    tar.extractall(path=dest_path, members=members)
            elif self.archive == 'tar':
                with tarfile.open(archive_path, 'r') as tar:
                    members = self._strip_members(tar)
                    tar.extractall(path=dest_path, members=members)
            elif self.archive == 'zip':
                with zipfile.ZipFile(archive_path, 'r') as zf:
                    members = self._strip_members(zf)
                    zf.extractall(path=dest_path, members=members)
            else:
                raise ValueError(
                    f"Archive format is not supported: {self.src}. "
                    "Supported formats: .tar.gz, .tar.bz2, .tar, .zip"
                )

        finally:
            if self.download and archive_path.exists():
                archive_path.unlink()

    def setup(self, dest):
        """Set up the input according to its type."""

        if isinstance(dest, Path):
            dest_path = dest.resolve()
        else:
            dest_path = Path(dest).resolve()

        if self.type == 'copy':
            self._copy_input(dest_path)
        elif self.type == 'symlink':
            self._symlink_input(dest_path)
        elif self.type == 'extract':
            self._extract_input(dest_path)
        elif self.type == 'download':
            self._download_input(dest_path)
        else:
            raise ValueError(
                f"Unknown input type: {self.type}. "
                "Supported types: 'copy', 'symlink', 'extract', 'download'"
            )


class ESMXTest:
    """Class to manage ESMX run execution."""

    def __init__(self, name, rundir, esmxcfg, runinputs, mpitsks, logfile):
        self.name = name
        self.rundir = rundir
        self.esmxcfg = esmxcfg
        self.runinputs = runinputs
        self.mpitsks = int(mpitsks)
        self.logfile = logfile

    def setup(self):
        """Set up run for ESMX execution."""

        start_time = time.time()
        self.rundir.mkdir(parents=True, exist_ok=True)

        # Setup run inputs
        for runinput in self.runinputs:
            try:
                runinput.setup(self.rundir)
            except Exception as e:
                print(f"✗ Failed to {runinput.type} {runinput.src}: {e}")
                elapsed = int(time.time() - start_time)
                return {'success': False, 'elapsed': elapsed}
        elapsed = int(time.time() - start_time)
        return {'success': True, 'elapsed': elapsed}

    def clean(self):
        """Clean the run directory."""

        start_time = time.time()
        if self.rundir.exists():
            shutil.rmtree(self.rundir)
        elapsed = int(time.time() - start_time)
        return {'success': True, 'elapsed': elapsed}

    def run(self, esmx: ESMFBuild, faillvl=1):
        """Run the ESMX executable with MPI."""

        runenv = os.environ.copy()

        if faillvl > 1:
            runenv['ESMF_RUNTIME_ABORT_ACTION'] = "SIGABRT"
            runenv['ESMF_RUNTIME_ABORT_LOGMSG_TYPES'] = "ESMF_LOGMSG_ERROR"

        run_ret = run_command(
            f'mpirun -np {self.mpitsks} {esmx.esmfexe} {self.esmxcfg}',
            self.rundir,
            runenv,
            self.logfile
        )
        with open(self.logfile, 'a') as f:
           f.write(f'\nSee run directory: {self.rundir}')

        return run_ret


def load_tests_from_config(config):
    """Load test configurations from INI file.

    Args:
        config: Dict with 'test_config', 'rundir', and 'logdir' paths

    Returns:
        List of ESMXTest objects
    """

    start_time = time.time()

    tests = []
    if not Path(config['test_config']).exists():
        print(f"✗ Test configuration file missing: {config['test_config']}")
        elapsed = int(time.time() - start_time)
        return {'success': False, 'elapsed': elapsed}, tests

    parser = configparser.ConfigParser()
    parser.read(config['test_config'])

    # Iterate through all sections except DEFAULT
    for section in parser.sections():
        runinputs_list = []

        try:
            name = parser.get(section, 'name', fallback=section)
            rundir_name = parser.get(section, 'rundir', fallback=section)
            esmxcfg = parser.get(section, 'esmxcfg')
            mpitsks = parser.getint(section, 'mpitsks', fallback=4)
            runinputs_str = parser.get(section, 'runinputs', fallback='')
        except ValueError as e:
            print(f"✗ Invalid configuration value in '{section}': {e}")
            elapsed = int(time.time() - start_time)
            return {'success': False, 'elapsed': elapsed}, tests
        except configparser.NoOptionError as e:
            print(f"✗ Missing configuration option: {e}")
            elapsed = int(time.time() - start_time)
            return {'success': False, 'elapsed': elapsed}, tests

        # Parse runinputs as JSON list of dictionaries
        for line in runinputs_str.splitlines():
            if line.strip():
                try:
                    runinputs_list.append(Input(json.loads(line)))
                except json.JSONDecodeError as e:
                    print(f"✗ Failed to parse runinputs in {section}")
                    print(f"  {line}")
                    elapsed = int(time.time() - start_time)
                    return {'success': False, 'elapsed': elapsed}, tests
                except Exception as e:
                    print(f"✗ Failed to parse runinputs in {section}")
                    print(f"  {e}: {line}")
                    elapsed = int(time.time() - start_time)
                    return {'success': False, 'elapsed': elapsed}, tests

        # Create ESMXTest object
        test = ESMXTest(
            name=name,
            rundir=config['rundir'] / rundir_name,
            esmxcfg=esmxcfg,
            runinputs=runinputs_list,
            mpitsks=mpitsks,
            logfile=config['logdir'] / f"run_{name}.log"
        )
        tests.append(test)


    elapsed = int(time.time() - start_time)
    return {'success': True, 'elapsed': elapsed}, tests


def main():
    """Main execution function."""

    try:
        parser = argparse.ArgumentParser(
            description="Run MPAS NUOPC atmosphere test workflow")
        parser.add_argument("--compiler", "-c",
            type=str, default="gnu",
            help="Compiler target passed to MPAS make (default: gnu)",)
        parser.add_argument("--build-tasks", "-j",
            type=int, default=4,
            help="Number of build tasks for MPAS make (default: 4)",)
        parser.add_argument("--clean-first", "-k",
            action="store_true", default=False,
            help="Clean before executing tests (default: False)",)
        parser.add_argument("--clean-only", "-D",
            action="store_true", default=False,
            help="Clean without executing tests (default: False)",)
        parser.add_argument("--failure-level", "-L",
            type=int, default=1,
            help="Failure level for the tests, higher is stricter (default: 1)",)
        parser.add_argument("--test-config", "-t",
            type=str, default="all_tests.ini",
            help="Test configuration file (default: all_tests.ini)",)
        args = parser.parse_args()

        config = {}
        config['curdir'] = Path(__file__).parent.resolve()
        config['mpasdir'] = config['curdir'].resolve().parent.parent.parent
        config['logdir'] = config['curdir'] / "logs"
        config['rundir'] = config['curdir'] / "run"
        config['compiler'] = args.compiler
        config['bldtsks'] = args.build_tasks
        config['faillvl'] = args.failure_level
        config['test_config'] = Path(args.test_config).resolve()
        if args.clean_only:
            config['clean'] = True
            config['execute'] = False
        else:
            config['clean'] = args.clean_first
            config['execute'] = True

        print("=" * 80)
        print(f"MPAS NUOPC Atmosphere Tests (ESMX)")
        print(f"  Compiler:       {config['compiler']}")
        print(f"  Build Tasks:    {config['bldtsks']}")
        print(f"  Clean:          {config['clean']}")
        print(f"  Failure Level:  {config['faillvl']}")
        print(f"  MPAS Root:      {config['mpasdir']}")
        print(f"  Test Config:    {config['test_config']}")
        print(f"  Log Directory:  {config['logdir']}")
        print(f"  Run Directory:  {config['rundir']}")
        print("=" * 80)
        print()

        # setup log and run directories
        if config['logdir'].exists():
            shutil.rmtree(config['logdir'])
        config['logdir'].mkdir(parents=True, exist_ok=True)
        if config['rundir'].exists():
            shutil.rmtree(config['rundir'])
        config['rundir'].mkdir(parents=True, exist_ok=True)

        # load configuration
        print(f"Loading test configuration...", flush=True)
        config_load_ret, all_tests = load_tests_from_config(config)
        if config_load_ret.get('success', False):
            print(f"✓ Configuration load succeeded: " +
                  f"{len(all_tests)} test(s)")
        else:
            print(f"✗ Configuration load failed")
            sys.exit(1)

        # define MPAS build
        mpas_build = MPASBuild(
            buildroot=config['mpasdir'],
            compiler=config['compiler'],
            bldtsks=config['bldtsks'],
            logfile=config['logdir'] / "build_mpas.log")

        # run MPAS build
        print(f"\nBuilding MPAS with NUOPC support...", flush=True)
        if config['clean']:
            mpas_clean_ret = mpas_build.clean()
            if mpas_clean_ret.get('success', False):
                print(f"✓ MPAS build clean succeeded: " +
                      f"{mpas_clean_ret['elapsed']} second(s)")
            else:
                print(f"✗ MPAS build clean failed")
                print(f"  see log file: {mpas_build.logfile}")
                sys.exit(1)
        if config['execute']:
            mpas_build_ret = mpas_build.build()
            if mpas_build_ret.get('success', False):
                print(f"✓ MPAS build succeeded: " +
                      f"{mpas_build_ret['elapsed']} second(s)")
            else:
                print(f"✗ MPAS build failed")
                print(f"  see log file: {mpas_build.logfile}")
                sys.exit(1)

        # define ESMX build
        esmx_build = ESMXBuild(
            buildroot=config['curdir'],
            esmxcfg=config['curdir'] / "esmx_build_atm.yml",
            esmfexe="esmx_mpas",
            logfile=config['logdir'] / "build_esmx.log")

        # run ESMX build
        print(f"\nBuilding ESMX executable...", flush=True)
        if config['clean']:
            esmx_clean_ret = esmx_build.clean()
            if esmx_clean_ret.get('success', False):
                print(f"✓ ESMX build clean succeeded: " +
                      f"{esmx_clean_ret['elapsed']} second(s)")
            else:
                print(f"✗ ESMX build clean failed")
                sys.exit(1)
        if config['execute']:
            esmx_build_ret = esmx_build.build()
            if esmx_build_ret.get('success', False):
                print(f"✓ ESMX build succeeded: " +
                      f"{esmx_build_ret['elapsed']} second(s)")
            else:
                print(f"✗ ESMX build failed")
                print(f"  see log file: {esmx_build.logfile}")
                sys.exit(1)

        # run ESMX tests
        failure_count = 0
        for test in all_tests:
            print(f"\nRunning ESMX {test.name} test case...", flush=True)
            if config['clean']:
                test_clean_ret = test.clean()
                if test_clean_ret.get('success', False):
                    print(f"✓ {test.name} clean succeeded: " +
                            f"{test_clean_ret['elapsed']} second(s)")
                else:
                    print(f"✗ {test.name} clean failed")
                    failure_count += 1
                    continue
            if config['execute']:
                test_setup_ret = test.setup()
                if test_setup_ret.get('success', False):
                    print(f"✓ {test.name} setup succeeded: " +
                          f"{test_setup_ret['elapsed']} second(s)")
                else:
                    print(f"✗ {test.name} setup failed")
                    failure_count += 1
                    continue
                test_run_ret = test.run(esmx=esmx_build,
                                        faillvl=config['faillvl'])
                if test_run_ret.get('success', False):
                    print(f"✓ {test.name} run succeeded: " +
                          f"{test_run_ret['elapsed']} second(s)")
                else:
                    print(f"✗ {test.name} run failed")
                    print(f"  see log file: {test.logfile}")
                    print(f"  see run directory: {test.rundir}")
                    failure_count += 1
                    continue

        if failure_count > 0:
            print(f"\n{failure_count} test(s) failed. Please review logs and run directories for details.")
            sys.exit(1)

        print("\nAll tests completed successfully!")

    except KeyboardInterrupt:
        print("\n\nTest interrupted by user.")
        sys.exit(130)


if __name__ == "__main__":
    main()
