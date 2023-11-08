import os
import stat

def make_runscript(RunscriptPath, BuildCmd, BuildDate, CAFLauncher="", IntelCAFConfig=""):
    #
    # The root of users' scratch directories
    # ---------------------------------------
    # Scratch files by default will be written to
    # /SCRATCHDIR_ROOT/user-name/a/job-identifier
    # The subdirectories are created by the launcher script at runtime.
    #
    scratchdir_root_default = "/scratch"
    #
    # The program will atempt to read the SCRATCH environment
    # variable. The default scratch location will be used if
    # SCRATCH is undefined.
    #
    # Default stack size in kibibytes. Too low stack size may lead
    # to segmentation faults. These segmentation faults are
    # particularly nasty, because their origin is not signalled by the
    # instrumented executable compiled with debugging flags.
    # When changing the default value, please run the executable with
    # multiple threads and using 64-bit default integer.
    #
    stacksize_threads = 8 * 8192
    unlimited_main_stack = True
    if CAFLauncher != "":
        command_def = """cmdlist = ["{}", "-np", nproc, exec_path] + exec_args""".format(CAFLauncher)
    else:
        command_def = """cmdlist = [exec_path] + exec_args"""

    if IntelCAFConfig != "":
        WriteIntelCAFConfig = """
IntelCAFConfigFile = path.join(path.dirname(path.realpath(args.inputfile)), os.path.splitext(os.path.basename(path.realpath(args.inputfile)))[0] + "-intel-coarray.conf")
environ["FOR_COARRAY_CONFIG_FILE"] = IntelCAFConfigFile
IntelCAFConfig = "{CAFConfigTemplate}".format(
      EXEC_PATH=exec_path,
      NIMAGES=args.np,
      ARGS=" ".join(exec_args))
open(IntelCAFConfigFile, "w").write(IntelCAFConfig)
        """.format(CAFConfigTemplate=IntelCAFConfig).strip()
    else:
        WriteIntelCAFConfig = ""
    
    s = """#!/usr/bin/env python3
#
# Generated automatically at {BUILDDATE}
#
import argparse
import os
import signal
import sys
import datetime
import subprocess
from os import environ
from os import path
import resource
import platform
import getpass
#
# Minimum stack size [kB] for the other threads
# LOWERING THIS VALUE MAY LEAD TO UNRELIABLE PROGRAM EXECUTION
#
STACKSIZE_THREADS = {STACKSIZE_THREADS}
UNLIMITED_MAIN_STACK = {UNLIMITED_MAIN_STACK}
parser = argparse.ArgumentParser()
parser.add_argument("inputfile", help="The path to an input file", type=str)
parser.add_argument("-np", help="Number of concurrent processes for distributed-momory parallelism", type=int, default=1)
parser.add_argument("-nt", help="Number of concurrent threads per process for shared-memory parallelism", type=int, default=6)
args = parser.parse_args()

def set_stacksize(stacksize_threads, unlimited_main_stack):
      soft_stack, hard_stack = resource.getrlimit(resource.RLIMIT_STACK)
      if unlimited_main_stack:
          #
          # Check if the stack limit is infinity. This can be represented as a negative number,
          # so comparisons won't work.
          #
          if hard_stack != resource.RLIM_INFINITY:
              print("% Error: Hard limit on the stack size isn't set to unlimited")
              print("% Please contact your system administrator and change the configuration in /etc/security/limits.conf")
              print("% Proceeding with too low amount of stack memory may lead to an unreliable program execution.")
              sys.exit(1)
          #
          # Stack size for the main thread
          #
          resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
      else:
          if hard_stack != resource.RLIM_INFINITY:
              if (hard_stack < stacksize_threads * 1024):
                  print("% Error: Hard limit on the stack size is less than {{s}} kB".format(s=stacksize_threads))
                  print("% Please contact your system administrator and change the configuration in /etc/security/limits.conf")
                  print("% Proceeding with too low amount of stack memory may lead to unreliable program execution.")
                  sys.exit(1)
          resource.setrlimit(resource.RLIMIT_STACK, (stacksize_threads * 1024, hard_stack))
      environ["OMP_STACKSIZE"] = str(stacksize_threads)
#
# Resolve all symbolic links to obtain this script's
# directory
#
script_dir = path.dirname(path.realpath(__file__))
root_dir = path.abspath(os.path.join(script_dir, os.pardir))
exec_path = path.join(script_dir, "a")

datetime_start = datetime.datetime.utcnow()
dateformat = "%a %b %d %H:%M:%S UTC %Y"
datestr = datetime_start.strftime(dateformat)

host = platform.node()
user = getpass.getuser()
launcher = "{LAUNCHER}"
#
# Process identifier of the launcher script
#
pid = os.getpid()
datestr_compressed = datetime_start.strftime("%Y%m%d%H%M%S")
jobname = "{{DATE}}_{{PID}}".format(DATE=datestr_compressed, PID=pid)
if "SCRATCH" in environ:
    scratch_dir = path.join(environ["SCRATCH"], "a", jobname) + os.sep
else:
    scratch_dir = path.join("{SCRATCHDIR_ROOT}", user, "a", jobname) + os.sep
if not path.exists(scratch_dir):
   os.makedirs(scratch_dir)

lib_dir = path.join(root_dir, "src", "lib")
if path.exists(lib_dir):
      if "LD_LIBRARY_PATH" in environ:
            environ["LD_LIBRARY_PATH"] = environ["LD_LIBRARY_PATH"] + os.pathsep + lib_dir
      else:
            environ["LD_LIBRARY_PATH"] = lib_dir

print("% Job started at {{s}}".format(s=datestr))
print("% User                     {{s}}".format(s=user))
print("% Node name                {{s}}".format(s=host))
print("% Executable               {{s}}".format(s=exec_path))
print("% Launcher                 {{s}}".format(s=launcher))
print("% Compiler command         {BuildCmd}")
print("% Source compiled at       {BUILDDATE}")
print("% Process ID               {{s}}".format(s=pid))
print("% Job identifier           {{s}}".format(s=jobname))
print("% Scratch directory        {{s}}".format(s=scratch_dir))
if UNLIMITED_MAIN_STACK:
    print("% Stack size (main thread) unlimited")
else:
    print("% Stack size (main thread) {{s}} kB".format(s=STACKSIZE_THREADS))
print("% Stack size (other)       {{s}} kB".format(s=STACKSIZE_THREADS))
#
# Make sure that the maximum stack size is in the reliable range.
#
set_stacksize(STACKSIZE_THREADS, UNLIMITED_MAIN_STACK)
sys.stdout.flush()

inputfile = path.realpath(args.inputfile)
nproc = str(args.np)
#
# The character used by the operating system to separate pathname components.
#
dirsep = os.sep 
#
# Arguments passed to the main executable
#
exec_args = [inputfile, scratch_dir, dirsep, jobname]
#
# Number of OpenMP threads
#
environ["OMP_NUM_THREADS"] = str(args.nt)
# --------------------------------------------------------
# Environment for the Intel Fortran compiler and libraries
# --------------------------------------------------------
#
# Number of Fortran images for the Intel shared-memory implementation
# of coarrays. On a distributed-memory system, an additional configuration
# file controls the number of images spawned by the launcher program.
#
environ["FOR_COARRAY_NUM_IMAGES"] = str(args.np)
#
# Number of threads used by MKL for matrix multiplications etc.
# Crucial for the thread-level parallelization of the RPA program.
#
environ["MKL_NUM_THREADS"] = str(args.nt)
#
# Disable the creation of memory buffers by the MKL memory allocator.
# Not an issue for the SCF part, but MKL's memory usage in the RPA
# program becomes really big for some reason, and even grows from iteration
# to iteration. Setting the following flag removes that issue.
#
environ["MKL_DISABLE_FAST_MM"] = "1"
{WRITE_INTEL_CAF_CONFIG}
#
# Execute the main Fortran program
#
{COMMAND_DEF}
os.setpgrp()
try:
    subprocess.run(cmdlist)
except KeyboardInterrupt:
    #
    # Ensure that all spawned processes are killed after the main program is interrupted.
    # I noticed a problem in Intel's co-array Fortran implementation, where the subprocesses
    # spawned by mpiexec.hydra keep using the computer's resources long after the main Python
    # script is killed.
    #
    os.killpg(0, signal.SIGKILL)
#
# Compute the duration time in hours
#
datetime_finish = datetime.datetime.utcnow()
td = datetime_finish - datetime_start
wallhours = ((td.microseconds + (td.seconds + td.days * 24 * 3600) * 10**6) / 10**6) / 3600.0
datestr_finish = datetime_finish.strftime(dateformat)
print("% Job finished at {{datestr}}".format(datestr=datestr_finish))
print("% Total wall clock time [hours]: {{WALLHOURS:.3f}}".format(WALLHOURS=wallhours))
sys.stdout.flush()
#
# Test if the scratch directory is empty. If yes, remove it.
#
if os.listdir(scratch_dir) == []:
    os.rmdir(scratch_dir)
    print("% Removed empty scratch directory")
else:
    print("% Nonempty scratch directory remains on disk")

""".format(SCRATCHDIR_ROOT=scratchdir_root_default,
           BUILDDATE=BuildDate, 
           STACKSIZE_THREADS=stacksize_threads,
           UNLIMITED_MAIN_STACK=unlimited_main_stack,
           COMMAND_DEF=command_def,
           BuildCmd=BuildCmd,
           LAUNCHER=CAFLauncher,
           WRITE_INTEL_CAF_CONFIG=WriteIntelCAFConfig)
    f = open(RunscriptPath, "w")
    f.write(s)
    f.close()
    #
    # Set executable flag
    #
    default_flags = os.stat(RunscriptPath).st_mode
    os.chmod(RunscriptPath, default_flags | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


