#!/bin/bash -l
#SBATCH --job-name="compilation"
## Number of nodes
#SBATCH -N 1
## Max number of cores
#SBATCH -n 24
#SBATCH --ntasks-per-node=24
## Max meomry per node (in MB)
#SBATCH --mem 100000
#SBATCH --time=1:00:00 
#SBATCH -A xccsr2
#SBATCH -p plgrid-testing
#SBATCH --output="build.log"
#SBATCH --error="build.log"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=m.m.modrzejewski@gmail.com

module load plgrid/tools/python
module load plgrid/libs/opencoarrays

#module load plgrid/tools/impi/2018
#module load plgrid/tools/intel/18.0.3
#module load plgrid/libs/mkl/2018.0.0
#./build.py -np 24 ifort-I64-AVX512
./build.py -np 8 gfortran-I64

