#!/bin/bash -l
#SBATCH --job-name="compilation"
## Number of nodes
#SBATCH -N 1
## Max number of cores
#SBATCH -n 24
#SBATCH --ntasks-per-node=48
## Max meomry per node (in MB)
#SBATCH --mem 100000
#SBATCH --time=1:00:00 
#SBATCH -A plgrpa2023-cpu
#SBATCH -p plgrid-testing
#SBATCH --output="build.log"
#SBATCH --error="build.log"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=m.m.modrzejewski@gmail.com

module load python
module load intel/2021b
./build.py -np 8 ifort-I64-AVX512

