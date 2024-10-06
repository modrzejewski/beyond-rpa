#!/bin/bash -l
#SBATCH --job-name="compilation"
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=48
#SBATCH --mem 100000
#SBATCH --time=1:00:00
#SBATCH -A plgrpa2025-cpu
#SBATCH -p plgrid-now
#SBATCH --output="build.log"
#SBATCH --error="build.log"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=m.m.modrzejewski@gmail.com

module load python
module load intel/2021b
./build.py -np 8 ifort-I64-AVX512

