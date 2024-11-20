#!/bin/bash -l
#SBATCH --job-name="compilation"
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=8
#SBATCH --mem 100000
#SBATCH --time=0:30:00
#SBATCH -A pl0415-01
#SBATCH -p fast
#SBATCH --output="build.log"
#SBATCH --error="build.log"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=m.m.modrzejewski@gmail.com

module load ifort
module load impi
module load mkl
./build.py -np 8 ifort-I64-AVX512

