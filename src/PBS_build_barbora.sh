#!/bin/bash -l
#PBS -N compilation
#PBS -l select=1:ncpus=36
#PBS -l walltime=00:30:00
#PBS -M m.m.modrzejewski@gmail.com
#PBS -q qexp
#PBS -A OPEN-27-22

cd $PBS_O_WORKDIR

module load Python
module load intel/2021b

/home/marcin/a/src/build.py -np 8 ifort-I64-AVX512 >& /home/marcin/a/src/build.log

