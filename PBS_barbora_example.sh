#!/bin/bash -l
#PBS -N example
#PBS -l select=4:ncpus=36
#PBS -l walltime=00:30:00
#PBS -M m.m.modrzejewski@gmail.com
#PBS -q qexp
#PBS -A OPEN-24-32

cd $PBS_O_WORKDIR

module load Python
module load intel/2021b

/home/marcin/a/bin/run -np 4 -nt 36 /home/marcin/a/example.inp >& /home/marcin/a/example.log

