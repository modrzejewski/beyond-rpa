#!/bin/bash -l
#SBATCH --job-name="test"
## Number of nodes
#SBATCH --nodes=10
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=24
## Max meomry per node (in MB)
#SBATCH --mem 120000
#SBATCH --time=1:00:00 
#SBATCH -A plgrpa2023
#SBATCH -p plgrid-testing
#SBATCH --output="example.log"
#SBATCH --error="example.log"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=m.m.modrzejewski@gmail.com

module load plgrid/tools/python
#module load plgrid/tools/intel/19.1.2
#module load plgrid/tools/intel/2021.1.1
module load plgrid/tools/intel/2021.3.0

#module load plgrid/tools/impi/2018
#module load plgrid/tools/intel/18.0.3
#module load plgrid/libs/mkl/2018

./bin/run -np $SLURM_NTASKS -nt $SLURM_CPUS_PER_TASK example.inp

