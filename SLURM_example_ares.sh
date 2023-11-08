#!/bin/bash -l
#SBATCH --job-name="test"
## Number of nodes
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=48
## Max meomry per node (in MB)
#SBATCH --mem 120000
#SBATCH --time=1:00:00 
#SBATCH -A plgrpa2023-cpu
#SBATCH -p plgrid-testing
#SBATCH --output="example.log"
#SBATCH --error="example.log"
#SBATCH --mail-type=ALL
#SBATCH --mail-user=m.m.modrzejewski@gmail.com

module load python
module load intel/2021b

./bin/run -np $SLURM_NTASKS -nt $SLURM_CPUS_PER_TASK example.inp

