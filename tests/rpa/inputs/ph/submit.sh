#!/bin/bash
#SBATCH --job-name="beyond-rpa-tests"
#SBATCH -A pl0415-03
#SBATCH -p altair 
#SBATCH --nodes=1      
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=48
#SBATCH --time=48:00:00
#SBATCH --mem=180gb

# MPI Environment setup
if [ -n "$I_MPI_PMI_LIBRARY" ]; then
    unset I_MPI_PMI_LIBRARY
fi
export I_MPI_HYDRA_BOOTSTRAP="ssh"
export I_MPI_OFI_PROVIDER="tcp"

# Load modules
module load python
module load ifort
module load impi
module load mkl

# Execute the runner script
python3 run_all.py
