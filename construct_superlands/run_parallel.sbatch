#!/bin/bash

#SBATCH --job-name=superlands
#SBATCH --output=scratch/slurm_out_%A_%a.out
#SBATCH --array=1-499
#SBATCH --time=01:00:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1

# Calculate the seed from the SLURM_ARRAY_TASK_ID
SEED=$(( SLURM_ARRAY_TASK_ID ))

# Call make with the specific seed
make SEED=$SEED USE_WEIGHTS=0
