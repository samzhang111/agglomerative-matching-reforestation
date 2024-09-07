#!/bin/bash

#SBATCH --job-name=matching
#SBATCH --output=scratch/slurm_out_%A_%a.out
#SBATCH --array=1-999
#SBATCH --time=01:00:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1

# Calculate the seed from the SLURM_ARRAY_TASK_ID
SEED=$(( SLURM_ARRAY_TASK_ID ))
INDEX_SIZE=10000

Rscript src/matching_superlands.R $SEED $INDEX_SIZE
