# Land tenure regimes influenced long-term restoration successes and reversals across Brazil’s Atlantic Forest 

This is the code for our agglomerative matching technique, and for reproducing the results in the paper "Land tenure regimes influenced long-term restoration successes and reversals across Brazil’s Atlantic Forest", by Rayna Benzeev, Sam Zhang, Pedro Piffer, and Meg Mills-Novoa.

This repository is organized roughly according to [Principled Data Processing](https://www.youtube.com/watch?v=ZSunU9GQdcI) principles.

The tasks are:
1. `import`
2. `construct_superlands`*
3. `matching`*
4. `summarize`
5. `visualize`

Tasks with asterisks run on slurm.

## Setup

Install all of the packages in `requirements.txt`.

Run `make scaffold`, and place `all_lands.shp` in `import/input`. 

## Running

For each of these tasks, the instructions indicate how to run that given task. First, change into that task's folder.

### `import` task

Run `make`.

### `construct_superlands` task

To run on slurm, dispatch the slurm sbatch file `run_parallel.sbatch`.

To run locally without slurm, run `make`.

### `matching` task

Run on slurm using `run_parallel.sbatch`.

### `summarize` task

Run `python src/summarize_balance.py`.

### `visualize` task

Each of the three R scripts generate different visualizations for the paper. In addition, `results.R` prints out the estimates and errors that go into Tables S1 and S3.
