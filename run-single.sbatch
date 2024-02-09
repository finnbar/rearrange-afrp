#!/bin/bash
#
#SBATCH --job-name=ghc-uses-too-much-ram
#SBATCH --partition=cpu-mixed
#SBATCH --cpus-per-task=24
#SBATCH --mem=120000
#SBATCH --exclusive=mcs
#SBATCH --time=06:00:00
#SBATCH --output=run-single.out
#SBATCH --error=run-single.err

srun stack bench
