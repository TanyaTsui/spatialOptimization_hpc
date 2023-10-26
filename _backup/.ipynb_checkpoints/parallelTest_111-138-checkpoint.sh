#!/bin/bash
#SBATCH --ntasks=30
#SBATCH --cpus-per-task=1
#SBATCH --partition=compute
#SBATCH --time=50:00:00 # HH:MM:SS 
#SBATCH --mem-per-cpu=10GB
#SBATCH --account=Research-ABE-AET
module load 2022r2
module load r
module load python

Rscript spatialAnnealing_parallel_hpc_111-138.R