#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=compute
#SBATCH --time=80:00:00 # HH:MM:SS 
#SBATCH --mem-per-cpu=1GB
#SBATCH --account=Research-ABE-AET
module load 2022r2
module load r
module load python

Rscript spatialAnnealing_81-100.R