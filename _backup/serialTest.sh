#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --partition=compute
#SBATCH --time=00:15:00 # HH:MM:SS 
#SBATCH --mem-per-cpu=10GB
#SBATCH --account=Research-ABE-AET
module load 2022r2
module load r
module load python

Rscript spatialAnnealing_serial_hpc.R