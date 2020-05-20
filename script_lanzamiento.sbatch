#!/bin/bash
#SBATCH -p normal               # partition (queue)
#SBATCH -N 1                    # number of nodes
#SBATCH -n 1                    # number of cores
#SBATCH -t 5-00:00               # time (D-HH:MM)
#SBATCH -o slurm.%N.%j.out      # STDOUT
#SBATCH -e slurm.%N.%j.err      # STDERR

cd DCL-DataAnalysis/Python\ codes/
module load python3.6/3.6.6

python main.py