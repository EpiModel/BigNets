#!/bin/bash

#SBATCH --mail-user=$USER@emory.edu
#SBATCH -o ./out/%x_%a.out

source sh/loadR.sh
Rscript R/"$1".R
