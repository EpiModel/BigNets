#!/bin/bash

#SBATCH --mail-user=sjennes@emory.edu
#SBATCH -o ./out/%x_%a.out

source sh/loadR.sh
Rscript R/"$1".R
