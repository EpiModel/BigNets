#!/bin/bash

#SBATCH -o ./out/%x_%a.out

source sh/loadR.sh
Rscript R/"$1".R
