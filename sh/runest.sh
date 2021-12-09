#!/bin/bash

#SBATCH -o ./../out/%x_%a.out

source loadR.sh
Rscript ../R/01-estimation.R
