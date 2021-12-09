#!/bin/bash

sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=estimation runest.sh

sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=dx --dependency=afterok:3677314 rundx.sh
