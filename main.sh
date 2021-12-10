#!/bin/bash

sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=est sh/runR.sh "01-estimation"

sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=dx --mail-type=ALL sh/runR.sh "02-diagnostics"

# sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=dx --dependency=afterok:3677314 rundx.sh
