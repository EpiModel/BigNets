#!/bin/bash

sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=est sh/runR.sh "01-estimation"

sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=dx --mail-type=ALL sh/runR.sh "02-diagnostics"

# sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=dx --dependency=afterok:3677314 rundx.sh

sbatch -p preemptable --array=1-20 --nodes=1 --cpus-per-task=32 --time=24:00:00 --mem=185G --job-name=s1 --mail-type=ALL --export=ALL,SIMNO=0001 sh/runR.sh "03-burnin1-simMin"

sbatch -p preemptable --array=1-5 --nodes=1 --ntasks-per-node=30 --time=24:00:00 --mem=180G --job-name=s1000 --mail-type=END,FAIL --mail-user=sjennes@emory.edu --export=ALL,SIMNO=1000 sh/runR.sh "03-burnin1-simMin"
