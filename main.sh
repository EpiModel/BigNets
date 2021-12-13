#!/bin/bash

sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=est sh/runR.sh "01-estimation"

sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=dx --mail-type=ALL sh/runR.sh "02-diagnostics"

# sbatch -N 1 -n 1 --cpus-per-task=32 -p preemptable --time=24:00:00 --mem=180G --job-name=dx --dependency=afterok:3677314 rundx.sh

sbatch -p preemptable --array=1-20 --nodes=1 --cpus-per-task=32 --time=24:00:00 --mem=185G --job-name=s1 --mail-type=ALL --export=ALL,SIMNO=0001 sh/runR.sh "03-burnin1-simMin"

sbatch -p preemptable --array=1-2 --nodes=1 --ntasks-per-node=32 --time=02:00:00 --mem=180G --job-name=s2
                      --export=ALL,SIMNO=0002,NJOBS=2,NSIMS=60 sh/runsim.sh "03-burnin1-simMin"
