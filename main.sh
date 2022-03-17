#!/bin/bash

sbatch -p epimodel -c 32 -t 24:00:00 --mem=0 -J est --export=ALL,NETSIZE=102000 sh/runR.sh "01-estimation"

sbatch -p epimodel -c 32 -t 24:00:00 --mem=0 -J dx -d afterok:4695323 --export=ALL,NETSIZE=102000 sh/runR.sh "02-diagnostics"

sbatch -p preemptable -a 1-20 -c 32 -t 24:00:00 --mem=0 -J s1 --export=ALL,SIMNO=0001 sh/runR.sh "03-burnin1-simMin"

sbatch -p preemptable -a 1-20 -c 32 -t 24:00:00 --mem=0 -J s3100 --export=ALL,SIMNO=3100,NETSIZE=100 sh/runR.sh "03-burnin1-simMin"
