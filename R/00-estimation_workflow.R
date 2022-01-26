##
## 00. Network Model Estimation, HPC setup
##

# how to get hpc limits:
#   sinfo <partition> -O Partition,Memory
#   scontrol show config

# Setup ------------------------------------------------------------------------
library(slurmworkflow)

if (fs::dir_exists("workflows/estimation"))
  fs::dir_delete("workflows/estimation")

# setup_script contains the bash script used to load the R module on the HPC
setup_script <- "sh/loadR_mox.sh"
max_cores <- 28

# Workflow creation ------------------------------------------------------------
wf <- create_workflow(
  wf_name = "estimation",
  default_sbatch_opts = list(
    "account" = "csde",
    "partition" = "csde",
    "mail-type" = "FAIL"
  )
)

# Update RENV on the HPC -------------------------------------------------------
#
# this template will run:
#   git pull
#   renv::restore()
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_renv_restore(setup_script = setup_script),
  sbatch_opts = list(
    "partition" = "build",
    "mem" = "16G",
    "cpus-per-task" = 4,
    "time" = 120
  )
)

# Estimate the networks --------------------------------------------------------
#
# this template uses the syntax of `base::do.call`
# the arguments in `args` will be made available as variables to the script on
# the HPC
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/01-estimation.R",
    args = list(ncores = max_cores),
    setup_script = setup_script
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "24:00:00",
    "mem" = "0" # special: all mem on node
  )
)

# Generate the diagnostics data ------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/02-diagnostics.R",
    args = list(
      ncores = 15,
      nsims = 30,
      nsteps = 1e3
    ),
    setup_script = setup_script
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END" # to get a mail upon completion
  )
)

# to send the workflows on the HPC
# scp -r workflows/estimation <user>@mox.hyak.uw.edu:gscratch/BigNets/workflows/
# from the BigNets folder on Mox: workflows/estimation/start_workflow.sh

# to get the data back
# scp -r <user>@mox.hyak.uw.edu:gscratch/BigNets/data/input data/
