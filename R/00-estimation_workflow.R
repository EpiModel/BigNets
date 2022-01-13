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

setup_script <- "sh/loadR_klone.sh"
max_cores <- 40

# Workflow creation ------------------------------------------------------------
wf <- create_workflow(
  wf_name = "estimation",
  default_sbatch_opts = list(
    "account" = "csde",
    "partition" = "compute",
    "mail-type" = "FAIL"
  )
)

# Update RENV on the HPC -------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_renv_restore(setup_script = setup_script),
  sbatch_opts = list(
    "mem" = "16G",
    "cpus-per-task" = 4,
    "time" = 120
  )
)

# Estimate the networks --------------------------------------------------------
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

# Generate the diagnostics -----------------------------------------------------
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
    "mem-per-cpu" = "4G"
  )
)
