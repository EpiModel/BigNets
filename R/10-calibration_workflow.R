##
## 10. Epidemic Model Parameter Calibration, HPC setup
##

# Setup ------------------------------------------------------------------------
library(slurmworkflow)

if (fs::dir_exists("workflows/calibration"))
  fs::dir_delete("workflows/calibration")

setup_script <- "sh/loadR_mox.sh"
max_cores <- 28 # only 16 cores to avoid OOM errors

# Workflow creation ------------------------------------------------------------
wf <- create_workflow(
  wf_name = "calibration",
  default_sbatch_opts = list(
    "account" = "csde-ckpt",
    "partition" = "ckpt",
    "mail-type" = "FAIL"
  )
)

# Update RENV on the HPC -------------------------------------------------------
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

# Run the simulations ----------------------------------------------------------
n_replications <- 10
scenarios.df <- read.csv("data/input/calib_scenarios.csv")
scenarios.list <- EpiModel:::make_scenarios_list(scenarios.df)
scenarios.list <- rep(scenarios.list, n_replications)

# for this template, the syntax is similar to `base::Map` and `mapply`
# in this case, each instance will have a different value of
# - scenario, scenario_name and batch_num
# but they all share the same value for `ncores`

wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/11-calibration_sim.R",
    scenario = scenarios.list,
    scenario_name = names(scenarios.list),
    batch_num = seq_along(scenarios.list),
    MoreArgs = list(
      ncores = max_cores
    ),
    max_array_size = 999,
    setup_script = setup_script
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "24:00:00",
    "mem" = "0" # special: all mem on node
  )
)

# Process calibrations ---------------------------------------------------------
# produce a data frame with the calibration targets for each scenario
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/12-calibration_process.R",
    args = list(
      ncores = 15,
      nsteps = 1e3
    ),
    setup_script = setup_script
  ),
  sbatch_opts = list(
    "cpus-per-task" = max_cores,
    "time" = "04:00:00",
    "mem-per-cpu" = "4G",
    "mail-type" = "END"
  )
)
# to send the workflows on the HPC
# scp -r workflows klone.hyak.uw.edu:gscratch/BigNets/
# from the BigNets folder on Klone: workflows/calibration/start_workflow.sh

# to get the data back
# scp klone.hyak.uw.edu:gscratch/BigNets/data/output/calib/assessments.rds data/output/calib/
#
# here I only download the processed data. Not all the simulations
