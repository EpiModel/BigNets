##
## 10. Epidemic Model Parameter Calibration, HPC setup
##

# Setup ------------------------------------------------------------------------
library("slurmworkflow")
library("EpiModelHPC")

# hpc_configs <- swf_configs_hyak(hpc = "mox", partition = "csde")
hpc_configs <- swf_configs_rsph(partition = "preemptable")
max_cores <- 28

# Workflow creation ------------------------------------------------------------
wf <- create_workflow(
  wf_name = "calibration",
  default_sbatch_opts = hpc_configs$default_sbatch_opts
)

# Update RENV on the HPC -------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_renv_restore(
    git_branch = "swf_dev",
    setup_lines = hpc_configs$r_loader
  ),
  sbatch_opts = hpc_configs$renv_sbatch_opts
)

# Run the simulations ----------------------------------------------------------
n_batches <- 10
scenarios.df <- read.csv("data/input/calib_scenarios.csv")
scenarios.list <- EpiModel::create_scenario_list(scenarios.df)
scenarios.list <- rep(scenarios.list, n_batches)

# for this template, the syntax is similar to `base::Map` and `mapply`
# in this case, each instance will have a different value of
# - scenario, scenario_name and batch_num
# but they all share the same value for `ncores`

wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/11-calibration_sim.R",
    scenario = scenarios.list,
    batch_num = seq_along(scenarios.list),
    MoreArgs = list(
      ncores = max_cores
    ),
    max_array_size = 999,
    setup_lines = hpc_configs$r_loader
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
      nsteps = 52
    ),
    setup_lines = hpc_configs$r_loader
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
