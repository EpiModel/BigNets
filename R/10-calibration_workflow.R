##
## 10. Epidemic Model Parameter Calibration, HPC setup
##

# Setup ------------------------------------------------------------------------
library(slurmworkflow)

if (fs::dir_exists("workflows/calibration"))
  fs::dir_delete("workflows/calibration")

setup_script <- "sh/loadR_klone.sh"
max_cores <- 40

# Workflow creation ------------------------------------------------------------
wf <- create_workflow(
  wf_name = "calibration",
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

# Run the simulations ----------------------------------------------------------
n_replications <- 10
scenarios.df <- read.csv("data/input/calib_scenarios.csv")
scenarios.list <- EpiModel:::make_scenarios_list(scenarios.df)
scenarios.list <- rep(scenarios.list, n_replications)

wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_map_script(
    r_script = "R/11-calibration_sim.R",
    scenario = scenarios.list,
    scenario_names = names(scenarios.list),
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

# Assess calibration -----------------------------------------------------------
wf <- add_workflow_step(
  wf_summary = wf,
  step_tmpl = step_tmpl_do_call_script(
    r_script = "R/12-calibration_assess.R",
    args = list(
      ncores = 15,
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
