##
## 11. Epidemic Model Parameter Calibration, Simulations
##

# Required variables:
#   - `scenario`
#   - `scenario_name`
#   - `ncores`

# Setup ------------------------------------------------------------------------
suppressMessages({
  library(EpiModelHIV)
  library(EpiModelHPC)
})

output_dir <- "data/output/calib"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

array_number <- Sys.getenv("SLURM_ARRAY_TASK_ID")

source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")

control <- control_msm(
  nsteps = nsteps,
  nsims = ncores,
  ncores = ncores,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = TRUE,
  verbose.int = 250,
  verbose.FUN = verbose.hpc.net,
  trackers.FUN = trackers.net,
  tracker.list = calibration_trackers
)

param <- update_params(param, scenario)

# Simulation -------------------------------------------------------------------
print(paste0("Starting simulation for scenario: ", scenario_name))
sim <- netsim(est, param, init, control)

## Save-Min
file_name <- paste0("simcalib__", scenario_name, "__", array_number, ".rds")
saveRDS(sim, paste0(output_dir, "/", file_name))

# I think we can also set simno = array_number and then use savesim (maybe)
