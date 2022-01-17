##
## 11. Epidemic Model Parameter Calibration, Simulations
##

# Required variables:
#   - `scenario`
#   - `scenario_name`
#   - `batch_num`
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

source("R/utils-netsim_inputs.R")
nsteps = 100
source("R/utils-targets.R")

control <- control_msm(
  nsteps = nsteps,
  nsims = ncores,
  ncores = ncores,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = TRUE,
  # verbose.int = 250,
  # verbose.FUN = verbose.hpc.net,
  # trackers.FUN = trackers.net,
  tracker.list = calibration_trackers
)

param <- update_params(param, scenario)

# Simulation -------------------------------------------------------------------
print(paste0("Starting simulation for scenario: ", scenario_name))
print(paste0("Batch number: ", batch_num))
sim <- netsim(est, param, init, control)

## Save-Min
file_name <- paste0("simcalib__", scenario_name, "__", batch_num, ".rds")
saveRDS(sim, paste0(output_dir, "/", file_name))

# I think we can also set simno = batch_num and then use savesim (maybe)
