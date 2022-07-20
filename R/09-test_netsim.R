##
## 09. Epidemic Model, Test file
##

# Setup ------------------------------------------------------------------------

# Load the `NETSIZE` value and the formatted `netsize_string`
# NETSIZE <- 1e4     # to override (before sourcing the file)
source("R/utils-netsize.R")
source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")
# renv::install("../EpiModelHIV-p")

ncores <- 1

control <- control_msm(
  nsteps = 1 * 52,
  nsims = ncores, ncores = ncores,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  tracker.list = calibration_trackers,
  save.nwstats = TRUE,
  raw.output = FALSE
)


# n_batches <- 10
# scenarios.df <- read.csv("data/input/calib_scenarios.csv")
# scenarios.df[[".at"]][4] <- 10
# scenarios.list <- EpiModel::create_scenario_list(scenarios.df)
# scenarios.list <- rep(scenarios.list, n_batches)
# param <- use_scenario(param, scenarios.list[[4]])

# Simulation -------------------------------------------------------------------
sim <- netsim(est, param, init, control)

sim$stats$nwstats

