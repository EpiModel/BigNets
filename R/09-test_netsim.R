##
## 09. Epidemic Model, Test file
##

# Setup ------------------------------------------------------------------------

# Load the `NETSIZE` value and the formatted `netsize_string`
# NETSIZE <- 1e4     # to override (before sourcing the file)
source("R/utils-netsize.R")

source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")

control <- control_msm(
  nsteps = 20,
  nsims = 1,
  ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = TRUE,
  tracker.list = calibration_trackers
)

n_batches <- 10
scenarios.df <- read.csv("data/input/calib_scenarios.csv")
scenarios.df[[".at"]][4] <- 10
scenarios.list <- EpiModel::create_scenario_list(scenarios.df)
scenarios.list <- rep(scenarios.list, n_batches)

# param <- use_scenario(param, scenarios.list[[4]])

# Simulation -------------------------------------------------------------------
sim <- netsim(est, param, init, control)
sim$param

# Exploration ------------------------------------------------------------------
d_sim <- as.data.frame(sim)
d_sim <- mutate_targets(d_sim)

# Test calibration functions ---------------------------------------------------
file_name <- "data/output/simtest__test__10.rds"
saveRDS(sim, file_name)
rm(sim, d_sim)
gc()
d_p <- process_one_calibration(file_name, nsteps = 10)
glimpse(d_p)

