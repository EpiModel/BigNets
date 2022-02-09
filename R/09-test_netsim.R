##
## 09. Epidemic Model, Test file
##

# Setup ------------------------------------------------------------------------
source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")

control <- control_msm(
  nsteps = 50,
  nsims = 2,
  ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = TRUE,
  tracker.list = calibration_trackers
)

n_batches <- 10
scenarios.df <- read.csv("data/input/calib_scenarios.csv")
scenarios.list <- EpiModel::make_scenarios_list(scenarios.df)
scenarios.list <- rep(scenarios.list, n_batches)

param <- use_scenario(param, scenarios.list[[4]])

# Simulation -------------------------------------------------------------------
sim <- netsim(est, param, init, control)

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

