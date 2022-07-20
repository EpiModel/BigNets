##
## 09. Epidemic Model, Test file
##

# Setup ------------------------------------------------------------------------

# Load the `NETSIZE` value and the formatted `netsize_string`
# NETSIZE <- 1e4     # to override (before sourcing the file)
source("R/utils-netsize.R")
source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")
# renv::install("../EpiModel.git/dev")

ncores <- 4

library(future.apply)
plan(multisession, workers = ncores)

control <- control_msm(
  nsteps = 1 * 52,
  nsims = 1, ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  tracker.list = calibration_trackers,
  raw.output = FALSE
)


gc()
Sys.sleep(5)
print("start_sim")


# n_batches <- 10
# scenarios.df <- read.csv("data/input/calib_scenarios.csv")
# scenarios.df[[".at"]][4] <- 10
# scenarios.list <- EpiModel::create_scenario_list(scenarios.df)
# scenarios.list <- rep(scenarios.list, n_batches)
# param <- use_scenario(param, scenarios.list[[4]])

# Simulation -------------------------------------------------------------------

# start_t <- Sys.time()
# sim <- netsim(est, param, init, control)
# print(Sys.time() - start_t)

dat_list <- future_lapply(
  1:ncores,
  function(x) netsim(est, param, init, control),
  future.seed = TRUE
)

rm(dat_list); gc()
