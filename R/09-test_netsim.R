
##
## 09. Epidemic Model, Test file
##

# Setup ------------------------------------------------------------------------
source("R/utils-netsim_inputs.R")

control <- control_msm(
  nsteps = nsteps,
  nsims = 1,
  ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = TRUE
)
# Simulation -------------------------------------------------------------------
sim <- netsim(est, param, init, control)

# Exploration ------------------------------------------------------------------
d_sim <- as.data.frame(sim)

