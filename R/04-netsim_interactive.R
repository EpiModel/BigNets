
##
## 04. Small-scale epidemic simulation for testing/debugging
##

# Required variables:
#   - NETSIZE

## Packages
suppressMessages(library("EpiModelHIV"))
suppressMessages(library("EpiModelHPC"))

NETSIZE <- 10000

## Parameters
epistats <- readRDS("data/input/epistats.rds")
netstats <- readRDS(paste0("data/input/netstats-", NETSIZE, ".rds"))
est <- readRDS(paste0("data/input/netest-", NETSIZE, ".rds"))

param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  a.rate = 0.00049,
  hiv.test.rate = c(0.00385, 0.00380, 0.00690),
  tx.init.prob = c(0.1775, 0.190, 0.2521),
  tx.halt.partial.prob = c(0.0062, 0.0055, 0.0031),
  tx.reinit.partial.prob = c(0.00255, 0.00255, 0.00255),
  trans.scale = c(2.44, 0.424, 0.270),
  riskh.start = 52,
  prep.start = 2,
  prep.start.prob = 0.66
)
init <- init_msm()
control <- control_msm(
  simno = 1,
  nsteps = 100,
  nsims = 1,
  ncores = 1,
  verbose = TRUE
)

sim <- netsim(est, param, init, control)
