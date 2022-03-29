##
## utils. Default Inputs for EpiModel::nestim
##

# Setup ------------------------------------------------------------------------
suppressMessages({
  library("EpiModelHIV")
  library("EpiModelHPC")
})

epistats <- readRDS("data/input/epistats.rds")
netstats <- readRDS("data/input/netstats.rds")
est <- readRDS("data/input/netest.rds")

# Relevant times
calibration_length <- 52 * 10
prep_start         <- calibration_length + 52 * 5 + 1
interv_start       <- prep_start + 52 * 5
nsteps             <- interv_start + 52 * 10 - 1

# Parameters -------------------------------------------------------------------
param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  a.rate = 0.00049,
  hiv.test.rate = c(0.00385, 0.00380, 0.00690),
  tx.init.prob = c(0.1775, 0.190, 0.2521),
  tx.halt.partial.prob = c(0.0062, 0.0055, 0.0031),
  tx.reinit.partial.prob = c(0.00255, 0.00255, 0.00255),
  trans.scale = c(2.44, 0.424, 0.270),
  riskh.start = prep_start - 53,
  prep.start = prep_start,
  prep.start.prob = 0.66
)

# Initial conditions -----------------------------------------------------------
init <- init_msm()
