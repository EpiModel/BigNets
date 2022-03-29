
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

param <- param_msm(netstats = netstats,
                   epistats = epistats)
init <- init_msm()
control <- control_msm(simno = 1,
                       nsteps = 100,
                       nsims = 1,
                       ncores = 1,
                       verbose = TRUE)

sim <- netsim(est, param, init, control)

