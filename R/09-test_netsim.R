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

ncores <- 3

control <- control_msm(
  nsteps = 2 * 52,
  nsims = ncores, ncores = ncores,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  tracker.list = calibration_trackers,
  save.nwstats = FALSE,
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

# populate `est`, `param` and `init`, R/01-estimation must be run once to create est, netest, etc
source("R/utils-netsize.R")
source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")

control <- control_msm(nsteps = 10 * 52, nsims = 1, ncores = 1, verbose = FALSE)

# creation of the data object
dat <- initialize_msm(est, param, init, control, s = 1)

# preparation before the `simulate` call in `simnet_msm`
nwparam <- EpiModel::get_nwparam(dat, network = 1)
nwL <- networkLite(dat[["el"]][[1]], dat[["attr"]])
dat <- set_attr(dat, "deg.casl", EpiModel::get_degree(dat$el[[2]]))
set.control.tergm <- get_control(dat, "set.control.tergm")
at <- 2

microbenchmark::microbenchmark( {
    simulate(
      nwL ~ Form(nwparam[["formation"]]) +
        Persist(nwparam[["coef.diss"]][["dissolution"]]),
      coef = c(nwparam[["coef.form"]], nwparam[["coef.diss"]][["coef.adj"]]),
      constraints = nwparam[["constraints"]],
      time.start = at - 1,
      time.slices = 1,
      time.offset = 1,
      control = set.control.tergm,
      output = "final",
      dynamic = TRUE
    )
  })

# 36.60988 38.63192 43.96643 41.18544 43.88365 209.2327   100
#
