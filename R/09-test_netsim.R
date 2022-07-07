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
  nsteps = 10 * 52,
  nsims = 1, ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  tracker.list = calibration_trackers,
  .checkpoint.dir = "data/cp_recal",
  .checkpoint.clear = FALSE,
  .checkpoint.steps = 52,
  raw.output = TRUE
)

n_batches <- 10
scenarios.df <- read.csv("data/input/calib_scenarios.csv")
scenarios.df[[".at"]][4] <- 10
scenarios.list <- EpiModel::create_scenario_list(scenarios.df)
scenarios.list <- rep(scenarios.list, n_batches)

# param <- use_scenario(param, scenarios.list[[4]])

# Simulation -------------------------------------------------------------------
sim <- netsim(est, param, init, control)

options(browser = "firefox")
dat <- sim[[1]]
dat$control$nsteps <- dat$control$nsteps + 1
at <- dat$control$nsteps
profvis::profvis({
  n = 0
  while (n < 100) {
    dat <- sim[[1]]
    # dat$control$nsteps <- dat$control$nsteps + 1
    at <- dat$control$nsteps

    dat <- aging_msm(dat, at)
    dat <- departure_msm(dat, at)
    dat <- arrival_msm(dat, at)
    dat <- partident_msm(dat, at)
    dat <- hivtest_msm(dat, at)
    dat <- hivtx_msm(dat, at)
    dat <- hivprogress_msm(dat, at)
    dat <- hivvl_msm(dat, at)
    # dat <- simnet_msm(dat, at)
    dat <- acts_msm(dat, at)
    dat <- condoms_msm(dat, at)
    dat <- position_msm(dat, at)
    dat <- prep_msm(dat, at)
    dat <- hivtrans_msm(dat, at)
    dat <- stitrans_msm(dat, at)
    dat <- stirecov_msm(dat, at)
    dat <- stitx_msm(dat, at)
    dat <- prevalence_msm(dat, at)
    dat <- trackers.net(dat, at)
    dat <- verbose.net(dat, at)

    n = n + 1
  }
})

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

# el_cuml ----------------------------------------------------------------------

el_cuml_list <- sim$el.cuml$sim1
el_cuml_df <- dplyr::bind_rows(el_cuml_list)
el_sizes <- vapply(el_cuml_list, nrow, numeric(1))
el_cuml_df[["network"]] <- rep(1:3, el_sizes)

el_cuml_df %>%
  group_by(network) %>%
  summarise(n = n())

concurent_el <- el_cuml_df %>%
  group_by(head, tail) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(el_cuml_df, by = c("head", "tail")) %>%
  mutate(stop = if_else(is.na(stop), Inf, stop)) %>%
  arrange(head, tail, start, stop) %>%
  group_by(head, tail) %>%
  mutate(
    concurent = start < lag(stop),
    concurent = if_else(is.na(concurent), lead(concurent), concurent)
  ) %>%
  filter(concurent)

nrow(concurent_el)
print(concurent_el, n = 2000)


# shared edges over time in the same network
el_cuml_df %>%
  ungroup() %>%
  group_by(network, head, tail) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(network) %>%
  summarize(repeated = sum(n > 1))

# repeated edges on the one-of network
el_cuml_df %>%
  filter(network == 3, stop - start > 0) %>%
  mutate(duration = stop - start + 1) %>%
  print(n = 200)

