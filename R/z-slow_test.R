# test toy model ---------------------------------------------------------------
library(EpiModel)

n <- 1e5
nw <- network_initialize(n = n)
nw <- set_vertex_attribute(nw, "race", rbinom(n, 1, 0.5))
est <- netest(
  nw,
  formation = ~edges + nodematch("race"), target.stats = c(25, 10),
  coef.diss = dissolution_coefs(~offset(edges), 10, 0),
  verbose = FALSE
)

param <- param.net(inf.prob = 0.3, act.rate = 0.5)
init <- init.net(i.num = n / 5)
control <- control.net(
  type = "SI",
  nsims = 1, nsteps = 100,
  verbose = FALSE, tergmLite = TRUE
)

options(browser = "firefox")

profvis::profvis({
  mod <- netsim(est, param, init, control)
})

set.seed(1)
microbenchmark::microbenchmark(
  time_it = {
  status <- rbinom(n, 1, 0.3)
  nw <- activate.vertex.attribute( nw,
    prefix = "testatus", value = status,
    onset = 1,
    terminus = Inf
  )
})

# test EMHIV -------------------------------------------------------------------
source("R/utils-netsize.R")

source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")

control <- control_msm(
  nsteps = 10 * 52,
  nsims = 1, ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  tracker.list = calibration_trackers
)

dat <- initialize_msm(est, param, init, control, s = 1)
dat2 <- dat

options(browser = "firefox")
profvis::profvis({
  for (at in 2:104) {
    print(at)
    at = 2
    # dat <- aging_msm(dat, at)
    # dat <- departure_msm(dat, at)
    # dat <- arrival_msm(dat, at)
    # dat <- partident_msm(dat, at)
    # dat <- hivtest_msm(dat, at)
    # dat <- hivtx_msm(dat, at)
    # dat <- hivprogress_msm(dat, at)
    # dat <- hivvl_msm(dat, at)
    dat <- simnet_msm(dat, at)
    # dat <- acts_msm(dat, at)
    # dat <- condoms_msm(dat, at)
    # dat <- position_msm(dat, at)
    # dat <- prep_msm(dat, at)
    # dat <- hivtrans_msm(dat, at)
    # dat <- stitrans_msm(dat, at)
    # dat <- stirecov_msm(dat, at)
    # dat <- stitx_msm(dat, at)
    # dat <- prevalence_msm(dat, at)
    # dat <- trackers.net(dat, at)
  }
})

# test netsim_msm --------------------------------------------------------------
source("R/utils-netsize.R")

source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")

control <- control_msm(
  nsteps = 10 * 52,
  nsims = 1, ncores = 1,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  tracker.list = calibration_trackers
)

dat <- initialize_msm(est, param, init, control, s = 1)
dat2 <- dat

options(browser = "firefox")

edges_correct_msm <- function(dat, at) {

  old.num <- get_epi(dat, "num", at - 1)
  new.num <- sum(get_attr(dat, "active") == 1, na.rm = TRUE)
  adjust <- log(old.num) - log(new.num)

  coef.form.m <- get_nwparam(dat, network = 1)[["coef.form"]]
  coef.form.m[1] <- coef.form.m[1] + adjust
  dat[["nwparam"]][[1]][["coef.form"]] <- coef.form.m

  coef.form.p <- get_nwparam(dat, network = 2)[["coef.form"]]
  coef.form.p[1] <- coef.form.p[1] + adjust
  dat[["nwparam"]][[2]][["coef.form"]] <- coef.form.p

  coef.form.i <- get_nwparam(dat, network = 3)[["coef.form"]]
  coef.form.i[1] <- coef.form.i[1] + adjust
  dat[["nwparam"]][[3]][["coef.form"]] <- coef.form.i

  return(dat)
}

profvis::profvis({
  for (at in 2:104) {
    print(at)
    at = 2
    dat <- dat2

    ## Parameters
    cumulative.edgelist <- get_control(dat, "cumulative.edgelist")
    truncate.el.cuml <- get_control(dat, "truncate.el.cuml")
    set.control.tergm <- get_control(dat, "set.control.tergm")

    ## Edges correction
    dat <- edges_correct_msm(dat, at)

    ## Main network
    nwparam <- EpiModel::get_nwparam(dat, network = 1)

    dat <- set_attr(dat, "deg.casl", EpiModel::get_degree(dat$el[[2]]))

    nwL <- networkLite(dat[["el"]][[1]], dat[["attr"]])

    dat[["nw"]][[1]] <- simulate(
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

    dat[["el"]][[1]] <- as.edgelist(dat[["nw"]][[1]])

    ## Casual network
    nwparam <- EpiModel::get_nwparam(dat, network = 2)

    dat <- set_attr(dat, "deg.main", EpiModel::get_degree(dat$el[[1]]))

    nwL <- networkLite(dat[["el"]][[2]], dat[["attr"]])

    dat[["nw"]][[2]] <- simulate(
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

    dat[["el"]][[2]] <- as.edgelist(dat[["nw"]][[2]])

    ## One-off network
    nwparam <- EpiModel::get_nwparam(dat, network = 3)

    dat <- set_attr(dat, "deg.tot",
      pmin(get_attr(dat, "deg.main") + EpiModel::get_degree(dat[["el"]][[2]]), 3))

    nwL <- networkLite(dat[["el"]][[3]], dat[["attr"]])

    # browser()
    dat[["nw"]][[3]] <- simulate(
      basis = nwL,
      object = nwparam[["formation"]],
      coef = nwparam[["coef.form"]],
      constraints = nwparam[["constraints"]],
      control = set.control.tergm,
      time.start = at - 1,
      time.slices = 1,
      time.offset = 1,
      dynamic = TRUE,
      output = "final"
    )

    dat[["el"]][[3]] <- as.edgelist(dat[["nw"]][[3]])

    if (get_control(dat, "save.nwstats")) {
      if (isTERGM) {
        term.options <- set.control.tergm$term.options
      } else {
        term.options <- set.control.ergm$term.options
      }

      dat$stats$nwstats[[i]] <- rbind(
        dat$stats$nwstats[[i]],
        summary(
          dat$control$nwstats.formulas[[i]],
          basis = nwL,
          term.options = term.options,
          dynamic = isTERGM
        )
      )
    }

    if (cumulative.edgelist) {
      for (n_network in seq_len(3)) {
        dat <- update_cumulative_edgelist(dat, n_network, truncate.el.cuml)
      }
    }
  }
})

# test simulate ----------------------------------------------------------------
source("R/utils-netsize.R")
source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")

control <- control_msm(nsteps = 10 * 52, nsims = 1, ncores = 1, verbose = FALSE)
dat <- initialize_msm(est, param, init, control, s = 1)
dat2 <- dat

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
