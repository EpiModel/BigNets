source("R/utils-netsize.R")
source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")
source("R/z-utils_test_mat.R")

control <- control_msm(nsteps = 10 * 52, nsims = 1, ncores = 1, verbose = FALSE)
dat <- initialize_msm(est, param, init, control, s = 1)
at <- 2

# mod.condom ---------------------------------------------------------------------

orig_mod <- function(dat, at) {

  # condoms done, do next in line
  # (position)

  return(dat)
}

# repl  ------------------------------------------------------------------------
new_mod <- function(dat, at) {

  return(dat)
}





# bench ------------------------------------------------------------------------

set.seed(1)
dat1 <- acts_msm(dat, 2)
dat1 <- orig_mod(dat1, 2)
set.seed(1)
dat2 <- acts_msm_new(dat, 2)
dat2 <- new_mod(dat2, 2)

mean(dat1$temp$al[, "uai"])
mean(dat2$temp$al[["uai"]])

microbenchmark::microbenchmark(
  old = {
    dat1 <- acts_msm(dat, 2)
    dat1 <- orig_mod(dat1, 2)
  },
  new = {
    dat2 <- acts_msm_new(dat, 2)
    set.seed(1)
    dat2 <- new_mod(dat2, 2)
  }
)
