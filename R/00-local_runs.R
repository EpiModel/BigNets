callr::r(
  function() {
    ncores <- 1
    source("R/01-estimation.R", local = TRUE)
  },
  show = TRUE
)

callr::r(
  function() {
    ncores <- 1
    nsims <- 10
    nsteps <- 100
    source("R/02-diagnostics.R", local = TRUE)
  },
  show = TRUE
)


