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
  nsteps = 1 * 52,
  nsims = 4, ncores = 4,
  cumulative.edgelist = TRUE,
  truncate.el.cuml = 0,
  verbose = FALSE,
  tracker.list = calibration_trackers,
  .checkpoint.dir = "data/cp_recal",
  .checkpoint.clear = TRUE,
  .checkpoint.steps = 0,
  raw.output = FALSE
)


n_batches <- 10
scenarios.df <- read.csv("data/input/calib_scenarios.csv")
scenarios.df[[".at"]][4] <- 10
scenarios.list <- EpiModel::create_scenario_list(scenarios.df)
scenarios.list <- rep(scenarios.list, n_batches)

# param <- use_scenario(param, scenarios.list[[4]])

# Simulation -------------------------------------------------------------------
sim <- netsim(est, param, init, control)

dat_list <- fs::dir_ls("data/cp_recal/cp_recal/batch_1")
dat_list <- lapply(dat_list, readRDS)

sim <- process_out.net(dat_list)

for (s in seq_along(dat_list)) {
  print(s)
  # Set output
  if (s == 1) {
    out <- saveout.net(dat_list[[s]], s)
  } else {
    out <- saveout.net(dat_list[[s]], s, out)
  }
}

s = 18
dat <- dat_list[[s]]

saveout.net <- function(dat, s, out = NULL) {

  # Counts number of simulated networks
  if (get_control(dat, "tergmLite") == TRUE) {
    num.nw <- length(dat$el)
  } else {
    num.nw <- length(dat$nw)
  }

  if (s == 1) {
    out <- list()
    out$param <- dat$param
    out$control <- dat$control
    out$nwparam <- dat$nwparam
    out$control$num.nw <- num.nw
    out[["last_timestep"]] <- get_current_timestep(dat)

    out$epi <- list()
    for (j in seq_along(dat$epi)) {
      out$epi[[names(dat$epi)[j]]] <- data.frame(dat$epi[j])
    }

    out$el.cuml <- list()
    out$el.cuml[[s]] <- dat$el.cuml

    out[["_last_unique_id"]] <- list()
    out[["_last_unique_id"]][[s]] <- dat[["_last_unique_id"]]

    out$attr.history <- list()
    out$attr.history[[s]] <- dat$attr.history

    out$raw.records <- list()
    out$raw.records[[s]] <- dat$raw.records

    out$stats <- list()
    if (dat$control$save.nwstats == TRUE) {
      out$stats$nwstats <- list(dat$stats$nwstats)
    }

    if (dat$control$save.transmat == TRUE) {
      if (!is.null(dat$stats$transmat)) {
        row.names(dat$stats$transmat) <- seq_len(nrow(dat$stats$transmat))
        out$stats$transmat <- list(dat$stats$transmat)
      } else {
        out$stats$transmat <- list(data.frame())
      }
      class(out$stats$transmat) <- c("transmat", class(out$stats$transmat))
    }

    if (dat$control$tergmLite == FALSE) {
      if (dat$control$save.network == TRUE) {
        out$network <- list(dat$nw)
      }
    }

    if (!is.null(dat$control$save.other)) {
      for (i in seq_along(dat$control$save.other)) {
        el.name <- dat$control$save.other[i]
        out[[el.name]] <- list(dat[[el.name]])
      }
    }

    if (dat$control$save.diss.stats == TRUE &&
        dat$control$save.network == TRUE &&
        dat$control$tergmLite == FALSE) {

      ## for each simulated network, if dissolution model is edges-only, compute diss stats
      out$diss.stats <- list(lapply(seq_len(num.nw), function(network) {
        if (dat$nwparam[[network]]$coef.diss$diss.model.type == "edgesonly") {
          toggles_to_diss_stats(tedgelist_to_toggles(as.data.frame(dat$nw[[network]])),
                                dat$nwparam[[network]]$coef.diss,
                                dat$control$nsteps,
                                dat$nw[[network]])
        } else {
          NULL
        }
      }))
    }
  }

  if (s > 1) {
    if (!is.null(dat$param$random.params.values)) {
      for (nms in names(dat$param$random.params.values)) {
        if (length(dat$param$random.params.values[[nms]]) > 1) {
          if (!is.list(out$param$random.params.values[[nms]])) {
            out$param$random.params.values[[nms]] <- list(
              out$param$random.params.values[[nms]]
            )
          }

          out$param$random.params.values[[nms]] <- c(
            out$param$random.params.values[[nms]],
            list(dat$param$random.params.values[[nms]])
          )

        } else {
          out$param$random.params.values[[nms]] <- c(
            out$param$random.params.values[[nms]],
            dat$param$random.params.values[[nms]]
          )
        }
      }
    }

    for (j in seq_along(dat$epi)) {
      out$epi[[names(dat$epi)[j]]][, s] <- data.frame(dat$epi[j])
    }

    out$el.cuml[[s]] <- dat$el.cuml

    out[["_last_unique_id"]][[s]] <- dat[["_last_unique_id"]]

    out$attr.history[[s]] <- dat$attr.history
    out$raw.records[[s]] <- dat$raw.records

    if (dat$control$save.nwstats == TRUE) {
      out$stats$nwstats[[s]] <- dat$stats$nwstats
    }

    if (dat$control$save.transmat == TRUE) {
      if (!is.null(dat$stats$transmat)) {
        row.names(dat$stats$transmat) <- seq_len(nrow(dat$stats$transmat))
        out$stats$transmat[[s]] <- dat$stats$transmat
      } else {
        out$stats$transmat[[s]] <- data.frame()
      }
    }

    if (dat$control$tergmLite == FALSE) {
      if (dat$control$save.network == TRUE) {
        out$network[[s]] <- dat$nw
      }
    }

    if (!is.null(dat$control$save.other)) {
      for (i in seq_along(dat$control$save.other)) {
        el.name <- dat$control$save.other[i]
        out[[el.name]][[s]] <- dat[[el.name]]
      }
    }

    if (dat$control$save.diss.stats == TRUE &&
        dat$control$save.network == TRUE &&
        dat$control$tergmLite == FALSE) {

      ## for each simulated network, if dissolution model is edges-only, compute diss stats
      out$diss.stats[[s]] <- lapply(seq_len(num.nw), function(network) {
        if (dat$nwparam[[network]]$coef.diss$diss.model.type == "edgesonly") {
          toggles_to_diss_stats(tedgelist_to_toggles(as.data.frame(dat$nw[[network]])),
                                dat$nwparam[[network]]$coef.diss,
                                dat$control$nsteps,
                                dat$nw[[network]])
        } else {
          NULL
        }
      })
    }

  }

  ## Final processing
  if (s == dat$control$nsims) {

    # Set names for out
    simnames <- paste0("sim", seq_len(dat$control$nsims))
    for (i in as.vector(which(lapply(out$epi, class) == "data.frame"))) {
      colnames(out$epi[[i]]) <- simnames
    }

    if (length(out$el.cuml) > 0)
      names(out$el.cuml) <- simnames

    if (length(out[["_last_unique_id"]]) > 0)
      names(out[["_last_unique_id"]]) <- simnames

    if (length(out$attr.history) > 0)
      names(out$attr.history) <- simnames

    if (length(out$.records) > 0)
    names(out$raw.records) <- simnames

    if (dat$control$save.nwstats == TRUE) {
      names(out$stats$nwstats) <- simnames
    }

    if (dat$control$save.transmat == TRUE) {
      names(out$stats$transmat) <- simnames
    }

    if (dat$control$tergmLite == FALSE) {
      if (dat$control$save.network == TRUE) {
        names(out$network) <- simnames
      }
    }

    if (dat$control$save.diss.stats == TRUE &&
        dat$control$save.network == TRUE &&
        dat$control$tergmLite == FALSE) {
      names(out$diss.stats) <- simnames
    }

    if (!is.null(dat$control$save.other)) {
      for (i in seq_along(dat$control$save.other)) {
        el.name <- dat$control$save.other[i]
        names(out[[el.name]]) <- simnames
      }
    }

    # Remove functions from control list
    ftodel <- grep(".FUN", names(out$control), value = TRUE)
    out$control[ftodel] <- NULL
    out$control$currsim <- NULL
    environment(out$control$nwstats.formula) <- NULL

    if (!("temp" %in% dat$control$save.other)) {
      out$temp <- NULL
    }

  }

  return(out)
}
