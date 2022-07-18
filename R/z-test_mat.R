source("R/utils-netsize.R")
source("R/utils-netsim_inputs.R")
source("R/utils-targets.R")
source("R/z-utils_test_mat.R")

control <- control_msm(nsteps = 10 * 52, nsims = 1, ncores = 1, verbose = FALSE)
dat <- initialize_msm(est, param, init, control, s = 1)
at <- 2

dat2 <- acts_msm_new(dat, 2)
dat2 <- condoms_msm_new(dat2, 2)
dat2 <- position_msm_new(dat2, 2)
dat <- dat2

# repl  ------------------------------------------------------------------------
new_mod <- function(dat, at) {
  ## Input
  # Attributes
  vl        <- get_attr(dat, "vl")
  stage     <- get_attr(dat, "stage")
  circ      <- get_attr(dat, "circ")
  status    <- get_attr(dat, "status")
  prepStat  <- get_attr(dat, "prepStat")
  prepClass <- get_attr(dat, "prepClass")
  rGC       <- get_attr(dat, "rGC")
  uGC       <- get_attr(dat, "uGC")
  rCT       <- get_attr(dat, "rCT")
  uCT       <- get_attr(dat, "uCT")
  race      <- get_attr(dat, "race")
  tx.status <- get_attr(dat, "tx.status")

  # Parameters
  hiv.urai.prob      <- get_param(dat, "hiv.urai.prob")
  hiv.uiai.prob      <- get_param(dat, "hiv.uiai.prob")
  hiv.trans.scale    <- get_param(dat, "hiv.trans.scale")
  hiv.acute.rr       <- get_param(dat, "hiv.acute.rr")

  hiv.cond.eff.rr    <- get_param(dat, "hiv.cond.eff.rr")
  hiv.cond.fail.rr   <- get_param(dat, "hiv.cond.fail.rr")

  hiv.circ.rr        <- get_param(dat, "hiv.circ.rr")
  prep.adhr.rr       <- get_param(dat, "prep.adhr.rr")
  hiv.ugc.rr         <- get_param(dat, "hiv.ugc.rr")
  hiv.uct.rr         <- get_param(dat, "hiv.uct.rr")
  hiv.rgc.rr         <- get_param(dat, "hiv.rgc.rr")
  hiv.rct.rr         <- get_param(dat, "hiv.rct.rr")
  hiv.dual.rr        <- get_param(dat, "hiv.dual.rr")

  ## Process
  # Data
  al <- dat[["temp"]][["al"]]

  isDiscordant <- status[al[["p1"]]] == 1 & status[al[["p1"]]] == 0
  if (!any(isDiscordant)) {
    return(dat)
  }

  dal <- al[isDiscordant, ]

  ## Reorder by role: ins on the left, rec on the right

  disc.ip <- dal[dal[["ins"]] == 1, ]
  disc.rp <- dal[dal[["ins"]] == 0, c(2, 1, 3:ncol(dal)), drop = FALSE]
  colnames(disc.ip)[1:2] <- colnames(disc.rp)[1:2] <- c("ins", "rec")


  # PATP: Insertive Man Infected (Col 1) --------------------------------

  # Attributes of infected
  ip.vl <- vl[disc.ip[, 1]]
  ip.stage <- stage[disc.ip[, 1]]
  ip.txStat <- tx.status[disc.ip[, 1]]

  # Attributes of susceptible
  ip.prep <- prepStat[disc.ip[, 2]]
  ip.prepcl <- prepClass[disc.ip[, 2]]
  ip.rGC <- rGC[disc.ip[, 2]]
  ip.rCT <- rCT[disc.ip[, 2]]

  # Base TP from VL
  ip.tprob <- pmin(0.99, hiv.urai.prob * 2.45^(ip.vl - 4.5))

  # Adjustment (based on Supervie JAIDS) for VL Suppressed, on ART
  ip.noTrans <- ip.vl <= log10(200) & ip.txStat == 1
  ip.tprob[ip.noTrans] <- 2.2 / 1e5

  # Transform to log odds
  ip.tlo <- log(ip.tprob / (1 - ip.tprob))

  # Condom use
  not.UAI <- which(disc.ip[, "uai"] == 0)
  condom.rr <- rep(NA, nrow(disc.ip))
  races <- sort(unique(race[disc.ip[, 1]]))
  for (i in races) {
    not.UAI.race <- intersect(not.UAI, which(race[disc.ip[, 1]] == i))
    condom.rr[not.UAI.race] <- 1 - (hiv.cond.eff.rr - hiv.cond.fail.rr[i])
  }
  ip.tlo[not.UAI] <- ip.tlo[not.UAI] + log(condom.rr[not.UAI])

  # PrEP, by adherence class
  ip.on.prep <- which(ip.prep == 1)
  ip.tlo[ip.on.prep] <- ip.tlo[ip.on.prep] + log(prep.adhr.rr[ip.prepcl[ip.on.prep]])

  # Acute-stage multipliers
  isAcute <- ip.stage %in% 1:2
  ip.tlo[isAcute] <- ip.tlo[isAcute] + log(hiv.acute.rr)

  ## Multiplier for STI
  is.rGC <- which(ip.rGC == 1)
  is.rCT <- which(ip.rCT == 1)
  is.rect.dual <- intersect(is.rGC, is.rCT)
  is.rGC.sing <- setdiff(is.rGC, is.rect.dual)
  is.rCT.sing <- setdiff(is.rCT, is.rect.dual)
  ip.tlo[is.rGC.sing] <- ip.tlo[is.rGC.sing] + log(hiv.rgc.rr)
  ip.tlo[is.rCT.sing] <- ip.tlo[is.rCT.sing] + log(hiv.rct.rr)
  ip.tlo[is.rect.dual] <- ip.tlo[is.rect.dual] +
    max(log(hiv.rgc.rr), log(hiv.rct.rr)) +
    min(log(hiv.rgc.rr), log(hiv.rct.rr)) * hiv.dual.rr

  # Race-specific scalar for calibration
  races <- race[disc.ip[, 2]]
  ip.tlo <- ip.tlo + log(hiv.trans.scale[races])

  # Convert back to probability
  ip.tprob <- plogis(ip.tlo)
  stopifnot(ip.tprob >= 0, ip.tprob <= 1)


  # PATP: Receptive Man Infected (Col 2) --------------------------------

  # Attributes of infected
  rp.vl <- vl[disc.rp[, 2]]
  rp.stage <- stage[disc.rp[, 2]]
  rp.txStat <- tx.status[disc.rp[, 2]]

  # Attributes of susceptible
  rp.circ <- circ[disc.rp[, 1]]
  rp.prep <- prepStat[disc.rp[, 1]]
  rp.prepcl <- prepClass[disc.rp[, 1]]
  rp.uGC <- uGC[disc.rp[, 1]]
  rp.uCT <- uCT[disc.rp[, 1]]

  # Base TP from VL
  rp.tprob <- pmin(0.99, hiv.uiai.prob * 2.45^(rp.vl - 4.5))

  # Adjustment (based on Supervie JAIDS) for VL Suppressed, on ART
  rp.noTrans <- which(rp.vl <= log10(200) & rp.txStat == 1)
  rp.tprob[rp.noTrans] <- 2.2 / 1e5

  # Transform to log odds
  rp.tlo <- log(rp.tprob / (1 - rp.tprob))

  # Circumcision
  rp.tlo[rp.circ == 1] <- rp.tlo[rp.circ == 1] + log(hiv.circ.rr)

  # Condom use
  not.UAI <- which(disc.rp[, "uai"] == 0)
  condom.rr <- rep(NA, nrow(disc.rp))
  races <- sort(unique(race[disc.rp[, 1]]))
  for (i in races) {
    not.UAI.race <- intersect(not.UAI, which(race[disc.rp[, 1]] == i))
    condom.rr[not.UAI.race] <- 1 - (hiv.cond.eff.rr - hiv.cond.fail.rr[i])
  }
  rp.tlo[not.UAI] <- rp.tlo[not.UAI] + log(condom.rr[not.UAI])

  # PrEP, by adherence class
  rp.on.prep <- which(rp.prep == 1)
  rp.tlo[rp.on.prep] <- rp.tlo[rp.on.prep] + log(prep.adhr.rr[rp.prepcl[rp.on.prep]])

  # Acute-stage multipliers
  isAcute <- rp.stage %in% 1:2
  rp.tlo[isAcute] <- rp.tlo[isAcute] + log(hiv.acute.rr)

  ## Multiplier for STI
  is.uGC <- which(rp.uGC == 1)
  is.uCT <- which(rp.uCT == 1)
  is.ureth.dual <- intersect(is.uGC, is.uCT)
  is.uGC.sing <- setdiff(is.uGC, is.ureth.dual)
  is.uCT.sing <- setdiff(is.uCT, is.ureth.dual)
  rp.tlo[is.uGC.sing] <- rp.tlo[is.uGC.sing] + log(hiv.ugc.rr)
  rp.tlo[is.uCT.sing] <- rp.tlo[is.uCT.sing] + log(hiv.uct.rr)
  rp.tlo[is.ureth.dual] <- rp.tlo[is.ureth.dual] +
    max(log(hiv.ugc.rr), log(hiv.uct.rr)) +
    min(log(hiv.ugc.rr), log(hiv.uct.rr)) * hiv.dual.rr

  # Race-specific scalar for calibration
  races <- race[disc.rp[, 1]]
  rp.tlo <- rp.tlo + log(hiv.trans.scale[races])

  # Convert back to probability
  rp.tprob <- plogis(rp.tlo)
  stopifnot(rp.tprob >= 0, rp.tprob <= 1)


  # Transmission --------------------------------------------------------

  trans.ip <- runif(length(ip.tprob)) < ip.tprob
  trans.rp <- runif(length(rp.tprob)) < rp.tprob


  # Output --------------------------------------------------------------

  infected <- numeric()
  if (sum(trans.ip, trans.rp) > 0) {
    infected <- c(
      disc.ip[trans.ip == 1, 2],
      disc.rp[trans.rp == 1, 1]
    )

    # Attributes of newly infected
    dat <- set_attr(dat, "status", 1, posit_ids = infected)
    dat <- set_attr(dat, "inf.time", at, posit_ids = infected)
    dat <- set_attr(dat, "vl", 0, posit_ids = infected)
    dat <- set_attr(dat, "stage", 1, posit_ids = infected)
    dat <- set_attr(dat, "stage.time", 0, posit_ids = infected)
    dat <- set_attr(dat, "diag.status", 0, posit_ids = infected)
    dat <- set_attr(dat, "tx.status", 0, posit_ids = infected)
    dat <- set_attr(dat, "cuml.time.on.tx", 0, posit_ids = infected)
    dat <- set_attr(dat, "cuml.time.off.tx", 0, posit_ids = infected)

  }

  # Set Epi Trackers
  dat <- set_epi(dat, "incid", at,  length(infected))
  dat <- set_epi(dat, "incid.B", at,  sum(race[infected] == 1))
  dat <- set_epi(dat, "incid.H", at,  sum(race[infected] == 2))
  dat <- set_epi(dat, "incid.W", at,  sum(race[infected] == 3))

  return(dat)
}





# bench ------------------------------------------------------------------------

set.seed(1)
dat1 <- acts_msm(dat, 2)
dat1 <- condoms_msm(dat1, 2)
dat1 <- position_msm(dat1, 2)
dat1 <- hivtrans_msm(dat1, 2)
# dat <- dat1
set.seed(1)
dat2 <- acts_msm_new(dat, 2)
dat2 <- condoms_msm_new(dat2, 2)
dat2 <- position_msm_new(dat2, 2)
dat2 <- new_mod(dat2, 2)
# dat <- dat2

rr = 2952:2953
dat1$temp$al[rr, ]
as.matrix(dat2$temp$al)[rr, ]
sum(dat1$temp$al[, "uai" ] < as.matrix(dat2$temp$al)[, "uai"])
sum(dat1$temp$al[, "uai" ] > as.matrix(dat2$temp$al)[, "uai"])

microbenchmark::microbenchmark(
  old = {
    dat1 <- acts_msm(dat, 2)
    dat1 <- condoms_msm(dat1, 2)
    dat1 <- hivtrans_msm(dat1, 2)
  },
  new = {
    dat2 <- acts_msm_new(dat, 2)
    dat2 <- condoms_msm_new(dat2, 2)
    dat2 <- new_mod(dat2, 2)
  }
)

# microbenchmark::microbenchmark(
#   old = {
#   },
#   new = {
#   }
# )

as.vector(rowsum(
    dat[["temp"]][["al"]][, "uai"],
    dat[["temp"]][["al"]][, "pid"]
  ))
