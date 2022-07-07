acts_msm_new <- function(dat, at) {
  ## Inputs
  # Attributes
  active      <- get_attr(dat, "active")
  status      <- get_attr(dat, "status")
  diag.status <- get_attr(dat, "diag.status")
  race        <- get_attr(dat, "race")
  age         <- get_attr(dat, "age")
  stage       <- get_attr(dat, "stage")
  vl          <- get_attr(dat, "vl")
  uid         <- get_unique_ids(dat)

  # Parameters
  acts.aids.vl <- get_param(dat, "acts.aids.vl")
  acts.scale   <- get_param(dat, "acts.scale")
  netstats     <- get_param(dat, "netstats")
  epistats     <- get_param(dat, "epistats")
  time.unit    <- get_param(dat, "time.unit")

  geog.lvl     <- netstats[["geog.lvl"]]
  race.flag    <- netstats[["race"]]
  acts.mod     <- epistats[["acts.mod"]]

  el <- get_cumulative_edgelists_df(dat)
  el <- el[is.na(el[["stop"]]), ]

  el <- dplyr::tibble(
    p1 = get_posit_ids(dat, el[["head"]]),
    p2 = get_posit_ids(dat, el[["tail"]]),
    st1 = status[p1],
    st2 = status[p2],
    ptype = el[["network"]],
    duration.time = at - el[["start"]],
    race.combo = get_race_combo(race[p1], race[p2]),
    comb.age = age[p1] + age[p2],
    hiv.concord.pos = diag.status[p1] * diag.status[p2],
    geogYN = 1
  )

  el.mc <- el[el[["ptype"]] != 3, ]

  el.mc_mod <- el.mc

  rates <- unname(predict(acts.mod, newdata = el.mc_mod, type = "response"))
  rates <- rates * acts.scale/ (364 / time.unit)
  el.mc[["ai"]] <- rpois(length(rates), rates)

  # Add one-time partnerships
  el.oo <- el[el[, "ptype"] == 3, ]
  el.oo[["ai"]] <- 1

  # Bind el back together
  el <- dplyr::bind_rows(el.mc, el.oo)
  el_sav = el

  # For AIDS cases with VL above acts.aids.vl, reduce their their acts to 0

  aids_vl <- (stage[el[["p1"]]] == 4 & vl[el[["p1"]]] >= acts.aids.vl) |
             (stage[el[["p2"]]] == 4 & vl[el[["p2"]]] >= acts.aids.vl)
  # NA | TRUE and TRUE | NA return TRUE
  # NA | FALSE and FALSE | NA return NA
  aids_vl <- ifelse(is.na(aids_vl), FALSE, aids_vl)
  el[["ai"]][aids_vl] <- 0

  disc <- el[["st2"]] - el[["st1"]] == 1
  el[disc, 1:4] <- el[disc, c(2, 1, 4, 3)]

  # Remove inactive edges from el
  el <- el[el[["ai"]] > 0, ]

  # Save out
  dat[["temp"]][["el"]] <- el

  return(dat)
}

# race combo codes:
# 1: 1 and 1 , 2: 1 and !1
# 3: 2 and !2, 4: 2 and 2
# 5: 3 and !3, 6: 3 and 3
get_race_combo <- function(ra, rb) {
  race.combo <- ifelse(ra == rb, ra, ra + 3)
  race.combo <- c(1, 4, 6, 2, 3, 5)[race.combo]
  return(race.combo)
}

condoms_msm_new <- function(dat, at) {
  ## Input
  # Attributes
  race        <- get_attr(dat, "race")
  diag.status <- get_attr(dat, "diag.status")
  prepStat    <- get_attr(dat, "prepStat")
  age         <- get_attr(dat, "age")

  # Parameters
  netstats   <- get_param(dat, "netstats")
  epistats   <- get_param(dat, "epistats")
  cond.scale <- get_param(dat, "cond.scale")

  race.flag <- netstats[["race"]]
  geog.lvl  <- netstats[["race"]]

  # Condom Use Models
  cond.mc.mod <- epistats[["cond.mc.mod"]]
  cond.oo.mod <- epistats[["cond.oo.mod"]]

  ## Process

  # Temp edgelist
  el <- dat[["temp"]][["el"]]

  el[["prep"]] <- as.numeric(prepStat[el$p1] + prepStat[el$p2] > 0)
  el.mc <- el[el[["ptype"]] != 3, ]

  ## Main/casual partnerships ##

  el.mc[["cond.prob"]] <- unname(
    predict(cond.mc.mod, newdata = el.mc, type = "response")
  )

  ## One-off partnerships ##
  el.oo <- el[el[["ptype"]] == 3, ]
  el.oo[["cond.prob"]] <- unname(
    predict(cond.oo.mod, newdata = el.oo, type = "response")
  )

  ## Bind el together
  el <- dplyr::bind_rows(el.mc, el.oo)
  el$cond.prob <- el$cond.prob * cond.scale

  # Acts
  ai.vec <- el$ai
  pid <- rep(seq_along(ai.vec), ai.vec)
  p1 <- rep(el$p1, ai.vec)
  p2 <- rep(el$p2, ai.vec)
  ptype <- rep(el$ptype, ai.vec)
  cond.prob <- rep(el$cond.prob, ai.vec)
  # UAI draw per act
  uai <- as.numeric(runif(length(cond.prob)) < 1 - cond.prob)

  # Act list construction
  al <- dplyr::tibble(p1, p2, ptype, uai, pid)
  dat[["temp"]][["al"]] <- al

  ## Output

  return(dat)
}
