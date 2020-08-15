calculate_basesurv <- function (time, event, lp, times.eval) {
  t.unique <- sort(unique(time[event == 1L]))
  alpha <- map_dbl(
    t.unique,
    ~ sum(time[event == 1L] == .x) / sum(exp(lp[time >= .x]))
  )
  obj <- approx(t.unique, cumsum(alpha),
                yleft = 0, xout = times.eval, rule = 2)
  obj$z <- exp(-obj$y)
  names(obj) <- c("times", "cumulative_base_hazard", "base_surv")
  obj
}

calculate_survival_prop <- function (lp, time, event, survtime) {
  lp <- as.numeric(lp)
  basesurv <- calculate_basesurv(time, event, lp, sort(survtime))
  exp(exp(lp) %*% (-t(basesurv$cumulative_base_hazard)))
}
