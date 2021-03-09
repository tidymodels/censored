calculate_basesurv <- function(time, event, lp, .time) {
  unique_event_times <- sort(unique(time[event == 1L]))
  alpha <- purrr::map_dbl(
    unique_event_times,
    ~ sum(time[event == 1L] == .x, na.rm = TRUE) /
      sum(exp(lp[time >= .x]), na.rm = TRUE)
  )
  obj <- approx(unique_event_times, cumsum(alpha),
                yleft = 0, xout = .time, rule = 2)
  obj$z <- exp(-obj$y)
  names(obj) <- c("times", "cumulative_base_hazard", "base_surv")
  obj
}

calculate_survival_prop <- function(lp, time, event, survtime) {
  lp <- as.numeric(lp)
  basesurv <- calculate_basesurv(time, event, lp, sort(survtime))
  exp(exp(lp) %*% (-t(basesurv$cumulative_base_hazard)))
}

calc_cph_km_table <- function(x, ...) {
  y <- survival::survfit(x, ...)
  # pad with a zero time point
  res <-
    tibble::tibble(
      .time = y$time,
      .pred_survival = y$surv,
      .pred_survival_lower = y$lower,
      .pred_survival_upper = y$upper,
      .pred_hazard_cumulative = y$cumhaz
    )
  if (any(names(y) == "strata")) {
    res$.strata <- rep(names(y$strata), y$strata)
  }
  res
}

interpolate_km_values <- function(x, .time, new_data) {

}


