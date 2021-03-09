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
  has_strata <- any(names(y) == "strata")
  if (has_strata) {
    res$.strata <- factor(rep(names(y$strata), y$strata))
    time_0 <-
      dplyr::group_by(res, .strata) %>%
      dplyr::summarize(dplyr::across(where(is.numeric), mean), .groups= "drop") %>%
      dplyr::select(dplyr::all_of(names(res)))
  } else {
    time_0 <- res[1,]
  }
  time_0$.time <- 0
  time_0$.pred_survival <- 1
  time_0$.pred_hazard_cumulative <- 0
  time_0$.pred_survival_lower <- time_0$.pred_survival_upper <- NA_real_

  res <- dplyr::bind_rows(time_0, res)
  if (has_strata) {
    res <- dplyr::group_nest(res, .strata, .key = "curves") %>%
      dplyr::mutate(curves = purrr::map(curves, km_with_cuts))
  } else {
    res <- km_with_cuts(res)
  }
  res
}

interpolate_km_values <- function(x, .time, new_data) {
  km <- x$.km_data
  has_strata <- grepl(".strata", names(km), fixed = TRUE)
}

km_with_cuts <- function(x, .times = NULL) {
  if (is.null(.times)) {
    .times <- unique(x$.time)
    .times <- c(-Inf, .times, Inf)
  }
  x$.cuts <- cut(x$.time, .times)
  x
}



