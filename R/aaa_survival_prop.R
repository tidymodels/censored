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
    res$.strata <- rep(names(y$strata), y$strata)
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

  # TODO should we pad with Inf too?

  res <- dplyr::bind_rows(time_0, res)
  if (has_strata) {
    res <- dplyr::group_nest(res, .strata, .key = ".pred") %>%
      dplyr::mutate(.pred = purrr::map(.pred, km_with_cuts))
  } else {
    res <- km_with_cuts(res)
  }
  res
}

interpolate_km_values <- function(x, .time, new_data) {
  km <- x$.km_data
  has_strata <- any(grepl(".strata", names(km), fixed = TRUE))
  if (has_strata) {
    .pred <- dplyr::mutate(km, .pred = purrr::map(.pred, km_probs, .time))
    new_new_data <- compute_strata(x, new_data) %>% mutate(.row = dplyr::row_number())
    new_new_data <- new_new_data[, c(".strata", ".row")]
    res <- dplyr::left_join(new_new_data, .pred, by = ".strata")
    res <- res[order(res$.row),]
    res <- dplyr::select(res, .pred)
  } else {
    res <- tibble::tibble(.pred = list(km_probs(km, .time)))
  }
  res
}

km_with_cuts <- function(x, .times = NULL) {
  if (is.null(.times)) {
    .times <- unique(x$.time)
  }
  .times <- c(-Inf, .times, Inf)
  .times <- unique(.times)

  x$.cuts <- cut(x$.time, .times)
  x
}

km_probs <- function(x, .times) {
  all_times <- tibble(.time = .times)
  pred_times <- km_with_cuts(all_times, .times = unique(x$.time))
  pred_times <- dplyr::rename(pred_times, .tmp_time = .time)
  res <-
    dplyr::left_join(pred_times, x, by = ".cuts") %>%
    dplyr::select(-.cuts, -.time, -.pred_hazard_cumulative) %>%
    dplyr::rename(.time = .tmp_time)
  res
}

