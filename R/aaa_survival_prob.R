keep_cols <- function(x, output, keep_penalty = FALSE) {
  if (keep_penalty) {
    cols_to_keep <- c(".row", "penalty", ".time")
  } else {
    cols_to_keep <- c(".row", ".time")
  }
  output_cols <- switch(
    output,
    surv = ".pred_survival",
    conf = c(".pred_lower", ".pred_upper"),
    survconf =  c(".pred_survival", ".pred_lower", ".pred_upper"),
    haz = ".pred_hazard_cumulative"
  )
  cols_to_keep <- c(cols_to_keep, output_cols)
  dplyr::select(x, dplyr::all_of(cols_to_keep))
}

stack_survfit <- function(x, n, penalty = NULL) {
  # glmnet does not calculate confidence intervals
  if (is.null(x$lower)) x$lower <- NA_real_
  if (is.null(x$upper)) x$upper <- NA_real_

  has_strata <- any(names(x) == "strata")

  if (has_strata) {
    # All components are vectors of length {t_i x n}
    res <- tibble::tibble(
      penalty = penalty,
      .time = x$time,
      .pred_survival = x$surv,
      .pred_lower = x$lower,
      .pred_upper = x$upper,
      .pred_hazard_cumulative = x$cumhaz,
      .row = rep(seq_len(n), x$strata)
    )
  } else {
    # All components are {t x n} matrices (unless nrow(new_data) = 1)
    if (is.matrix(x$surv)) {
      times <- nrow(x$surv)
    } else {
      times <- 1
    }
    res <- tibble::tibble(
      penalty = penalty,
      .time = rep(x$time, n),
      .pred_survival = as.vector(x$surv),
      .pred_lower = as.vector(x$lower),
      .pred_upper = as.vector(x$upper),
      .pred_hazard_cumulative = as.vector(x$cumhaz),
      .row = rep(seq_len(n), each = times)
    )
  }

  res
}

prob_template <- tibble::tibble(
  .time = 0,
  .pred_survival = 1,
  .pred_lower = NA_real_,
  .pred_upper = NA_real_,
  .pred_hazard_cumulative = 0
)

interpolate_km_values <- function(x, times, group = NULL){

  if (is.null(group)) {
    return(interpolate_km_values_ungrouped(x, times))
  }

  group_tbl <- tibble::tibble(
    .row = seq_along(group),
    group = group
  )

  ret <- dplyr::left_join(x, group_tbl, by = ".row") %>%
    dplyr::group_nest(group, .key = ".pred") %>%
    dplyr::mutate(
      .pred = purrr::map(.pred, interpolate_km_values_ungrouped, times)
    ) %>%
    tidyr::unnest(cols = c(.pred)) %>%
    dplyr::select(-group)
}

# We want to maintain the step-function aspect of the predictions so, rather
# than use `approx()`, we cut the times and match the new times based on these
# intervals.
interpolate_km_values_ungrouped <- function(x, times) {
  x <- km_with_cuts(x)
  pred_times <-
    tibble::tibble(.time = times) %>%
    km_with_cuts(times = x$.time) %>%
    dplyr::rename(.tmp = .time) %>%
    dplyr::left_join(x, by = ".cuts") %>%
    dplyr::select(-.time, .time = .tmp, -.cuts)
  pred_times
}

km_with_cuts <- function(x, times = NULL) {
  if (is.null(times)) {
    # When cutting the original data in the survfit object
    times <- unique(x$.time)
  }
  times <- c(-Inf, times, Inf)
  times <- unique(times)
  x$.cuts <- cut(x$.time, times, right = FALSE)
  x
}

predict_survival_na <- function(time, interval = "none") {
  ret <- tibble(.time = time, .pred_survival = NA_real_)
  if (interval == "confidence"){
    ret <- ret %>%
      dplyr::mutate(.pred_lower = NA_real_, .pred_upper = NA_real_)
  }
  ret
}

pad_survival_na <- function(pred_to_pad,
                            missings_in_new_data,
                            times,
                            interval,
                            n_total) {
  pred_na <- predict_survival_na(times, interval)
  pred_full <- vector(mode = "list", length = n_total)
  pred_full[missings_in_new_data] <- list(pred_na)
  pred_full[-missings_in_new_data] <- pred_to_pad$.pred
  res <- tibble(.pred = pred_full)
  res
}


# -------------------------------------------------------------------------

# This function takes a matrix and turns it into list of nested tibbles
# suitable for predict_survival
matrix_to_nested_tibbles_survival <- function(x, time) {

  res <- tibble(
    .row = rep(seq_len(nrow(x)), each = ncol(x)),
    .time = rep(time, nrow(x)),
    .pred_survival = as.numeric(t(x))
  )

  dplyr::group_nest(res, .row, .key = ".pred")$.pred
}

