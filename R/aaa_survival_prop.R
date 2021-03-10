#' @export
cph_survival_prob <- function(x, new_data, .times, conf.int = .95, ...) {
  new_data$.id <- 1:nrow(new_data)
  y <- survival::survfit(x, newdata = new_data, id = .id,
                         conf.int = conf.int, na.action = na.exclude, ...)
  res <-
    stack_survfit_cph(y, nrow(new_data)) %>%
    dplyr::group_nest(.row, .key = ".pred") %>%
    mutate(
      .pred = purrr::map(.pred, ~ dplyr::bind_rows(prob_template, .x)),
      .pred = purrr::map(.pred, interpolate_km_values, .times)
    ) %>%
    dplyr::select(-.row)
  res
}

stack_survfit_cph <- function(x, n) {
  has_strata <- any(names(x) == "strata")
  if (has_strata) {
    # All components are vectors of length {t_i x n}
    res <- tibble::tibble(
      .time = x$time,
      .pred_survival = x$surv,
      .pred_survival_lower = x$lower,
      .pred_survival_upper = x$upper,
      .pred_hazard_cumulative = x$cumhaz,
      .row = rep(1:n, x$strata)
    )
  } else {
    # All components are {t x n} matrices
    times <- nrow(x$surv)
    res <- tibble::tibble(
      .time = rep(x$time, n),
      .pred_survival = as.vector(x$surv),
      .pred_survival_lower = as.vector(x$lower),
      .pred_survival_upper = as.vector(x$upper),
      .pred_hazard_cumulative = as.vector(x$cumhaz),
      .row = rep(1:n, each = times)
    )
  }

  res
}

prob_template <- tibble::tibble(
  .time = 0,
  .pred_survival = 1,
  .pred_survival_lower = NA_real_,
  .pred_survival_upper = NA_real_,
  .pred_hazard_cumulative = 0
)

# We want to maintain the step-function aspect of the predictions so, rather
# than use `approx()`, we cut the times and match the new times based on these
# intervals.
interpolate_km_values <- function(x, .times) {
  x <- km_with_cuts(x)
  pred_times <-
    tibble::tibble(.time = .times) %>%
    km_with_cuts(.times = x$.time) %>%
    dplyr::rename(.tmp = .time) %>%
    dplyr::left_join(x, by = ".cuts") %>%
    dplyr::select(-.time, .time = .tmp, -.cuts, -.pred_hazard_cumulative)
  pred_times
}

km_with_cuts <- function(x, .times = NULL) {
  if (is.null(.times)) {
    # When cutting the original data in the survfit object
    .times <- unique(x$.time)
  }
  .times <- c(-Inf, .times, Inf)
  .times <- unique(.times)
  x$.cuts <- cut(x$.time, .times)
  x
}

