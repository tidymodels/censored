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



# summary_survfit helpers -------------------------------------------------


survfit_summary_typestable <- function(object){
  # make matrix of dimension n_times x n_obs
  sanitize_element <- function(x) {
    if (!is.matrix(x)) {
      x <- matrix(x, ncol = length(object$n))
    }
    x
  }
  # sanitize elements we care about
  elements <- available_survfit_summary_elements(object)
  for (i in elements) {
    object[[i]] <- sanitize_element(object[[i]])
  }

  object
}

available_survfit_summary_elements <- function(object) {
  intersect(
    names(object),
    c("surv", "std.err", "lower", "upper", "cumhaz", "std.chaz")
  )
}

survfit_summary_patch_infinite_time <- function(object, time) {

  time_neg_inf <- is.infinite(time) & (time < 0)
  time_inf <- is.infinite(time) & (time > 0)

  patch_neg_inf <- function(x, value) {
    rbind(
      matrix(value, nrow = sum(time_neg_inf), ncol = ncol(x)),
      x
    )
  }
  patch_inf <- function(x, value) {
    rbind(
      x,
      matrix(value, nrow = sum(time_inf), ncol = ncol(x))
    )
  }

  # glmnet does not provide standard errors etc
  has_std_error <- "std.err" %in% names(object)

  if (any(time_neg_inf)) {
    object$surv <- patch_neg_inf(object$surv, value = 1)
    object$cumhaz <- patch_neg_inf(object$cumhaz, value = 0)
    if (has_std_error) {
      object$std.err <- patch_neg_inf(object$std.err, value = NA_real_)
      object$lower <- patch_neg_inf(object$lower, value = NA_real_)
      object$upper <- patch_neg_inf(object$upper, value = NA_real_)
      object$std.chaz <- patch_neg_inf(object$std.chaz, value = NA_real_)
    }
  }
  if (any(time_inf)) {
    object$surv <- patch_inf(object$surv, value = 0)
    object$cumhaz <- patch_inf(object$cumhaz, value = 1)
    if (has_std_error) {
      object$std.err <- patch_inf(object$std.err, value = NA_real_)
      object$lower <- patch_inf(object$lower, value = NA_real_)
      object$upper <- patch_inf(object$upper, value = NA_real_)
      object$std.chaz <- patch_inf(object$std.chaz, value = NA_real_)
    }
  }

  object
}

survfit_summary_restore_time_order <- function(object, time) {
  # preserve original order of `time` because `summary()` returns a result for
  # an ordered vector of finite time
  # Note that this requires a survfit summary object which has already been
  # patched for infinite time points
  original_order_time <- match(time, sort(time))

  elements <- available_survfit_summary_elements(object)

  # restore original order of prediction time points
  for (i in elements) {
    object[[i]] <- object[[i]][original_order_time, , drop = FALSE]
  }

  object
}

survfit_summary_patch_missings <- function(object, index_missing, time, n_obs) {
  if (is.null(index_missing)) {
    return(object)
  }

  patch_element <- function(x) {
    full_matrix <- matrix(NA, nrow = length(time), ncol = n_obs)
    full_matrix[, -index_missing] <- x
    full_matrix
  }

  elements <- available_survfit_summary_elements(object)

  for (i in elements) {
    object[[i]] <- patch_element(object[[i]])
  }

  object
}

survfit_summary_to_tibble <- function(object, time, n_obs) {
  ret <- tibble::tibble(
    .row = rep(seq_len(n_obs), each = length(time)),
    .time = rep(time, times = n_obs),
    .pred_survival = as.vector(object$surv),
    # TODO standard error
    .pred_lower = as.vector(object$lower),
    .pred_upper = as.vector(object$upper),
    .pred_hazard_cumulative = as.vector(object$cumhaz)
    # TODO standard error for cumulative hazard
  )
  ret
}

survfit_summary_to_patched_tibble <- function(object, index_missing, time, n_obs) {
  object %>%
    summary(times = time, extend = TRUE) %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_infinite_time(time = time) %>%
    survfit_summary_restore_time_order(time = time) %>%
    survfit_summary_patch_missings(
      index_missing = index_missing,
      time = time,
      n_obs = n_obs
    ) %>%
    survfit_summary_to_tibble(time = time, n_obs = n_obs)
}

