#' A wrapper for survival probabilities with cph models
#' @param x A model from `coxph()`.
#' @param new_data Data for prediction
#' @param times A vector of integers for prediction times.
#' @param output One of "surv", "conf", or "haz".
#' @param conf.int The confidence level
#' @param ... Options to pass to [survival::survfit()]
#' @return A nested tibble
#' @keywords internal
#' @export
survival_prob_cph <- function(x, new_data, times, output = "surv", conf.int = .95, ...) {
  output <- match.arg(output, c("surv", "conf", "haz"))
  y <- survival::survfit(x, newdata = new_data, conf.int = conf.int,
                         na.action = na.exclude, ...)

  if (has_strata(x$terms)) {
    new_strata <- compute_strata(x, new_data) %>%
      dplyr::pull(.strata)
  } else {
    new_strata <- NULL
  }

  stacked_survfit <- stack_survfit(y, nrow(new_data))
  starting_rows <- stacked_survfit %>%
    dplyr::distinct(.row) %>%
    dplyr::bind_cols(prob_template)

  res <- dplyr::bind_rows(starting_rows, stacked_survfit) %>%
    interpolate_km_values(times, new_strata) %>%
    keep_cols(output) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)
}

keep_cols <- function(x, output, keep_penalty = FALSE) {
  if (keep_penalty) {
    cols_to_keep <- c(".row", "penalty", ".time")
  } else {
    cols_to_keep <- c(".row", ".time")
  }
  output_cols <- switch(output,
                        surv = ".pred_survival",
                        conf = c(".pred_survival_lower", ".pred_survival_upper"),
                        haz = ".pred_hazard_cumulative")
  cols_to_keep <- c(cols_to_keep, output_cols)
  dplyr::select(x, cols_to_keep)
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
      .pred_survival_lower = x$lower,
      .pred_survival_upper = x$upper,
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
      .pred_survival_lower = as.vector(x$lower),
      .pred_survival_upper = as.vector(x$upper),
      .pred_hazard_cumulative = as.vector(x$cumhaz),
      .row = rep(seq_len(n), each = times)
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

cph_survival_pre <- function(new_data, object) {

  # Check that the stratification variable is part of `new_data`.
  # If this information is missing, survival::survfit() does not error but
  # instead returns the survival curves for _all_ strata.
  terms_x <- stats::terms(object$fit)
  terms_special <- attr(terms_x, "specials")
  has_strata <- !is.null(terms_special$strata)

  if (has_strata) {
    strata <- attr(terms_x, "term.labels")
    strata <- grep(pattern = "^strata", x = strata, value = TRUE)
    strata <- sub(pattern = "strata\\(", replacement = "", x = strata)
    strata <- sub(pattern = "\\)", replacement = "", x = strata)

    if (!strata %in% names(new_data)) {
      rlang::abort("Please provide the strata variable in `new_data`.")
    }
  }

  new_data
}

#' A wrapper for survival probabilities with coxnet models
#' @param object A fitted `_coxnet` object.
#' @param new_data Data for prediction.
#' @param times A vector of integers for prediction times.
#' @param output One of "surv" or "haz".
#' @param ... Options to pass to [survival::survfit()].
#' @return A nested tibble.
#' @keywords internal
#' @export
survival_prob_coxnet <- function(object, new_data, times, output = "surv", penalty = NULL, ...) {

  output <- match.arg(output, c("surv", "haz"))
  multi <- length(penalty) > 1

  new_x <- parsnip::.convert_form_to_xy_new(
    object$preproc$coxnet,
    new_data,
    composition = "matrix")$x

  if (has_strata(object$formula)) {
    new_strata <- get_strata(object$formula, data = new_data)
  } else {
    new_strata <- NULL
  }

  y <- survival::survfit(
    object$fit,
    newx = new_x,
    newstrata = new_strata,
    s = penalty,
    x = object$training_data$x,
    y = object$training_data$y,
    na.action = na.exclude,
    ...
  )

  if (multi) {
    keep_penalty <- TRUE
    stacked_survfit <-
      purrr::map2_dfr(y, penalty,
                      ~stack_survfit(.x, n = nrow(new_data), penalty = .y))
    starting_rows <- stacked_survfit %>%
      dplyr::distinct(.row, penalty) %>%
      dplyr::bind_cols(prob_template)
  } else {
    keep_penalty <- FALSE
    stacked_survfit <-
      stack_survfit(y, nrow(new_data))
    starting_rows <- stacked_survfit %>%
      dplyr::distinct(.row) %>%
      dplyr::bind_cols(prob_template)
  }
  res <- dplyr::bind_rows(starting_rows, stacked_survfit) %>%
    interpolate_km_values(times, new_strata) %>%
    keep_cols(output, keep_penalty) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  res
}
