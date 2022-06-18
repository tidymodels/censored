#' A wrapper for survival probabilities with coxph models
#' @param x A model from `coxph()`.
#' @param new_data Data for prediction
#' @param time A vector of integers for prediction times.
#' @param output One of `"surv"`, `"conf"`, or `"haz"`.
#' @param interval Add confidence interval for survival probability? Options
#' are `"none"` or `"confidence"`.
#' @param conf.int The confidence level.
#' @param ... Options to pass to [survival::survfit()]
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- coxph(Surv(time, status) ~ ., data = lung)
#' survival_prob_coxph(cox_mod, new_data = lung[1:3, ], time = 300)
survival_prob_coxph <- function(x,
                                new_data,
                                time,
                                output = "surv",
                                interval = "none",
                                conf.int = .95,
                                ...) {
  interval <- rlang::arg_match(interval, c("none", "confidence"))
  output <- rlang::arg_match(output, c("surv", "conf", "haz"))
  if (output == "surv" & interval == "confidence") {
    output <- "survconf"
  }

  missings_in_new_data <- get_missings_coxph(x, new_data)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- predict_survival_na(time, interval)
      ret <- tibble(.pred = rep(list(ret), n_missing))
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, , drop = FALSE]
  }

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
    interpolate_km_values(time, new_strata) %>%
    keep_cols(output) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  if (!is.null(missings_in_new_data)) {
    res <- pad_survival_na(res, missings_in_new_data, time,
                           interval, n_total)
  }
  res
}

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

    if (!all(strata %in% names(new_data))) {
      rlang::abort("Please provide the strata variable(s) in `new_data`.")
    }
  }

  new_data
}

# see https://github.com/therneau/survival/issues/137
get_missings_coxph <- function(object, new_data) {
  trms <- stats::terms(object)
  trms <- stats::delete.response(trms)
  xlevels <- object$xlevels
  mod_frame <- stats::model.frame(trms, data = new_data, xlev = xlevels,
                                  na.action = stats::na.exclude)

  attr(mod_frame, "na.action")
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


#' A wrapper for survival times with `coxph` models
#' @param object A model from `coxph()`.
#' @param new_data Data for prediction
#' @return A vector.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- coxph(Surv(time, status) ~ ., data = lung)
#' survival_time_coxph(cox_mod, new_data = lung[1:3, ])
survival_time_coxph <- function(object, new_data) {

  missings_in_new_data <- get_missings_coxph(object, new_data)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- rep(NA, n_missing)
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, ]
  }


  y <- survival::survfit(object, new_data, na.action = stats::na.exclude)

  tabs <- summary(y)$table
  if (is.matrix(tabs)) {
    colnames(tabs) <- gsub("[[:punct:]]", "", colnames(tabs))
    res <- unname(tabs[, "rmean"])
  } else {
    names(tabs) <- gsub("[[:punct:]]", "", names(tabs))
    res <- unname(tabs["rmean"])
  }
  if (!is.null(missings_in_new_data)) {
    index_with_na <- rep(NA, n_total)
    index_with_na[-missings_in_new_data] <- seq_along(res)
    res <- res[index_with_na]
  }
  res
}

#' A wrapper for survival probabilities with coxnet models
#' @param object A fitted `_coxnet` object.
#' @param new_data Data for prediction.
#' @param time A vector of integers for prediction times.
#' @param output One of "surv" or "haz".
#' @param penalty Penalty value(s).
#' @param ... Options to pass to [survival::survfit()].
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- proportional_hazards(penalty = 0.1) %>%
#'   set_engine("glmnet") %>%
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_prob_coxnet(cox_mod, new_data = lung[1:3, ], time = 300)
survival_prob_coxnet <- function(object, new_data, time, output = "surv", penalty = NULL, ...) {

  output <- match.arg(output, c("surv", "haz"))
  multi <- length(penalty) > 1

  new_x <- parsnip::.convert_form_to_xy_new(
    object$preproc$coxnet,
    new_data,
    composition = "matrix")$x

  if (has_strata(object$formula, object$training_data)) {
    new_strata <- get_strata_glmnet(object$formula, data = new_data,
                                    na.action = stats::na.pass)
  } else {
    new_strata <- NULL
  }

  missings_in_new_data <- get_missings_coxnet(new_x, new_strata)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- predict_survival_na(time, interval = "none")
      ret <- tibble(.pred = rep(list(ret), n_missing))
      return(ret)
    }
    new_x <- new_x[-missings_in_new_data, , drop = FALSE]
    new_strata <- new_strata[-missings_in_new_data]
  }

  y <- survival::survfit(
    object$fit,
    newx = new_x,
    newstrata = new_strata,
    s = penalty,
    x = object$training_data$x,
    y = object$training_data$y,
    weights = object$preproc$coxnet$weights,
    na.action = na.exclude,
    ...
  )

  if (multi) {
    keep_penalty <- TRUE
    stacked_survfit <-
      purrr::map2_dfr(y, penalty,
                      ~stack_survfit(.x, n = nrow(new_x), penalty = .y))
    starting_rows <- stacked_survfit %>%
      dplyr::distinct(.row, penalty) %>%
      dplyr::bind_cols(prob_template)
  } else {
    keep_penalty <- FALSE
    stacked_survfit <-
      stack_survfit(y, nrow(new_x))
    starting_rows <- stacked_survfit %>%
      dplyr::distinct(.row) %>%
      dplyr::bind_cols(prob_template)
  }
  res <- dplyr::bind_rows(starting_rows, stacked_survfit) %>%
    interpolate_km_values(time, new_strata) %>%
    keep_cols(output, keep_penalty) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  if (!is.null(missings_in_new_data)) {
    res <- pad_survival_na(res, missings_in_new_data, time,
                           interval = "none", n_total)
  }
  res
}


get_missings_coxnet <- function(new_x, new_strata) {
  missings_logical <- apply(cbind(new_x, new_strata), MARGIN = 1, anyNA)
  if (!any(missings_logical)) {
    return(NULL)
  }
  which(missings_logical)
}


#' A wrapper for survival times with coxnet models
#' @param object A fitted `_coxnet` object.
#' @param new_data Data for prediction.
#' @param penalty Penalty value(s).
#' @param ... Options to pass to [survival::survfit()].
#' @return A vector.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- proportional_hazards(penalty = 0.1) %>%
#'   set_engine("glmnet") %>%
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_time_coxnet(cox_mod, new_data = lung[1:3, ], penalty = 0.1)
survival_time_coxnet <- function(object, new_data, penalty = NULL, ...) {

  new_x <- parsnip::.convert_form_to_xy_new(
    object$preproc$coxnet,
    new_data,
    composition = "matrix")$x

  if (has_strata(object$formula, object$training_data)) {
    new_strata <- get_strata_glmnet(object$formula, data = new_data,
                                    na.action = stats::na.pass)
  } else {
    new_strata <- NULL
  }

  missings_in_new_data <- get_missings_coxnet(new_x, new_strata)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- rep(NA, n_missing)
      return(ret)
    }
    new_x <- new_x[-missings_in_new_data, , drop = FALSE]
    new_strata <- new_strata[-missings_in_new_data]
  }

  y <- survival::survfit(
    object$fit,
    newx = new_x,
    newstrata = new_strata,
    s = penalty,
    x = object$training_data$x,
    y = object$training_data$y,
    weights = object$preproc$coxnet$weights,
    na.action = stats::na.exclude,
    ...
  )

  tabs <- summary(y)$table
  if (is.matrix(tabs)) {
    colnames(tabs) <- gsub("[[:punct:]]", "", colnames(tabs))
    res <- unname(tabs[, "rmean"])
  } else {
    names(tabs) <- gsub("[[:punct:]]", "", names(tabs))
    res <- unname(tabs["rmean"])
  }
  if (!is.null(missings_in_new_data)) {
    index_with_na <- rep(NA, n_total)
    index_with_na[-missings_in_new_data] <- seq_along(res)
    res <- res[index_with_na]
  }
  res
}
