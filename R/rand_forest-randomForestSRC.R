#' Internal helper functions for randomForestSRC survival models
#'
#' @param formula A model formula with a [survival::Surv()] response.
#' @param data A data frame containing the response and predictors.
#' @param weights A numeric vector of case weights, or `NULL`.
#' @param ... Additional arguments passed to [randomForestSRC::rfsrc()].
#' @param object A parsnip `model_fit` object resulting from
#'   [rand_forest() with engine = "randomForestSRC"][parsnip::rand_forest].
#' @param new_data A data frame to be predicted.
#' @param eval_time A vector of times to predict the survival probability.
#' @return
#' * `rfsrc_train()`: a fitted `rfsrc` object.
#' * `survival_time_rfsrc()`: A numeric vector of predicted survival times.
#' * `survival_prob_rfsrc()`: A tibble with a list column of nested tibbles.
#' @keywords internal
#' @name randomForestSRC_internal
NULL

#' @rdname randomForestSRC_internal
#' @export
rfsrc_train <- function(formula, data, weights = NULL, ...) {
  # To normalize a 1/2 status variable to 0/1, evaluate the `Surv()` call and
  # then pass on the time and status columns to `rfsrc()`.
  # This also makes it possible to use a pre-made Surv object as the response.

  surv <- eval(formula[[2]], envir = data, enclos = environment(formula))
  if (!.is_censored_right(surv)) {
    surv_type <- .extract_surv_type(surv)
    cli::cli_abort(
      "The {.val randomForestSRC} engine only supports right-censored data,
       not data with censoring type {.val {surv_type}}."
    )
  }

  # drop the original outcome columns so a `.` in the formula's right-hand side
  # can't pick them up as predictors
  data[intersect(names(data), all.vars(formula[[2]]))] <- NULL

  time_nm <- avoid_name_collision("..y_time", names(data))
  status_nm <- avoid_name_collision("..y_status", names(data))
  data[[time_nm]] <- .extract_surv_time(surv)
  data[[status_nm]] <- .extract_surv_status(surv)
  formula[[2]] <- call("Surv", as.name(time_nm), as.name(status_nm))

  randomForestSRC::rfsrc(formula, data = data, case.wt = weights, ...)
}

avoid_name_collision <- function(nm, existing) {
  while (nm %in% existing) {
    nm <- paste0(".", nm)
  }
  nm
}

#' @rdname randomForestSRC_internal
#' @details
#' `survival_time_rfsrc()` returns the median survival time, i.e. the first
#' event time at which the predicted survival curve drops to 0.5 or below. A
#' curve that never crosses 0.5 has an undefined median and yields `NA`; the
#' helper does not impute it.
#' For a rank-based use such as the concordance index, these `NA`s can be
#' imputed as `Inf` in post-processing (a non-crossing curve marks the longest
#' survivors, so `Inf` ranks them at the top).
#' @export
survival_time_rfsrc <- function(object, new_data) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "rfsrc", arg = "object$fit")
  check_data_frame(new_data)

  pred <- predict(engine_fit, newdata = new_data, na.action = "na.impute")
  surv_matrix <- pred$survival
  times <- pred$time.interest

  apply(surv_matrix, 1, function(srow) {
    below <- which(srow <= 0.5)
    if (length(below) == 0) NA_real_ else times[min(below)]
  })
}

#' @rdname randomForestSRC_internal
#' @export
survival_prob_rfsrc <- function(object, new_data, eval_time) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "rfsrc", arg = "object$fit")
  check_data_frame(new_data)

  pred <- predict(engine_fit, newdata = new_data, na.action = "na.impute")

  # $survival is obs x time.interest; survival_curve_to_prob() wants time x obs
  survival_prob <- survival_curve_to_prob(
    eval_time,
    event_times = pred$time.interest,
    survival_prob = t(pred$survival)
  )

  # survival_prob is length(eval_time) x nrow(new_data)
  n_obs <- ncol(survival_prob)
  tibble::tibble(
    .row = rep(seq_len(n_obs), each = length(eval_time)),
    .eval_time = rep(eval_time, times = n_obs),
    .pred_survival = as.vector(survival_prob)
  ) |>
    tidyr::nest(.pred = c(-.row)) |>
    dplyr::select(-.row)
}
