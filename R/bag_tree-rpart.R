#' Bagged trees via ipred
#'
#' `bagging_surv_train()` is a wrapper for the `bagging()` function in the
#' \pkg{ipred} package that passes the main arguments for the trees on to
#' [rpart::rpart.control()].
#'
#' @param formula A symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param maxdepth The maximum depth of any node of the final tree.
#' @param minsplit The minimum number of observations in a node in order for
#'   a split to be attempted.
#' @param cp The complexity parameter.
#' @param ... Other arguments to pass to [rpart::rpart.control()] or
#'   [ipred::bagging()], matched by name.
#' @details Arguments for [rpart::rpart.control()] which are not set take their
#'   default values from \pkg{ipred}, or from a `control` object if one is
#'   passed via `...`. The main arguments and other [rpart::rpart.control()]
#'   arguments passed via `...` take precedence over a `control` object.
#' @return A fitted bagging model.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("ipred")
#' bagging_surv_train(Surv(time, status) ~ age + ph.ecog, data = lung, maxdepth = 2)
bagging_surv_train <- function(
  formula,
  data,
  maxdepth = NULL,
  minsplit = NULL,
  cp = NULL,
  ...
) {
  dots <- list(...)
  base_control <- dots[["control"]] %||% ipred_default_control()
  dots[["control"]] <- NULL
  is_control_arg <- names(dots) %in% names(formals(rpart::rpart.control))

  control_args <- purrr::list_modify(
    base_control,
    !!!purrr::compact(list(maxdepth = maxdepth, minsplit = minsplit, cp = cp)),
    !!!dots[is_control_arg]
  )
  control <- rlang::exec(rpart::rpart.control, !!!control_args)

  rlang::inject(
    ipred::bagging(
      formula,
      data = data,
      control = control,
      !!!dots[!is_control_arg]
    )
  )
}

ipred_default_control <- function() {
  # ipred's default is a call, e.g. `rpart.control(xval = 0)`
  ipredbagg_surv <- utils::getS3method(
    "ipredbagg",
    "Surv",
    envir = asNamespace("ipred")
  )
  rlang::call_args(formals(ipredbagg_surv)$control)
}

#' A wrapper for survival times with `survbagg` models
#' @param object A parsnip `model_fit` object resulting from [bag_tree() with engine = "rpart"][parsnip::details_bag_tree_rpart].
#' @param new_data Data for prediction
#' @return A vector.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("ipred")
#' bagged_tree <- bag_tree() |>
#'   set_engine("rpart") |>
#'   set_mode("censored regression") |>
#'   fit(Surv(time, status) ~ age + ph.ecog, data = lung)
#' survival_time_survbagg(bagged_tree, lung[1:3, ])
survival_time_survbagg <- function(object, new_data) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "survbagg", arg = "object$fit")
  check_data_frame(new_data)

  missings_in_new_data <- get_missings_survbagg(engine_fit, new_data)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- rep(NA, n_missing)
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, , drop = FALSE]
  }

  y <- predict(engine_fit, newdata = new_data)

  res <- purrr::map_dbl(y, \(.x) quantile(.x, probs = .5)$quantile)

  if (!is.null(missings_in_new_data)) {
    index_with_na <- rep(NA, n_total)
    index_with_na[-missings_in_new_data] <- seq_along(res)
    res <- res[index_with_na]
  }
  res
}

get_missings_survbagg <- function(object, new_data) {
  object <- object$mtrees[[1]]$btree
  trms <- stats::terms(object)
  trms <- stats::delete.response(trms)
  na_action <- (object$call)$na.action %||% rpart::na.rpart
  mod_frame <- stats::model.frame(
    trms,
    data = new_data,
    na.action = na_action,
    xlev = attr(object, "xlevels")
  )
  attr(mod_frame, "na.action")
}

#' A wrapper for survival probabilities with `survbagg` models
#' @param object A parsnip `model_fit` object resulting from [bag_tree() with engine = "rpart"][parsnip::details_bag_tree_rpart].
#' @param new_data Data for prediction.
#' @param eval_time A vector of prediction times.
#' @param time Deprecated in favor of `eval_time`. A vector of prediction times.
#' @return A vctrs list of tibbles.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("ipred")
#' bagged_tree <- bag_tree() |>
#'   set_engine("rpart") |>
#'   set_mode("censored regression") |>
#'   fit(Surv(time, status) ~ age + ph.ecog, data = lung)
#' survival_prob_survbagg(bagged_tree, lung[1:3, ], eval_time = 100)
survival_prob_survbagg <- function(
  object,
  new_data,
  eval_time,
  time = deprecated()
) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "survbagg", arg = "object$fit")
  check_data_frame(new_data)

  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "survival_prob_survbagg(time)",
      "survival_prob_survbagg(eval_time)"
    )
    eval_time <- time
  }

  check_eval_time(eval_time, allow_infinite = TRUE, allow_negative = TRUE)

  # we could access more than the survival probabilities but
  # we should not use the standard error and confidence intervals because
  # "the KM does not know about the tree at all", or more specifically,
  # how the sample of observations used for the KM is selected based on the tree
  output <- "surv"

  n_obs <- nrow(new_data)
  missings_in_new_data <- get_missings_survbagg(engine_fit, new_data)

  if (!is.null(missings_in_new_data)) {
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_obs
    if (all_missing) {
      ret <- predict_survival_na(eval_time)
      ret <- tibble(.pred = rep(list(ret), n_missing))
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, , drop = FALSE]
  }

  y <- predict(engine_fit, newdata = new_data)

  survfit_summary_list <- purrr::map(
    y,
    summary,
    times = eval_time,
    extend = TRUE
  )
  survfit_summary_combined <- combine_list_of_survfit_summary(
    survfit_summary_list,
    eval_time = eval_time
  )

  res <- survfit_summary_patch(
    survfit_summary_combined,
    index_missing = missings_in_new_data,
    eval_time = eval_time,
    n_obs = n_obs
  ) |>
    survfit_summary_to_tibble(eval_time = eval_time, n_obs = n_obs) |>
    keep_cols(output) |>
    tidyr::nest(.pred = c(-.row)) |>
    dplyr::select(-.row)

  res
}
