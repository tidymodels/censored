# mboost helpers -----------------------------------------------------------------

#' Boosted trees via mboost
#'
#' `blackboost_train()` is a wrapper for the `blackboost()` function in the
#' \pkg{mboost} package that fits tree-based models
#'  where all of the model arguments are in the main function.
#'
#' @param formula A symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param family A [mboost::Family()] object.
#' @param weights An optional vector of weights to be used in the fitting process.
#' @param teststat A character specifying the type of the test statistic to be
#'   applied for variable selection.
#' @param testtype A character specifying how to compute the distribution of
#'   the test statistic. The first three options refer to p-values as criterion,
#'   `"Teststatistic"` uses the raw statistic as criterion. `"Bonferroni"` and
#'   `"Univariate"` relate to p-values from the asymptotic distribution (adjusted or
#'   unadjusted). Bonferroni-adjusted Monte-Carlo p-values are computed when
#'   both `"Bonferroni"` and `"MonteCarlo"` are given.
#' @param mincriterion The value of the test statistic or 1 - p-value that must
#'   be exceeded in order to implement a split.
#' @param minsplit The minimum sum of weights in a node in order to be
#'   considered for splitting.
#' @param minbucket The minimum sum of weights in a terminal node.
#' @param maxdepth The maximum depth of the tree. The default `maxdepth = Inf` means
#'   that no restrictions are applied to tree sizes.
#' @param saveinfo Logical. Store information about variable selection procedure
#'   in info slot of each partynode.
#' @param ... Other arguments to pass.
#' @return A fitted blackboost model.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("mboost")
#' blackboost_train(Surv(time, status) ~ age + ph.ecog,
#'   data = lung[-14, ], family = mboost::CoxPH()
#' )
blackboost_train <-
  function(
    formula,
    data,
    family,
    weights = NULL,
    teststat = "quadratic",
    testtype = "Teststatistic",
    mincriterion = 0,
    minsplit = 10,
    minbucket = 4,
    maxdepth = 2,
    saveinfo = FALSE,
    ...
  ) {
    other_args <- list(...)
    protect_ct <- c(
      "teststat",
      "testtype",
      "mincriterion",
      "minsplit",
      "minbucket",
      "maxdepth",
      "saveinfo"
    )
    protect_cb <- character()
    protect_fit <- c("formula", "data", "family", "weight")

    ct_names <- names(formals(getFromNamespace("ctree_control", "partykit")))
    cb_names <- names(formals(getFromNamespace("boost_control", "mboost")))
    fit_names <- names(formals(getFromNamespace("blackboost", "mboost")))

    other_args <- other_args[
      !(other_args %in%
        c(protect_ct, protect_cb, protect_fit))
    ]

    ct_args <- other_args[names(other_args) %in% ct_names]
    cb_args <- other_args[names(other_args) %in% cb_names]
    fit_args <- other_args[names(other_args) %in% fit_names]

    # partykit::ctree_control
    ct_ctrl <- call2("ctree_control", .ns = "partykit")
    ct_ctrl$teststat <- teststat
    ct_ctrl$testtype <- testtype
    ct_ctrl$mincriterion <- mincriterion
    ct_ctrl$minsplit <- minsplit
    ct_ctrl$minbucket <- minbucket
    ct_ctrl$maxdepth <- maxdepth
    ct_ctrl$saveinfo <- saveinfo
    ct_ctrl <- rlang::call_modify(ct_ctrl, !!!ct_args)

    # mboost::boost_control
    cb_ctrl <- call2("boost_control", .ns = "mboost")
    cb_ctrl <- rlang::call_modify(cb_ctrl, !!!cb_args)

    # Fit
    fit_call <- call2("blackboost", .ns = "mboost")
    fit_call$formula <- expr(formula)
    fit_call$data <- expr(data)
    fit_call$family <- expr(family)
    fit_call$control <- cb_ctrl
    fit_call$tree_controls <- ct_ctrl
    if (!is.null(weights)) {
      fit_call$weights <- quote(weights)
    }
    fit_call <- rlang::call_modify(fit_call, !!!fit_args)

    eval_tidy(fit_call)
  }

#' @export
predict_linear_pred._blackboost <- function(
  object,
  new_data,
  ...,
  increasing = TRUE
) {
  res <- NextMethod()
  if (increasing) {
    # For consistency with other models, we want the lp to increase with
    # time. For this, we change the sign
    res <- -res
  }
  res
}

#' A wrapper for survival probabilities with mboost models
#' @param object A parsnip `model_fit` object resulting from [boost_tree() with engine = "mboost"][parsnip::details_boost_tree_mboost].
#' @param new_data Data for prediction.
#' @param eval_time A vector of integers for prediction times.
#' @param time Deprecated in favor of `eval_time`. A vector of integers for prediction times.
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("mboost")
#' mod <- boost_tree() |>
#'   set_engine("mboost") |>
#'   set_mode("censored regression") |>
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_prob_mboost(mod, new_data = lung[1:3, ], eval_time = 300)
survival_prob_mboost <- function(
  object,
  new_data,
  eval_time,
  time = deprecated()
) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "mboost", arg = "object$fit")
  check_data_frame(new_data)

  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "survival_prob_mboost(time)",
      "survival_prob_mboost(eval_time)"
    )
    eval_time <- time
  }

  survival_curve <- mboost::survFit(engine_fit, newdata = new_data)

  survival_prob <- survival_curve_to_prob(
    eval_time,
    event_times = survival_curve$time,
    survival_prob = survival_curve$surv
  )

  # survival_prob is length(eval_time) x nrow(new_data)
  n_obs <- ncol(survival_prob)
  ret <- tibble::tibble(
    .row = rep(seq_len(n_obs), each = length(eval_time)),
    .eval_time = rep(eval_time, times = n_obs),
    .pred_survival = as.vector(survival_prob)
  ) |>
    tidyr::nest(.pred = c(-.row)) |>
    dplyr::select(-.row)

  ret
}

survival_curve_to_prob <- function(eval_time, event_times, survival_prob) {
  # add survival prob of 1 and 0 at the start and end of time, respectively
  if (event_times[1] != -Inf) {
    event_times <- c(-Inf, event_times)
    survival_prob <- rbind(1, survival_prob)
  }
  if (event_times[length(event_times)] != Inf) {
    event_times <- c(event_times, Inf)
    survival_prob <- rbind(survival_prob, 0)
  }

  # get survival probability (intervals are closed on the left, open on the right)
  index <- findInterval(eval_time, event_times)

  survival_prob[index, , drop = FALSE]
}


#' A wrapper for mean survival times with `mboost` models
#' @param object A parsnip `model_fit` object resulting from [boost_tree() with engine = "mboost"][parsnip::details_boost_tree_mboost].
#' @param new_data Data for prediction
#' @return A tibble.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("mboost")
#' boosted_tree <- boost_tree() |>
#'   set_engine("mboost") |>
#'   set_mode("censored regression") |>
#'   fit(Surv(time, status) ~ age + ph.ecog, data = lung[-14, ])
#' survival_time_mboost(boosted_tree, new_data = lung[1:3, ])
survival_time_mboost <- function(object, new_data) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "mboost", arg = "object$fit")
  check_data_frame(new_data)

  y <- mboost::survFit(engine_fit, new_data)

  stacked_survfit <- stack_survfit(y, n = nrow(new_data))

  starting_rows <- stacked_survfit |>
    dplyr::distinct(.row) |>
    dplyr::bind_cols(prob_template)

  res <- dplyr::bind_rows(starting_rows, stacked_survfit) |>
    dplyr::group_by(.row) |>
    dplyr::mutate(
      next_event_time = dplyr::lead(.time),
      time_interval = next_event_time - .time,
      sum_component = time_interval * .pred_survival
    ) |>
    dplyr::summarize(.pred_time = sum(sum_component, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(.pred_time)

  res
}


# multi_predict -----------------------------------------------------------

#' @export
multi_predict._blackboost <- function(
  object,
  new_data,
  type = NULL,
  opts = list(),
  trees = NULL,
  ...
) {
  dots <- list(...)

  check_installs(object$spec)
  load_libs(object$spec, quiet = TRUE)

  type <- check_pred_type(object, type)
  check_spec_pred_type(object, type)
  if (type != "raw" && length(opts) > 0) {
    rlang::warn("`opts` is only used with `type = 'raw'` and was ignored.")
  }
  check_pred_type_dots(object, type, ...)

  engine_fit <- hardhat::extract_fit_engine(object)
  mstop_original <- mboost::mstop(engine_fit)
  on.exit(engine_fit[mstop_original], add = TRUE)

  trees <- check_trees(trees, mstop_original)

  pred <- switch(
    type,
    "time" = multi_predict_mboost_time(object, new_data, trees),
    "survival" = multi_predict_mboost_survival(
      object,
      new_data = new_data,
      trees = trees,
      ...
    ),
    "linear_pred" = multi_predict_mboost_linear_pred(
      object,
      new_data = new_data,
      trees = trees,
      opts = dots
    )
  )

  pred
}

check_trees <- function(trees, mstop_original, call = caller_env()) {
  if (is.null(trees)) {
    trees <- mstop_original
  }
  if (
    !is.numeric(trees) || any(trees != as.integer(trees)) || any(trees < 1L)
  ) {
    cli::cli_abort(
      "{.arg trees} must be a vector of positive integers.",
      call = call
    )
  }
  trees <- sort(unique(as.integer(trees)))
  if (any(trees > mstop_original)) {
    cli::cli_abort(
      c(
        "{.arg trees} values must not exceed the number of boosting \\
         iterations in the fitted model ({mstop_original}).",
        i = "mboost would otherwise refit additional iterations."
      ),
      call = call
    )
  }
  trees
}

multi_predict_mboost_time <- function(object, new_data, trees) {
  new_data <- parsnip::prepare_data(object, new_data)
  engine_fit <- hardhat::extract_fit_engine(object)
  n_obs <- nrow(new_data)

  per_t <- purrr::map(trees, function(t) {
    engine_fit[t]
    pred_t <- survival_time_mboost(object, new_data)
    tibble::tibble(
      .row = seq_len(n_obs),
      trees = t,
      .pred_time = pred_t$.pred_time
    )
  })

  vctrs::vec_rbind(!!!per_t) |>
    dplyr::arrange(.row, trees) |>
    tidyr::nest(.pred = c(-.row)) |>
    dplyr::select(-.row)
}

multi_predict_mboost_survival <- function(object, new_data, trees, ...) {
  dots <- list(...)
  dots$eval_time <- .filter_eval_time(dots$eval_time)

  new_data <- parsnip::prepare_data(object, new_data)
  engine_fit <- hardhat::extract_fit_engine(object)
  eval_time <- dots$eval_time

  per_t <- purrr::map(trees, function(t) {
    engine_fit[t]
    pred_t <- survival_prob_mboost(object, new_data, eval_time = eval_time)
    pred_t |>
      dplyr::mutate(.row = dplyr::row_number()) |>
      tidyr::unnest(cols = .pred) |>
      dplyr::mutate(trees = t)
  })

  vctrs::vec_rbind(!!!per_t) |>
    dplyr::select(.row, trees, .eval_time, .pred_survival) |>
    dplyr::arrange(.row, trees) |>
    tidyr::nest(.pred = c(-.row)) |>
    dplyr::select(-.row)
}

multi_predict_mboost_linear_pred <- function(object, new_data, trees, opts) {
  if ("increasing" %in% names(opts)) {
    increasing <- opts$increasing
  } else {
    increasing <- TRUE
  }

  new_data <- parsnip::prepare_data(object, new_data)
  engine_fit <- hardhat::extract_fit_engine(object)
  n_obs <- nrow(new_data)

  per_t <- purrr::map(trees, function(t) {
    engine_fit[t]
    pred_t <- as.numeric(predict(engine_fit, newdata = new_data))
    if (increasing) {
      pred_t <- -pred_t
    }
    tibble::tibble(
      .row = seq_len(n_obs),
      trees = t,
      .pred_linear_pred = pred_t
    )
  })

  vctrs::vec_rbind(!!!per_t) |>
    dplyr::arrange(.row, trees) |>
    tidyr::nest(.pred = c(-.row)) |>
    dplyr::select(-.row)
}
