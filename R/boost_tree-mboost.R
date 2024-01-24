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
  function(formula, data, family, weights = NULL,
           teststat = "quadratic", testtype = "Teststatistic",
           mincriterion = 0, minsplit = 10,
           minbucket = 4, maxdepth = 2, saveinfo = FALSE, ...) {
    other_args <- list(...)
    protect_ct <- c(
      "teststat", "testtype", "mincriterion", "minsplit",
      "minbucket", "maxdepth", "saveinfo"
    )
    protect_cb <- character()
    protect_fit <- c("formula", "data", "family", "weight")

    ct_names <- names(formals(getFromNamespace("ctree_control", "partykit")))
    cb_names <- names(formals(getFromNamespace("boost_control", "mboost")))
    fit_names <- names(formals(getFromNamespace("blackboost", "mboost")))

    other_args <- other_args[!(other_args %in%
      c(protect_ct, protect_cb, protect_fit))]

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
predict_linear_pred._blackboost <- function(object,
                                            new_data,
                                            ...,
                                            increasing = TRUE) {
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
#' mod <- boost_tree() %>%
#'   set_engine("mboost") %>%
#'   set_mode("censored regression") %>%
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_prob_mboost(mod, new_data = lung[1:3, ], eval_time = 300)
survival_prob_mboost <- function(object, new_data, eval_time, time = deprecated()) {
  if (inherits(object, "mboost")) {
    cli::cli_abort("{.arg object} needs to be a parsnip {.cls model_fit} object, not a {.cls mboost} object.")
  }

  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "survival_prob_mboost(time)",
      "survival_prob_mboost(eval_time)"
    )
    eval_time <- time
  }

  survival_curve <- mboost::survFit(object$fit, newdata = new_data)

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
  ) %>%
    tidyr::nest(.pred = c(-.row)) %>%
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
#' boosted_tree <- boost_tree() %>%
#'   set_engine("mboost") %>%
#'   set_mode("censored regression") %>%
#'   fit(Surv(time, status) ~ age + ph.ecog, data = lung[-14, ])
#' survival_time_mboost(boosted_tree, new_data = lung[1:3, ])
survival_time_mboost <- function(object, new_data) {
  if (inherits(object, "mboost")) {
    cli::cli_abort("{.arg object} needs to be a parsnip {.cls model_fit} object, not a {.cls mboost} object.")
  }

  y <- mboost::survFit(object$fit, new_data)

  stacked_survfit <- stack_survfit(y, n = nrow(new_data))

  starting_rows <- stacked_survfit %>%
    dplyr::distinct(.row) %>%
    dplyr::bind_cols(prob_template)

  res <- dplyr::bind_rows(starting_rows, stacked_survfit) %>%
    dplyr::group_by(.row) %>%
    dplyr::mutate(
      next_event_time = dplyr::lead(.time),
      time_interval = next_event_time - .time,
      sum_component = time_interval * .pred_survival
    ) %>%
    dplyr::summarize(.pred_time = sum(sum_component, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(.pred_time)

  res
}
