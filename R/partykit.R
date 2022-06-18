
#' Internal helper functions for party objects
#' @param object A model object from `partykit::ctree()` or `partykit::cforest()`.
#' @param new_data A data frame to be predicted.
#' @param time A vector of times to predict the survival probability.
#' @param output Type of output.
#' @return A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @name party_internal
#' @examples
#' library(partykit)
#' c_forest <- cforest(Surv(time, status) ~ age + ph.ecog, data = lung, ntree = 10)
#' survival_prob_cforest(c_forest, lung[1:3, ], time = 100)
#' c_tree <- ctree(Surv(time, status) ~ age + ph.ecog, data = lung)
#' survival_prob_ctree(c_tree, lung[1:3, ], time = 100)
survival_prob_cforest <- function(object, new_data, time, output = "surv") {
  output <- rlang::arg_match(output, c("surv", "conf", "haz"))

  res <- predict(object, newdata = new_data, type = "prob")
  res <- purrr::map(res, approx_surv_fit, time, output)
  tibble::tibble(.pred = res)
}

#' @export
#' @keywords internal
#' @rdname party_internal
survival_prob_ctree <- function(object, new_data, time, output = "surv") {
  output <- rlang::arg_match(output, c("surv", "conf", "haz"))
  cl <-
    rlang::call2(
      "Predict",
      .ns = "modeltools",
      rlang::expr(object),
      rlang::expr(new_data),
      type = "prob"
    )
  res <- rlang::eval_tidy(cl)
  res <- purrr::map(res, approx_surv_fit, time, output)
  tibble::tibble(.pred = res)
}

approx_surv_fit <- function(x, .time = 0, output = "surv") {
  dplyr::bind_rows(prob_template, stack_survfit(x, n = 1)) %>%
    interpolate_km_values_ungrouped(.time) %>%
    keep_cols(output) %>%
    dplyr::select(-.row)
}


# ------------------------------------------------------------------------------
# Enable easy tuning of engine parameters

cforest_engine_args <-
  tibble::tibble(
    name = c(
      "teststat",
      "testtype",
      "mincriterion"
    ),
    call_info = list(
      list(pkg = "dials", fun = "conditional_test_statistic"),
      list(pkg = "dials", fun = "conditional_test_type"),
      list(pkg = "dials", fun = "conditional_min_criterion")
    ),
    source = "model_spec",
    component = "rand_forest",
    component_id = "engine"
  )

ctree_engine_args <-
  tibble::tibble(
    name = c(
      "teststat",
      "testtype",
      "mincriterion"
    ),
    call_info = list(
      list(pkg = "dials", fun = "conditional_test_statistic"),
      list(pkg = "dials", fun = "conditional_test_type"),
      list(pkg = "dials", fun = "conditional_min_criterion")
    ),
    source = "model_spec",
    component = "decision_tree",
    component_id = "engine"
  )
