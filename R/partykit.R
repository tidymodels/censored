#' A wrapper for survival probabilities with partykit models
#' @param object A model object from `partykit::ctree()` or `partykit::cforest()`.
#' @param new_data A data frame to be predicted.
#' @param time A vector of times to predict the survival probability.
#' @param output Type of output. Can be either `"surv"` or `"haz"`.
#' @return A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @examples
#' library(partykit)
#' c_tree <- ctree(Surv(time, status) ~ age + ph.ecog, data = lung)
#' survival_prob_partykit(c_tree, lung[1:3, ], time = 100)
#' c_forest <- cforest(Surv(time, status) ~ age + ph.ecog, data = lung, ntree = 10)
#' survival_prob_partykit(c_forest, lung[1:3, ], time = 100)
survival_prob_partykit <- function(object, new_data, time, output = "surv") {
  # don't use the confidence intervals: the KM does not know about
  # how the sample for it got constructed (by the tree) and thus standard errors
  # don't take that into account
  output <- rlang::arg_match(output, c("surv", "haz"))

  n_obs <- nrow(new_data)
  # partykit handles missing values
  missings_in_new_data <- NULL

  y <- predict(object, newdata = new_data, type = "prob")

  survfit_summary_list <- purrr::map(y, summary, times = time, extend = TRUE)
  survfit_summary_combined <- combine_list_of_survfit_summary(
    survfit_summary_list,
    time = time
  )

  res <- survfit_summary_patch(
    survfit_summary_combined,
    index_missing = missings_in_new_data,
    time = time,
    n_obs = n_obs
  ) %>%
    survfit_summary_to_tibble(time = time, n_obs = n_obs) %>%
    keep_cols(output) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  res
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
