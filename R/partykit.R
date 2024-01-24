#' A wrapper for survival probabilities with partykit models
#' @param object A parsnip `model_fit` object resulting from 
#' [decision_tree() with engine = "partykit"][parsnip::details_decision_tree_partykit] or 
#' [rand_forest() with engine = "partykit"][parsnip::details_rand_forest_partykit].
#' @param new_data A data frame to be predicted.
#' @param eval_time A vector of times to predict the survival probability.
#' @param time Deprecated in favor of `eval_time`. A vector of times to predict the survival probability.
#' @param output Type of output. Can be either `"surv"` or `"haz"`.
#' @return A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @examplesIf rlang::is_installed(c("partykit", "coin"))
#' tree <- decision_tree() %>%
#'   set_mode("censored regression") %>%
#'   set_engine("partykit") %>%
#'   fit(Surv(time, status) ~ age + ph.ecog, data = lung)
#' survival_prob_partykit(tree, lung[1:3, ], eval_time = 100)
#' forest <- rand_forest() %>%
#'   set_mode("censored regression") %>%
#'   set_engine("partykit") %>%
#'   fit(Surv(time, status) ~ age + ph.ecog, data = lung)
#' survival_prob_partykit(forest, lung[1:3, ], eval_time = 100)
survival_prob_partykit <- function(object,
                                   new_data,
                                   eval_time,
                                   time = deprecated(),
                                   output = "surv") {
  if (inherits(object, "party")) {
    cli::cli_abort("{.arg object} needs to be a parsnip {.cls model_fit} object, not a {.cls party} object.")
  }

  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "survival_prob_partykit(time)",
      "survival_prob_partykit(eval_time)"
    )
    eval_time <- time
  }

  # don't use the confidence intervals: the KM does not know about
  # how the sample for it got constructed (by the tree) and thus standard errors
  # don't take that into account
  output <- rlang::arg_match(output, c("surv", "haz"))

  n_obs <- nrow(new_data)
  # partykit handles missing values
  missings_in_new_data <- NULL

  y <- predict(object$fit, newdata = new_data, type = "prob")

  survfit_summary_list <- purrr::map(y, summary, times = eval_time, extend = TRUE)
  survfit_summary_combined <- combine_list_of_survfit_summary(
    survfit_summary_list,
    eval_time = eval_time
  )

  res <- survfit_summary_patch(
    survfit_summary_combined,
    index_missing = missings_in_new_data,
    eval_time = eval_time,
    n_obs = n_obs
  ) %>%
    survfit_summary_to_tibble(eval_time = eval_time, n_obs = n_obs) %>%
    keep_cols(output) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  res
}
