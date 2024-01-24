#' Internal helper function for aorsf objects
#' @param object A parsnip `model_fit` object resulting from 
#' [rand_forest() with engine = "aorsf"][parsnip::details_rand_forest_aorsf].
#' @param new_data A data frame to be predicted.
#' @param eval_time A vector of times to predict the survival probability.
#' @param time Deprecated in favor of `eval_time`. A vector of times to predict the survival probability.
#' @return A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @name aorsf_internal
#' @examplesIf rlang::is_installed("aorsf")
#' mod <- rand_forest() %>%
#'   set_engine("aorsf") %>%
#'   set_mode("censored regression") %>%
#'   fit(Surv(time, status) ~ age + ph.ecog, data = na.omit(lung))
#' preds <- survival_prob_orsf(mod, lung[1:3, ], eval_time = c(250, 100))
survival_prob_orsf <- function(object, new_data, eval_time, time = deprecated()) {
  if (inherits(object, "orsf_fit")) {
    cli::cli_abort("{.arg object} needs to be a parsnip {.cls model_fit} object, not a {.cls orsf_fit} object.")
  }
  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "survival_prob_orsf(time)",
      "survival_prob_orsf(eval_time)"
    )
    eval_time <- time
  }

  pred <- predict(
    object$fit,
    new_data = new_data,
    pred_horizon = eval_time,
    pred_type = "surv",
    na_action = "pass",
    boundary_checks = FALSE
  )

  n_obs <- nrow(new_data)
  n_eval_time <- length(eval_time)

  res <- data.frame(
    .row = rep(seq_len(n_obs), times = n_eval_time),
    .eval_time = rep(eval_time, each = n_obs),
    .pred_survival =  as.numeric(pred)
  ) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  res
}
