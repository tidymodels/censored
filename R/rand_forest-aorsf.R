#' Internal helper function for aorsf objects
#' @param object A model object from `aorsf::orsf()`.
#' @param new_data A data frame to be predicted.
#' @param eval_time A vector of times to predict the survival probability.
#' @param time Deprecated in favor of `eval_time`. A vector of times to predict the survival probability.
#' @return A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @name aorsf_internal
#' @examples
#' library(aorsf)
#' rf_spec <-
#'   rand_forest(trees = 10) %>%
#'   set_engine("aorsf", na_action = 'impute_meanmode') %>%
#'   set_mode("censored regression")
#' rf_fit <- fit(rf_spec, Surv(time, status) ~ age + ph.ecog, data = lung)
#' preds <- survival_prob_orsf(rf_fit, new_data = lung[1:3, ], eval_time = c(250, 100))
survival_prob_orsf <- function(object, new_data, eval_time, time = deprecated()) {
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
