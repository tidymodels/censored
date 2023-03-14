#' Internal helper function for aorsf objects
#' @param object A model object from `aorsf::orsf()`.
#' @param new_data A data frame to be predicted.
#' @param time A vector of times to predict the survival probability.
#' @return A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @name aorsf_internal
#' @examples
#' library(aorsf)
#' aorsf <- orsf(na.omit(lung), Surv(time, status) ~ age + ph.ecog, n_tree = 10)
#' preds <- survival_prob_orsf(aorsf, lung[1:3, ], time = c(250, 100))
survival_prob_orsf <- function(object, new_data, time) {
  # This is not just a `post` hook in `set_pred()` because parsnip adds the
  # argument `time` to the prediction call and `aorsf::predict.orsf_fit()`
  # expects empty dots, i.e. no `time` argument.

  res <- predict(
    object,
    new_data = new_data,
    pred_horizon = time,
    pred_type = "surv",
    na_action = "pass",
    boundary_checks = FALSE
  )

  res <- matrix_to_nested_tibbles_survival(res, time)

  # return a tibble
  tibble(.pred = res)
}
