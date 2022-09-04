
#' Internal helper functions for aorsf objects
#' @param object A model object from `aorsf::orsf()`.
#' @param new_data A data frame to be predicted.
#' @param time A vector of times to predict the survival probability.
#' @param output Type of output.
#' @return A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @name aorsf_internal
#' @examples
#' library(aorsf)
#' aorsf <- orsf(na.omit(lung), Surv(time, status) ~ age + ph.ecog, n_tree = 10)
#' preds <- survival_prob_orsf(aorsf, lung[1:3, ], time = c(250, 100))

survival_prob_orsf <- function(object, new_data, time, ...) {

  res <- predict(object,
                 new_data = new_data,
                 pred_horizon = time,
                 pred_type = 'surv',
                 na_action = 'pass',
                 boundary_checks = FALSE)

  res <- matrix_to_nested_tibbles_survival(res, time)

  # return a tibble
  tibble(.pred = res)

}

# ------------------------------------------------------------------------------
# Enable easy tuning of engine parameters

aorsf_engine_args <-
  tibble::tibble(
    name = c(
      "split_min_stat"
    ),
    call_info = list(
      list(pkg = "dials", fun = "conditional_min_criterion")
    ),
    source = "model_spec",
    component = "rand_forest",
    component_id = "engine"
  )

