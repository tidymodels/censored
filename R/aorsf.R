
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
#' lung_orsf <- na.omit(lung)
#' lung_orsf$status <- lung_orsf$status - 1 # aorsf is picky
#' aorsf <- orsf(Surv(time, status) ~ age + ph.ecog, data = lung_orsf, n_tree = 10)
#' preds <- survival_prob_orsf(aorsf, lung[1:3, ], time = c(250, 100))
#' preds <- hazard_orsf(aorsf, lung[1:3, ], time = c(250, 100))

survival_prob_orsf <- function(object, new_data, time, ...) {
  orsf_predict(object, new_data, time, output = 'survival')
}

#' @rdname aorsf_internal
#' @export
hazard_orsf <- function(object, new_data, time, ...) {
  orsf_predict(object, new_data, time, output = 'hazard')
}

orsf_predict <- function(object, new_data, time, output){

  output <- rlang::arg_match(output, c("survival", "hazard"))

  switch(
    output,

    'survival' = {
     pred_type <- 'surv'
     .name <- '.pred_survival'
    },

    'hazard' = {
      pred_type <- 'chf'
      .name <- '.pred_hazard'
    }

  )

  time_ordered <- order(time)
  pred_horizon <- time[time_ordered] # aorsf wants time in ascending order

  res <- predict(object,
                 new_data = new_data,
                 pred_horizon = pred_horizon,
                 pred_type = pred_type)

  # preds in the same order as time
  res <- res[, order(time_ordered), drop = FALSE]

  res <- matrix_to_nested_tibbles(res, time, .name = .name)

  # return a tibble?
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

