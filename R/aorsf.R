
#' Internal helper functions for aorsf objects
#' @param object A model object from `aorsf::orsf()`.
#' @param new_data A data frame to be predicted.
#' @param time A vector of times to predict the survival probability.
#' @param output Type of output.
#' @return A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @name party_internal
#' @examples
#' library(aorsf)
#' lung_orsf <- na.omit(lung)
#' lung_orsf$status <- lung_orsf$status - 1 # aorsf is picky
#' aorsf <- orsf(Surv(time, status) ~ age + ph.ecog, data = lung_orsf, n_tree = 10)
#' survival_prob_orsf(aorsf, lung[1:3, ], time = 100)
survival_prob_orsf <- function(object, new_data, time, output = "surv") {

  output <- rlang::arg_match(output, c("surv"))

  pred_horizon <- sort(time, decreasing = FALSE) # aorsf is picky

  res <- predict(object,
                 new_data = new_data,
                 pred_horizon = pred_horizon,
                 # to get survival prob instead of 1 - survival
                 risk = FALSE)

  # km interpolate is not recommended for aorsf; use ensemble survival prob
  res_tbls <- apply(res,
                    MARGIN = 1,
                    FUN = function(.x)
                      tibble::tibble(.pred_survival = .x),
                    simplify = FALSE) %>%
    purrr::map(
      tibble::add_column,
      .time = pred_horizon,
      .before = ".pred_survival"
    )

  tibble::tibble(.pred = res_tbls)

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

