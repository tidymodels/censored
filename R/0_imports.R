#' @importFrom dplyr %>%
#' @importFrom generics fit
#' @importFrom parsnip eval_args multi_predict predict_raw predict_survival
#' @importFrom parsnip predict_linear_pred
#' @importFrom parsnip predict.model_fit translate
#' @importFrom purrr map map_dbl
#' @importFrom rlang abort call2 empty_env enquos eval_tidy expr is_call
#' @importFrom rlang new_quosure
#' @importFrom stats na.exclude na.pass predict quantile setNames
#' @importFrom survival strata
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr gather pivot_longer
#' @importFrom utils getFromNamespace
NULL

utils::globalVariables(
  c("time", ".time", "object", "new_data", ".label", ".pred", ".cuts",
    ".id", ".tmp", "engine", "predictor_indicators", ".strata", "group")
)

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {

  make_proportional_hazards_survival()
  make_proportional_hazards_glmnet()

  make_boost_tree_mboost()

  make_decision_tree_rpart()

  make_bag_tree_ipred()

  make_survival_reg_survival()
  make_survival_reg_flexsurv()

  make_decision_tree_party()
  make_rand_forest_party()
}
