#' @importFrom dials new_quant_param
#' @importFrom dplyr %>% arrange bind_rows group_nest mutate
#' @importFrom generics fit
#' @importFrom parsnip bag_tree check_final_param eval_args model_printer
#' @importFrom parsnip multi_predict new_model_spec null_value
#' @importFrom parsnip predict_raw predict_survival predict.model_fit
#' @importFrom parsnip set_encoding set_model_arg set_new_model show_call
#' @importFrom parsnip translate translate.default update_dot_check
#' @importFrom parsnip update_engine_parameters update_main_parameters
#' @importFrom purrr map map_dbl map_lgl
#' @importFrom rlang abort call2 empty_env enquo enquos eval_tidy expr is_call
#' @importFrom rlang new_quosure
#' @importFrom stats approx na.exclude na.pass predict quantile setNames
#' @importFrom survival strata untangle.specials
#' @importFrom tibble as_tibble is_tibble tibble
#' @importFrom tidyr gather pivot_longer
#' @importFrom utils getFromNamespace
#' @importFrom withr with_options
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
