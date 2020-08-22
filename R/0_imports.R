#' @importFrom rlang enquo expr enquos
#' @importFrom purrr map_lgl map_dbl
#' @importFrom tibble is_tibble as_tibble
#' @importFrom parsnip set_new_model new_model_spec update_dot_check null_value
#' @importFrom parsnip set_encoding set_model_arg eval_args predict.model_fit
#' @importFrom withr with_options
#' @importFrom stats predict approx
#' @importFrom dials new_quant_param
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_nest
#' @importFrom baguette bag_tree

utils::globalVariables(
  c(".time", "object", "new_data")
)

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {

  # This defines cox_reg in the model database
  make_cox_reg_survival()
  make_cox_reg_glmnet()

  make_boost_tree_mboost()

  make_decision_tree_rpart()
  make_decision_tree_party()

  make_bag_tree_ipred()

}
