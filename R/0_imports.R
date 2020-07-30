#' @importFrom rlang enquo
#' @importFrom purrr map_lgl
#' @importFrom tibble is_tibble as_tibble
#' @importFrom parsnip set_new_model new_model_spec update_dot_check null_value
#' @importFrom withr with_options
#' @importFrom stats predict
#' @importFrom dials new_quant_param

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines cox_reg in the model database
  make_cox_reg_survival()

}
