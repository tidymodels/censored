# TODO remove this after the next release of flexsurv,
# see https://github.com/chjackson/flexsurv-dev/pull/144
#' @export
fit_xy.survival_reg <- function(object,
                                x,
                                y,
                                case_weights = NULL,
                                control = parsnip::control_parsnip(),
                                ...) {

  if (object$engine == "flexsurv") {
    # flexsurv currently can't deal with the dot notation in the formula
    rlang::abort("For the `'flexsurv'` engine, please use the formula interface via `fit()`.")
  }

  # call parsnip::fit_xy.model_spec()
  res <- NextMethod()
  res
}
