#' @export
fit_xy.decision_tree <- function(object,
                                 x,
                                 y,
                                 case_weights = NULL,
                                 control = parsnip::control_parsnip(),
                                 ...) {

  if (object$engine == "rpart") {
    # prodlim::EventHistory.frame() expects a call to `Surv()` (or `Hist()`) on
    # the left-hand side of the formula
    rlang::abort("For the `'rpart'` engine, please use the formula interface via `fit()`.")
  }

  # call parsnip::fit_xy.model_spec()
  res <- NextMethod()
  res
}
