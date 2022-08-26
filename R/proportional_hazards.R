# The model specification is in parsnip.

#' @export
fit.proportional_hazards <- function(object,
                                     formula,
                                     data,
                                     control = parsnip::control_parsnip(),
                                     ...) {

  # call parsnip::fit.model_spec()
  res <- NextMethod()

  # we clean up the $fit slot which contains elements needed for prediction:
  # - the training data (because glmnet requires it)
  # - the preprocessing elements and formula (for the translation between
  #   formula and matrix interface)
  if (object$engine == "glmnet") {
    training_data_ind <- names(res$fit$preproc) %in% c("x", "y")
    res$training_data <- res$fit$preproc[training_data_ind]
    # this is not stored in $preproc directly due to how prediction in parsnip
    # is set up: parsnip::prepare_data() would automatically run with the
    # modified terms in $preproc$terms which are missing the strata term
    res$preproc$coxnet <- res$fit$preproc[!training_data_ind]
    res$fit <- res$fit$fit
    res$formula <- formula
  }

  res
}
