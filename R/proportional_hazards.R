# The model specification is in parsnip.

#' @name proportional_hazards
#'
#' @title Parsnip engines for Proportional Hazards Models
#'
#' `proportional_hazards()` is a way to generate a _specification_ of a model before
#'  fitting and allows the model to be created using different packages in R.
#'  The main arguments for the model are:
#' \itemize{
#'   \item \code{penalty}: The total amount of regularization
#'  in the model. Note that this must be zero for some engines.
#'   \item \code{mixture}: The mixture amounts of different types of
#'   regularization (see below). Note that this will be ignored for some engines.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and arguments can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @details
#' Proportional hazards models include the Cox model.
#' For `proportional_hazards()`, the mode will always be "censored regression".
#'
#' The model can be created using the `fit()` function using the following _engines_:
#' \itemize{
#' \item \pkg{R}: `"survival"` (the default)
#' }
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the model fit call.
#' For this type of model, the template of the fit calls are:
#'
#' \pkg{survival} engine
#'
#' \preformatted{
#' survival::coxph(formula = missing_arg())
#' }
#'
#' Note that, for linear predictor prediction types, the results are formatted
#' for all models such that the prediction _increases_ with time. For the
#' proportional hazards model, the sign is reversed.
#'
#' @examples
#' parsnip::show_engines("proportional_hazards")
#'
#' library(survival)
#'
#' cox_mod <-
#'   proportional_hazards() %>%
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ x, data = aml)
NULL

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

# ------------------------------------------------------------------------------

# notes adapted from parsnip:

# glmnet call stack for censored regression using `predict` when object has
# classes "_coxnet" and "model_fit":
#
#  predict()
#   predict._coxnet(penalty = NULL)   <-- checks and sets penalty
#    predict.model_fit()              <-- checks for extra vars in ...
#     predict_survival()
#      predict_survival._coxnet()
#       predict_survival.model_fit()
#        survival_prob_coxnet()

# glmnet call stack for censored regression using `multi_predict(type = "linear_pred")` when object has
# classes "_coxnet" and "model_fit":
#
# 	multi_predict()
#    multi_predict._coxnet(penalty = NULL)
#      predict._coxnet(multi = TRUE)          <-- checks and sets penalty
#       predict.model_fit()                   <-- checks for extra vars in ...
#        predict_raw()
#         predict_raw._coxnet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.coxnet()

# glmnet call stack for censored regression using `multi_predict(type = "survival")` when object has
# classes "_coxnet" and "model_fit":
#
# 	multi_predict()
#    multi_predict._coxnet(penalty = NULL)
#      predict._coxnet(multi = TRUE)          <-- checks and sets penalty
#       predict.model_fit()                  <-- checks for extra vars in ...
#        predict_survival()
#         predict_survival._coxnet()
#          predict_survival.model_fit()
#           survival_prob_coxnet()

#' @export
predict._coxnet <-
  function(object, new_data, type = NULL, opts = list(), penalty = NULL, multi = FALSE, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    }

    object$spec$args$penalty <- parsnip::.check_glmnet_penalty_predict(penalty, object, multi)

    object$spec <- eval_args(object$spec)
    predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
  }

#' @export
predict_survival._coxnet <- function(object, new_data, ...) {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  NextMethod()
}

#' @export
predict_raw._coxnet <- function(object, new_data, opts = list(), ...)  {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  opts$s <- object$spec$args$penalty
  NextMethod()
}

#' @export
multi_predict._coxnet <- function(object,
                                  new_data,
                                  type = NULL,
                                  penalty = NULL,
                                  ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    dots <- list(...)

    object$spec <- eval_args(object$spec)

    if (is.null(penalty)) {
      # See discussion in https://github.com/tidymodels/parsnip/issues/195
      if (!is.null(object$spec$args$penalty)) {
        penalty <- object$spec$args$penalty
      } else {
        penalty <- object$fit$lambda
      }
    }

    if (type == "linear_pred"){
      pred <- multi_predict_coxnet_linear_pred(object, new_data = new_data,
                                               opts = dots, penalty = penalty)
    } else {
      pred <- predict(object, new_data = new_data, type = type, ...,
                      penalty = penalty, multi = TRUE)
    }

    pred
}

multi_predict_coxnet_linear_pred <- function(object, new_data, opts, penalty) {
  pred <- predict(object, new_data = new_data, type = "raw",
                  opts = opts, penalty = penalty, multi = TRUE)

  # post-processing into nested tibble
  param_key <- tibble(group = colnames(pred), penalty = penalty)
  pred <- pred %>%
    as_tibble() %>%
    dplyr::mutate(.row = seq_len(nrow(pred))) %>%
    tidyr::pivot_longer(
      - .row,
      names_to = "group",
      values_to = ".pred_linear_pred"
    )
  pred <- dplyr::inner_join(param_key, pred, by = "group") %>%
    dplyr::select(-group) %>%
    dplyr::arrange(.row, penalty) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)
}

#' @export
print._coxnet <- function(x, ...) {
  cat("parsnip model object\n\n")
  cat("Fit time: ", prettyunits::pretty_sec(x$elapsed[["elapsed"]]), "\n")

  if (inherits(x$fit$fit, "try-error")) {
    cat("Model fit failed with error:\n", x$fit, "\n")
  } else {
    print(x$fit, ...)
    cat("The training data has been saved for prediction.\n")
  }
  invisible(x)
}
