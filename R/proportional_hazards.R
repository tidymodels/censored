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

  # TODO declare specials here? (only for glmnet or for all?)

  # call parsnip::fit.model_spec()
  res <- NextMethod()

  # TODO find a public place to document that
  # For coxnet models, we need the training data for prediction. It's being
  # returned by fit.model_spec() in the $fit slot. Since we want $fit to only
  # contain the fitted model object, we move the training one level up into the
  # parsnip object.
  # For stratification, glmnet requires that we stratify the response. Having
  # the strata variable on the left-hand side of the formula would mean it's not
  # available for prediction so we are requiring the strata specification to be
  # part of the right-hand side of the formula and do the translation between
  # formula and glmnet's matrix interface manually. To be able to do this also
  # at prediction time, we save the original formula and the pre-processing
  # results.
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

#' @export
translate.proportional_hazards <- function(x, engine = x$engine, ...) {
  x <- translate.default(x, engine, ...)

  if (engine == "glmnet") {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    x$method$fit$args$lambda <- NULL
    # Since the `fit` information is gone for the penalty, we need to have an
    # evaluated value for the parameter.
    x$args$penalty <- rlang::eval_tidy(x$args$penalty)
    check_glmnet_penalty(x)
  }

  x
}


# ------------------------------------------------------------------------------

# copy of the unexported parsnip:::organize_glmnet_pred():
organize_glmnet_pred <- function(x, object) {
  if (ncol(x) == 1) {
    res <- x[, 1]
    res <- unname(res)
  } else {
    n <- nrow(x)
    res <- utils::stack(as.data.frame(x))
    if (!is.null(object$spec$args$penalty))
      res$lambda <- rep(object$spec$args$penalty, each = n) else
        res$lambda <- rep(object$fit$lambda, each = n)
    res <- res[, colnames(res) %in% c("values", "lambda")]
  }
  res
}

# ------------------------------------------------------------------------------

# copy of the unexported parsnip:::check_penalty():

# For `predict` methods that use `glmnet`, we have specific methods.
# Only one value of the penalty should be allowed when called by `predict()`:

check_penalty <- function(penalty = NULL, object, multi = FALSE) {

  if (is.null(penalty)) {
    penalty <- object$fit$lambda
  }

  # when using `predict()`, allow for a single lambda
  if (!multi) {
    if (length(penalty) != 1)
      rlang::abort(
        glue::glue(
          "`penalty` should be a single numeric value. `multi_predict()` ",
          "can be used to get multiple predictions per row of data.",
        )
      )
  }

  if (length(object$fit$lambda) == 1 && penalty != object$fit$lambda)
    rlang::abort(
      glue::glue(
        "The glmnet model was fit with a single penalty value of ",
        "{object$fit$lambda}. Predicting with a value of {penalty} ",
        "will give incorrect results from `glmnet()`."
      )
    )

  penalty
}

# copy of the unexported parsnip:::check_glmnet_penalty():
check_glmnet_penalty <- function(x) {
  if (length(x$args$penalty) != 1) {
    rlang::abort(c(
      "For the glmnet engine, `penalty` must be a single number (or a value of `tune()`).",
      glue::glue("There are {length(x$args$penalty)} values for `penalty`."),
      "To try multiple values for total regularization, use the tune package.",
      "To predict multiple penalties, use `multi_predict()`"
    ))
  }
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
#        coxnet_survival_prob()

#' @export
predict._coxnet <-
  function(object, new_data, type = NULL, opts = list(), penalty = NULL, multi = FALSE, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    }

    object$spec$args$penalty <- check_penalty(penalty, object, multi)

    object$spec <- eval_args(object$spec)
    predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
  }

#' @export
predict_survival._coxnet <- function(object, new_data, ...) {
  if (any(names(enquos(...)) == "newdata"))
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

  object$spec <- eval_args(object$spec)
  predict_survival.model_fit(object, new_data = new_data, ...)
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
