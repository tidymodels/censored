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

#' @export
predict_linear_pred._coxph <- function(object,
                                       new_data,
                                       ...,
                                       increasing = TRUE) {
  res <- NextMethod()
  if (increasing) {
    # For consistency with other models, we want the lp to increase with
    # time. For this, we change the sign
    res <- -res
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
predict_linear_pred._coxnet <- function(object,
                                        new_data,
                                        ...,
                                        increasing = TRUE) {
  res <- NextMethod()
  if (increasing) {
    # For consistency with other models, we want the lp to increase with
    # time. For this, we change the sign
    res <- -res
  }
  res
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
