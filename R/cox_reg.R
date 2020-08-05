#' General Interface for Cox Models
#'
#' `cox_reg()` is a way to generate a _specification_ of a model before
#'  fitting and allows the model to be created using different packages in R.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", or "censored regression".
#'
#' @details
#'
#' For `cox_reg()`, the mode will always be "survival".
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
#' @examples
#' library(survival)
#'
#' cox_mod <-
#'   cox_reg() %>%
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ x, data = aml)
#' @export
cox_reg <-
  function(mode = "censored regression") {
    args <-
      list()

    new_model_spec(
      "cox_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.cox_reg <- function(x, ...) {
  cat("Cox Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @param object A cox model specification.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#'
#' @method update cox_reg
#' @rdname cox_reg
#' @export
update.cox_reg <-
  function(object, fresh = FALSE, ...) {
    update_dot_check(...)
    args <-
      list(
      )

    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    new_model_spec(
      "cox_reg",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }
