# utilities copied from parnsip

pred_types <- c(
  "raw",
  "numeric",
  "class",
  "prob",
  "conf_int",
  "pred_int",
  "quantile",
  "time",
  "survival",
  "linear_pred",
  "hazard"
)

# used directly, probably export?
check_pred_type <- function(object, type, ...) {
  if (is.null(type)) {
    type <-
      switch(
        object$spec$mode,
        regression = "numeric",
        classification = "class",
        "censored regression" = "time",
        rlang::abort(
          "`type` should be 'regression', 'censored regression', or 'classification'."
        )
      )
  }
  if (!(type %in% pred_types)) {
    rlang::abort(
      glue::glue(
        "`type` should be one of: ",
        glue::glue_collapse(pred_types, sep = ", ", last = " and ")
      )
    )
  }

  switch(
    type,
    "numeric" = if (object$spec$mode != "regression") {
      rlang::abort(
        "For numeric predictions, the object should be a regression model."
      )
    },
    "class" = if (object$spec$mode != "classification") {
      rlang::abort(
        "For class predictions, the object should be a classification model."
      )
    },
    "prob" = if (object$spec$mode != "classification") {
      rlang::abort(
        "For probability predictions, the object should be a classification model."
      )
    },
    "time" = if (object$spec$mode != "censored regression") {
      rlang::abort(
        "For event time predictions, the object should be a censored regression."
      )
    },
    "survival" = if (object$spec$mode != "censored regression") {
      rlang::abort(
        "For survival probability predictions, the object should be a censored regression."
      )
    },
    "hazard" = if (object$spec$mode != "censored regression") {
      rlang::abort(
        "For hazard predictions, the object should be a censored regression."
      )
    },
    "linear_pred" = if (object$spec$mode != "censored regression") {
      rlang::abort(
        "For the linear predictor, the object should be a censored regression."
      )
    }
  )

  # TODO check for ... options when not the correct type
  type
}

# used directly, maybe export?
check_spec_pred_type <- function(object, type, call = caller_env()) {
  if (!spec_has_pred_type(object, type)) {
    possible_preds <- names(object$spec$method$pred)
    cli::cli_abort(
      "No {.val {type}} prediction method available for this model. \\
      {.arg type} should be one of: {.val {possible_preds}}.",
      call = call
    )
  }
  invisible(NULL)
}

spec_has_pred_type <- function(object, type) {
  possible_preds <- names(object$spec$method$pred)
  any(possible_preds == type)
}

# used directly, probably export
check_pred_type_dots <- function(
  object,
  type,
  ...,
  call = rlang::caller_env()
) {
  the_dots <- list(...)
  nms <- names(the_dots)

  # ----------------------------------------------------------------------------

  check_for_newdata(..., call = call)

  # ----------------------------------------------------------------------------

  other_args <- c(
    "interval",
    "level",
    "std_error",
    "quantile_levels",
    "time",
    "eval_time",
    "increasing",
    "add_censoring_weights"
  )

  eval_time_types <- c("survival", "hazard")

  is_pred_arg <- names(the_dots) %in% other_args
  if (!all(is_pred_arg)) {
    bad_args <- names(the_dots)[!is_pred_arg]
    bad_args <- paste0("`", bad_args, "`", collapse = ", ")
    cli::cli_abort(
      "The ellipses are not used to pass args to the model function's
         predict function. These arguments cannot be used: {.val bad_args}",
      call = call
    )
  }

  # ----------------------------------------------------------------------------
  # places where eval_time should not be given
  if (any(nms == "eval_time") & !type %in% c("survival", "hazard")) {
    cli::cli_abort(
      "{.arg eval_time} should only be passed to {.fn predict} when \\
       {.arg type} is one of {.or {.val {eval_time_types}}}.",
      call = call
    )
  }
  if (any(nms == "time") & !type %in% c("survival", "hazard")) {
    cli::cli_abort(
      "{.arg time} should only be passed to {.fn predict} when {.arg type} is
       one of {.or {.val {eval_time_types}}}.",
      call = call
    )
  }
  # when eval_time should be passed
  if (
    !any(nms %in% c("eval_time", "time")) & type %in% c("survival", "hazard")
  ) {
    cli::cli_abort(
      "When using {.arg type} values of {.or {.val {eval_time_types}}} a numeric
     vector {.arg eval_time} should also be given.",
      call = call
    )
  }

  # `increasing` only applies to linear_pred for censored regression
  if (
    any(nms == "increasing") &
      !(type == "linear_pred" &
        object$spec$mode == "censored regression")
  ) {
    cli::cli_abort(
      "{.arg increasing} only applies to predictions of
       type 'linear_pred' for the mode censored regression.",
      call = call
    )
  }

  # `add_censoring_weights` only applies to survival predictions
  if (any(nms == "add_censoring_weights") & type != "survival") {
    cli::cli_abort(
      "{.arg add_censoring_weights} should only be passed to {.fn predict}
       when {.arg type} is {.val survival}.",
      call = call
    )
  }

  invisible(TRUE)
}

check_for_newdata <- function(..., call = rlang::caller_env()) {
  if (any(names(list(...)) == "newdata")) {
    rlang::abort(
      "Please use `new_data` instead of `newdata`.",
      call = call
    )
  }
}

# used directly, maybe export?
check_installs <- function(x) {
  if (length(x$method$libs) > 0) {
    is_inst <- purrr::map_lgl(x$method$libs, is_installed)
    if (any(!is_inst)) {
      missing_pkg <- x$method$libs[!is_inst]
      missing_pkg <- paste0(missing_pkg, collapse = ", ")
      rlang::abort(
        glue::glue(
          "This engine requires some package installs: ",
          glue::glue_collapse(glue::glue("'{missing_pkg}'"), sep = ", ")
        )
      )
    }
  }
}

shhhh <- function(x) {
  suppressPackageStartupMessages(requireNamespace(x, quietly = TRUE))
}

is_installed <- function(pkg) {
  res <- try(shhhh(pkg), silent = TRUE)
  res
}

# used directly, maybe export?
load_libs <- function(x, quiet, attach = FALSE) {
  for (pkg in x$method$libs) {
    if (!attach) {
      suppressPackageStartupMessages(requireNamespace(pkg, quietly = quiet))
    } else {
      library(pkg, character.only = TRUE, quietly = quiet)
    }
  }
  invisible(x)
}
