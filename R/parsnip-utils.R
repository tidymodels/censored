# utilities copied from parnsip

pred_types <- c(
  "raw", "numeric", "class", "prob", "conf_int", "pred_int", "quantile",
  "time", "survival", "linear_pred", "hazard"
)

# used directly, probably export?
check_pred_type <- function(object, type, ...) {
  if (is.null(type)) {
    type <-
      switch(object$spec$mode,
        regression = "numeric",
        classification = "class",
        "censored regression" = "time",
        rlang::abort("`type` should be 'regression', 'censored regression', or 'classification'.")
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

  switch(type,
    "numeric" = if (object$spec$mode != "regression") {
      rlang::abort("For numeric predictions, the object should be a regression model.")
    },
    "class" = if (object$spec$mode != "classification") {
      rlang::abort("For class predictions, the object should be a classification model.")
    },
    "prob" = if (object$spec$mode != "classification") {
      rlang::abort("For probability predictions, the object should be a classification model.")
    },
    "time" = if (object$spec$mode != "censored regression") {
      rlang::abort("For event time predictions, the object should be a censored regression.")
    },
    "survival" = if (object$spec$mode != "censored regression") {
      rlang::abort("For survival probability predictions, the object should be a censored regression.")
    },
    "hazard" = if (object$spec$mode != "censored regression") {
      rlang::abort("For hazard predictions, the object should be a censored regression.")
    },
    "linear_pred" = if (object$spec$mode != "censored regression") {
      rlang::abort("For the linear predictor, the object should be a censored regression.")
    }
  )

  # TODO check for ... options when not the correct type
  type
}

# used directly, maybe export?
check_spec_pred_type <- function(object, type) {
  if (!spec_has_pred_type(object, type)) {
    possible_preds <- names(object$spec$method$pred)
    rlang::abort(c(
      glue::glue("No {type} prediction method available for this model."),
      glue::glue(
        "Value for `type` should be one of: ",
        glue::glue_collapse(glue::glue("'{possible_preds}'"), sep = ", ")
      )
    ))
  }
  invisible(NULL)
}

spec_has_pred_type <- function(object, type) {
  possible_preds <- names(object$spec$method$pred)
  any(possible_preds == type)
}

# used directly, probably export
check_pred_type_dots <- function(object, type, ..., call = rlang::caller_env()) {
  the_dots <- list(...)
  nms <- names(the_dots)

  # ----------------------------------------------------------------------------

  check_for_newdata(..., call = call)

  # ----------------------------------------------------------------------------

  other_args <- c(
    "interval", "level", "std_error", "quantile",
    "time", "eval_time", "increasing"
  )
  is_pred_arg <- names(the_dots) %in% other_args
  if (any(!is_pred_arg)) {
    bad_args <- names(the_dots)[!is_pred_arg]
    bad_args <- paste0("`", bad_args, "`", collapse = ", ")
    rlang::abort(
      glue::glue(
        "The ellipses are not used to pass args to the model function's ",
        "predict function. These arguments cannot be used: {bad_args}",
      )
    )
  }

  # ----------------------------------------------------------------------------
  # places where eval_time should not be given
  if (any(nms == "eval_time") & !type %in% c("survival", "hazard")) {
    rlang::abort(
      paste(
        "`eval_time` should only be passed to `predict()` when `type` is one of:",
        paste0("'", c("survival", "hazard"), "'", collapse = ", ")
      )
    )
  }
  if (any(nms == "time") & !type %in% c("survival", "hazard")) {
    rlang::abort(
      paste(
        "'time' should only be passed to `predict()` when 'type' is one of:",
        paste0("'", c("survival", "hazard"), "'", collapse = ", ")
      )
    )
  }
  # when eval_time should be passed
  if (!any(nms %in% c("eval_time", "time")) & type %in% c("survival", "hazard")) {
    rlang::abort(
      paste(
        "When using `type` values of 'survival' or 'hazard',",
        "a numeric vector `eval_time` should also be given."
      )
    )
  }

  # `increasing` only applies to linear_pred for censored regression
  if (any(nms == "increasing") &
    !(type == "linear_pred" &
      object$spec$mode == "censored regression")) {
    rlang::abort(
      paste(
        "The 'increasing' argument only applies to predictions of",
        "type 'linear_pred' for the mode censored regression."
      )
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

# used directly, from parsnip's standalone file
.filter_eval_time <- function(eval_time, fail = TRUE) {
  if (!is.null(eval_time)) {
    eval_time <- as.numeric(eval_time)
  }
  eval_time_0 <- eval_time
  # will still propagate nulls:
  eval_time <- eval_time[!is.na(eval_time)]
  eval_time <- eval_time[eval_time >= 0 & is.finite(eval_time)]
  eval_time <- unique(eval_time)
  if (fail && identical(eval_time, numeric(0))) {
    cli::cli_abort(
      "There were no usable evaluation times (finite, non-missing, and >= 0).",
      call = NULL
    )
  }
  if (!identical(eval_time, eval_time_0)) {
    diffs <- length(eval_time_0) - length(eval_time)

    offenders <- character()

    n_na <- sum(is.na(eval_time_0))
    if (n_na > 0) {
      offenders <- c(offenders, "*" = "{n_na} missing value{?s}.")
    }

    n_inf <- sum(is.infinite(eval_time_0))
    if (n_inf > 0) {
      offenders <- c(offenders, "*" = "{n_inf} infinite value{?s}.")
    }

    n_neg <- sum(eval_time_0 < 0, na.rm = TRUE)
    if (n_neg > 0) {
      offenders <- c(offenders, "*" = "{n_neg} negative value{?s}.")
    }

    n_dup <- diffs - n_na - n_inf - n_neg
    if (n_dup > 0) {
      offenders <- c(offenders, "*" = "{n_dup} duplicate value{?s}.")
    }

    cli::cli_warn(
      c(
        "There {?was/were} {diffs} inappropriate evaluation time \\
        point{?s} that {?was/were} removed. {?It was/They were}:",
        offenders
      ),
      call = NULL
    )
  }
  eval_time
}
