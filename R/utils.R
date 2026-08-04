check_inherits <- function(
  x,
  class,
  ...,
  allow_null = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!missing(x)) {
    if (inherits(x, class)) {
      return(invisible(NULL))
    }
    if (allow_null && rlang::is_null(x)) {
      return(invisible(NULL))
    }
  }

  what <- paste0("a ", oxford_comma(paste0("<", class, ">")), " object")
  rlang::stop_input_type(
    x,
    what,
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_eval_time <- function(
  x,
  ...,
  allow_empty = FALSE,
  allow_missing = FALSE,
  allow_infinite = FALSE,
  allow_negative = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (missing(x) || !is.numeric(x)) {
    rlang::stop_input_type(
      x,
      "a numeric vector",
      ...,
      arg = arg,
      call = call
    )
  }
  if (!allow_empty && length(x) == 0) {
    cli::cli_abort("{.arg {arg}} can't be empty.", call = call)
  }
  if (!allow_missing && anyNA(x)) {
    cli::cli_abort(
      "{.arg {arg}} can't contain missing values.",
      call = call
    )
  }
  if (!allow_infinite && any(is.infinite(x))) {
    cli::cli_abort(
      "{.arg {arg}} can't contain infinite values.",
      call = call
    )
  }
  if (!allow_negative && any(x < 0, na.rm = TRUE)) {
    cli::cli_abort(
      "{.arg {arg}} can't contain negative values.",
      call = call
    )
  }
  invisible(NULL)
}

# `formula` can also be the terms of a fit, e.g. `survreg_fit$terms`.
get_strata <- function(formula, data, xlev = NULL, na.action = stats::na.omit) {
  trms <- stats::terms(formula, specials = "strata", data = data)
  trms <- stats::delete.response(trms)
  mod_frame <- stats::model.frame(
    trms,
    data,
    xlev = xlev,
    na.action = na.action
  )

  strata_cols <- attr(trms, "specials")$strata
  strata <- mod_frame[, strata_cols]

  # There is one column per strata term and only survreg allows more than one
  # term, so this is a no-op for glmnet. For several columns it collapses them
  # into a single factor, labelled to match the names of `survreg_fit$scale`.
  survival::strata(strata, shortlabel = TRUE)
}
