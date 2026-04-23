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
