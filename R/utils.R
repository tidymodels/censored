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

  rlang::stop_input_type(
    x,
    paste0("a <", class[[1]], "> object"),
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
