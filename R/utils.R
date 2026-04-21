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
