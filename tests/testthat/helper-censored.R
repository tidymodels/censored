# a helper to sanitize the environment of quosures so that
# expect_snapshot results are reproducible
clear_quosure_environment <- function(x) {
  if (rlang::is_quosure(x)) {
    x <- rlang::quo_set_env(x, rlang::empty_env())
  }

  x
}

# a helper to express the idiom of translating, subsetting out the
# generated args, and snapshotting them
translate_args <- function(x) {
  x %>%
    translate() %>%
    purrr::pluck("method", "fit", "args") %>%
    purrr::map(clear_quosure_environment)
}
