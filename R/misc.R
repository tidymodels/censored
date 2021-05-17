# This function takes a matrix and turns it into list of nested tibbles
# suitable for predict_survival
matrix_to_nested_tibbles_survival <- function(x, time) {

  res <- tibble(
    .row = rep(seq_len(nrow(x)), each = ncol(x)),
    .time = rep(time, nrow(x)),
    .pred_survival = as.numeric(t(x))
  )

  group_nest(res, .row, .key = ".pred")$.pred
}

# ------------------------------------------------------------------------------

# copied form recipes

names0 <- function(num, prefix = "x") {
  if (num < 1)
    rlang::abort("`num` should be > 0.")
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

# In some cases, the test value needs to be wrapped in an empty
#  environment. If arguments are set in the model specification
# (as opposed to being set by a `translate` function), they will
# need this wrapper.

new_empty_quosure <- function(expr) {
  new_quosure(expr, env = empty_env())
}
