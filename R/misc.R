# This function takes a matrix and turns it into list of nested tibbles
# suitable for predict_survival
matrix_to_nested_tibbles_survival <- function(x, time) {

  res <- tibble(
    .row = rep(seq_len(nrow(x)), each = ncol(x)),
    .time = rep(time, nrow(x)),
    .pred_survival = as.numeric(t(x))
  )

  dplyr::group_nest(res, .row, .key = ".pred")$.pred
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
