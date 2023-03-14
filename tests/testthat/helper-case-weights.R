expect_unequal <- function(object,
                           expected,
                           ...,
                           tolerance = if (edition_get() >= 3) testthat_tolerance()) {
  expect_true(!compare(object, expected, tolerance = tolerance, ...)$equal)
}

make_cens_wts <- function() {
  data(time_to_million, package = "censored", envir = rlang::current_env())

  set.seed(1)
  time_to_million <- time_to_million[1:100, c("time", "event", "released_theaters", "rated")]
  wts <- runif(nrow(time_to_million))
  wts <- ifelse(wts < 1 / 5, 0, 1)
  cens_subset <- time_to_million[wts != 0, ]
  wts <- importance_weights(wts)

  list(wts = wts, subset = cens_subset, full = time_to_million)
}
