# These functions define the Bagged Decision Tree Models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_bag_tree_ipred <- function() {
  parsnip::set_model_mode("bag_tree", "censored regression")

  parsnip::set_model_engine("bag_tree", mode = "censored regression", eng = "ipred")
  parsnip::set_dependency("bag_tree", eng = "ipred", pkg =  "ipred")
  parsnip::set_dependency("bag_tree", eng = "ipred", pkg = "censored")

  parsnip::set_fit(
    model = "bag_tree",
    eng = "ipred",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "ipred", fun = "bagging"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "bag_tree",
    eng = "ipred",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "bag_tree",
    eng = "ipred",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = function(x, object) {
        map_dbl(x, ~ quantile(.x, probs = .5)$quantile)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "bag_tree",
    eng = "ipred",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = function(x, object) {
        .time <- object$spec$method$pred$survival$args$.time
        res <- map(x, ~ summary(.x, times = pmin(.time, max(.x$time)))$surv)
        res <- matrix(unlist(res), ncol = length(.time), byrow = TRUE)
        colnames(res) <- .time
        matrix_to_nested_tibbles_survival(res)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )
}

# nocov end
