# These functions define the Bagged Decision Tree Models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_bag_tree_rpart <- function() {
  parsnip::set_model_engine("bag_tree", mode = "censored regression", eng = "rpart")
  parsnip::set_dependency("bag_tree",
                          eng = "rpart",
                          pkg =  "ipred",
                          mode = "censored regression")
  parsnip::set_dependency("bag_tree",
                          eng = "rpart",
                          pkg = "censored",
                          mode = "censored regression")

  parsnip::set_fit(
    model = "bag_tree",
    eng = "rpart",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "ipred", fun = "bagging"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "bag_tree",
    eng = "rpart",
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
    eng = "rpart",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_time_survbagg"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "bag_tree",
    eng = "rpart",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_survbagg"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data),
          time = rlang::expr(time)
        )
    )
  )
}

# nocov end
