# These functions define the Cox regression models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_boost_tree_xgboost <- function() {
  parsnip::set_model_engine("boost_tree", mode = "censored regression", eng = "xgboost")
  parsnip::set_dependency("boost_tree",
                          eng = "xgboost",
                          pkg = "xgboost",
                          mode = "censored regression")
  parsnip::set_dependency("boost_tree",
                          eng = "xgboost",
                          pkg = "censored",
                          mode = "censored regression")

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost",
    parsnip = "trees",
    original = "nrounds",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost",
    parsnip = "learn_rate",
    original = "eta",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost",
    parsnip = "mtry",
    original = "colsample_bynode",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost",
    parsnip = "min_n",
    original = "min_child_weight",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost",
    parsnip = "loss_reduction",
    original = "gamma",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost",
    parsnip = "sample_size",
    original = "subsample",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost",
    parsnip = "stop_iter",
    original = "early_stop",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )


  parsnip::set_fit(
    model = "boost_tree",
    eng = "xgboost",
    mode = "censored regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      func = c(pkg = "censored", fun = "xgb_train_censored"),
      defaults = list(nthread = 1, verbose = 0)
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    eng = "xgboost",
    mode = "censored regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = TRUE,
      allow_sparse_x = TRUE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "xgboost",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "xgb_pred"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

}



xgb_pred <- function(object, newdata, ...) {
  if (!inherits(newdata, "xgb.DMatrix")) {
    newdata <- maybe_matrix(newdata)
    newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
  }

  res <- predict(object, newdata, ...)

  res
}
