# These functions define the Cox regression models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_boost_tree_xgboost_cox <- function() {
  parsnip::set_model_engine("boost_tree", mode = "censored regression", eng = "xgboost_cox")
  parsnip::set_dependency("boost_tree",
                          eng = "xgboost_cox",
                          pkg = "xgboost",
                          mode = "censored regression")
  parsnip::set_dependency("boost_tree",
                          eng = "xgboost_cox",
                          pkg = "censored",
                          mode = "censored regression")

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost_cox",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost_cox",
    parsnip = "trees",
    original = "nrounds",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost_cox",
    parsnip = "learn_rate",
    original = "eta",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost_cox",
    parsnip = "mtry",
    original = "colsample_bynode",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost_cox",
    parsnip = "min_n",
    original = "min_child_weight",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost_cox",
    parsnip = "loss_reduction",
    original = "gamma",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost_cox",
    parsnip = "sample_size",
    original = "subsample",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "xgboost_cox",
    parsnip = "stop_iter",
    original = "early_stop",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )


  parsnip::set_fit(
    model = "boost_tree",
    eng = "xgboost_cox",
    mode = "censored regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y",'objective'),
      func = c(pkg = "censored", fun = "xgb_train_censored"),
      defaults = list(nthread = 1, verbose = 0,objective = 'survival:cox')
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    eng = "xgboost_cox",
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
    eng = "xgboost_cox",
    mode = "censored regression",
    type = "linear_pred",
    value = list(
      pre = function(x, object) {
        if (object$fit$params$objective != "survival:cox")
          rlang::abort(
            glue::glue(
              "The objective should be survival:cox not {object$fit$params$objective}"
            )
          )
        x
      },
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = expr(object$fit), newdata = expr(new_data))
    )
  )

}
