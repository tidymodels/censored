# These functions define the Cox regression models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_boost_tree_mboost <- function() {
  parsnip::set_model_engine("boost_tree", mode = "censored regression", eng = "mboost")
  parsnip::set_dependency("boost_tree",
                          eng = "mboost",
                          pkg = "mboost",
                          mode = "censored regression")
  parsnip::set_dependency("boost_tree",
                          eng = "mboost",
                          pkg = "censored",
                          mode = "censored regression")

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "trees",
    original = "mstop",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "loss_reduction",
    original = "mincriterion",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "boost_tree",
    eng = "mboost",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "censored", fun = "blackboost_train"),
      defaults = list(family = expr(mboost::CoxPH()))
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    eng = "mboost",
    mode = "censored regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "mboost",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = function(x, object) {
        time <- object$spec$method$pred$survival$args$time
        res <- floor_surv_mboost(x, time)
        matrix_to_nested_tibbles_survival(res, time)
      },
      func = c(pkg = "mboost", fun = "survFit"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "mboost",
    mode = "censored regression",
    type = "linear_pred",
    value = list(
      pre = NULL,
      post = as.numeric,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "mboost",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_time_mboost"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data)
        )
    )
  )

}

# nocov end
