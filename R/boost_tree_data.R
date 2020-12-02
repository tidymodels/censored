# These functions define the Cox regression models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_boost_tree_mboost <- function() {
  parsnip::set_model_mode("boost_tree", "censored regression")

  parsnip::set_model_engine("boost_tree", mode = "censored regression", eng = "mboost")
  parsnip::set_dependency("boost_tree", eng = "mboost", pkg = "mboost")

  set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "trees",
    original = "mstop",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "boost_tree",
    eng = "mboost",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  set_model_arg(
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
      protect = c("formula", "data"),
      func = c(pkg = "survnip", fun = "blackboost_train"),
      defaults = list(family = mboost::CoxPH())
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
        res <- floor_surv_mboost(
          x,
          object$spec$method$pred$survival$args$.time
        )
        colnames(res) <- object$spec$method$pred$survival$args$.time

        matrix_to_nested_tibbles_survival(res)
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
}

# nocov end

# the mboost::survFit isn't able to predict survival probabilities for a given
# timepoint. This function rounds down to nearest timepoint and uses that
# for prediction.
floor_surv_mboost <- function(x, .time) {
  ind <- purrr::map_int(.time, ~ max(which(.x > c(-Inf, unname(x$time)))))
  t(unname(rbind(1, x$surv))[ind, ])
}
