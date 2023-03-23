# These functions define the Random Forest models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_rand_forest_partykit <- function() {
  parsnip::set_model_engine(
    "rand_forest",
    mode = "censored regression",
    eng = "partykit"
  )
  parsnip::set_dependency(
    "rand_forest",
    eng = "partykit",
    pkg = "partykit",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "rand_forest",
    eng = "partykit",
    pkg = "modeltools",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "rand_forest",
    eng = "partykit",
    pkg = "censored",
    mode = "censored regression"
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "trees",
    original = "ntree",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "partykit",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "partykit",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "parsnip", fun = "cforest_train"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    mode = "censored regression",
    eng = "partykit",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "partykit",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        type = "response"
      )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "partykit",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_partykit"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data),
        eval_time = rlang::expr(eval_time)
      )
    )
  )
}

make_rand_forest_aorsf <- function() {
  parsnip::set_model_engine(
    "rand_forest",
    mode = "censored regression",
    eng = "aorsf"
  )
  parsnip::set_dependency(
    "rand_forest",
    eng = "aorsf",
    pkg = "aorsf",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "rand_forest",
    eng = "aorsf",
    pkg = "censored",
    mode = "censored regression"
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "aorsf",
    parsnip = "trees",
    original = "n_tree",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "aorsf",
    parsnip = "min_n",
    original = "leaf_min_obs",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "aorsf",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "aorsf",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "aorsf", fun = "orsf"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    mode = "censored regression",
    eng = "aorsf",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "aorsf",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_orsf"),
      args = list(
        object = rlang::expr(object$fit),
        new_data = rlang::expr(new_data),
        eval_time = rlang::expr(eval_time)
      )
    )
  )
}

# nocov end
