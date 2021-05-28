# These functions define the Random Forest models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_rand_forest_party <- function() {

  parsnip::set_model_engine("rand_forest", mode = "censored regression", eng = "party")
  parsnip::set_dependency("rand_forest", eng = "party", pkg = "party")
  parsnip::set_dependency("rand_forest", eng = "party", pkg = "modeltools")
  parsnip::set_dependency("rand_forest", eng = "party", pkg = "censored")

  parsnip::set_fit(
    model = "rand_forest",
    eng = "party",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "censored", fun = "cond_inference_surv_cforest"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    mode = "censored regression",
    eng = "party",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "party",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "modeltools", fun = "Predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "party",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_cforest"),
      args = list(object = quote(object$fit),
                  new_data = quote(new_data))
    )
  )

  # model args ----------------------------------------------------

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "party",
    parsnip = "trees",
    original = "ntree",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "party",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "party",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

}

# nocov end
