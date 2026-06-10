# These functions define the `null_model` engine for censored regression. They
# are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_null_model_censored <- function() {
  parsnip::set_model_mode("null_model", "censored regression")
  parsnip::set_model_engine(
    "null_model",
    mode = "censored regression",
    eng = "censored"
  )
  parsnip::set_dependency(
    "null_model",
    eng = "censored",
    pkg = "survival",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "null_model",
    eng = "censored",
    pkg = "censored",
    mode = "censored regression"
  )

  parsnip::set_fit(
    model = "null_model",
    eng = "censored",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "censored", fun = "survfit_null"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "null_model",
    eng = "censored",
    mode = "censored regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "null_model",
    eng = "censored",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_time_survfit_null"),
      args = list(
        object = expr(object),
        new_data = expr(new_data)
      )
    )
  )

  parsnip::set_pred(
    model = "null_model",
    eng = "censored",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_survfit_null"),
      args = list(
        object = expr(object),
        new_data = expr(new_data),
        eval_time = expr(eval_time)
      )
    )
  )
}

# nocov end
