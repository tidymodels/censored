# These functions define the discriminant analysis models. They are executed when
# this package is loaded via `.onLoad()` and modify the parsnip package's
# model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_cox_reg_survival <- function() {
  parsnip::set_new_model("cox_reg")
  parsnip::set_model_mode("cox_reg", "censored regression")

  parsnip::set_model_engine("cox_reg", mode = "censored regression", eng = "survival")
  parsnip::set_dependency("cox_reg", eng = "survival", pkg =  "survival")

  parsnip::set_fit(
    model = "cox_reg",
    eng = "survival",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "survival", fun = "coxph"),
      defaults = list(x = TRUE)
    )
  )

  set_encoding(
    model = "cox_reg",
    eng = "survival",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  parsnip::set_pred(
    model = "cox_reg",
    eng = "survival",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = function(x, object) {
        unname(summary(x)$table[, "*rmean"])
      },
      func = c(fun = "survfit"),
      args =
        list(
          formula = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "cox_reg",
    eng = "survival",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "pec", fun = "predictSurvProb"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          times = rlang::expr(.time)
        )
    )
  )
}

make_cox_reg_glmnet <- function() {
  parsnip::set_model_engine("cox_reg", mode = "censored regression", eng = "glmnet")
  parsnip::set_dependency("cox_reg", eng = "glmnet", pkg =  "glmnet")

  parsnip::set_fit(
    model = "cox_reg",
    eng = "glmnet",
    mode = "censored regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      func = c(pkg = "glmnet", fun = "glmnet"),
      defaults = list(family = "cox")
    )
  )

  set_encoding(
    model = "cox_reg",
    eng = "glmnet",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE
    )
  )

  set_model_arg(
    model = "cox_reg",
    eng = "glmnet",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = TRUE
  )

  set_model_arg(
    model = "cox_reg",
    eng = "glmnet",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )

  #parsnip::set_pred(
  #  model = "cox_reg",
  #  eng = "survival",
  #  mode = "censored regression",
  #  type = "survival",
  #  value = list(
  #    pre = NULL,
  #    post = NULL,
  #    func = c(fun = "predict"),
  #    args =
  #      list(
  #        object = quote(object$fit),
  #        newdata = quote(new_data)
  #      )
  #  )
  #)
}



# nocov end
