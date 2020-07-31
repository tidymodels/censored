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
  parsnip::set_model_mode("cox_reg", "risk prediction")

  parsnip::set_model_engine("cox_reg", mode = "risk prediction", eng = "survival")
  parsnip::set_dependency("cox_reg", eng = "survival", pkg =  "survival")

  parsnip::set_fit(
    model = "cox_reg",
    eng = "survival",
    mode = "risk prediction",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "survival", fun = "coxph"),
      defaults = list()
    )
  )

  #parsnip::set_pred(
  #  model = "cox_reg",
  #  eng = "survival",
  #  mode = "risk prediction",
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

make_cox_reg_glmnet <- function() {
  parsnip::set_model_engine("cox_reg", mode = "risk prediction", eng = "glmnet")
  parsnip::set_dependency("cox_reg", eng = "glmnet", pkg =  "glmnet")

  parsnip::set_fit(
    model = "cox_reg",
    eng = "glmnet",
    mode = "risk prediction",
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
    mode = "risk prediction",
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
  #  mode = "risk prediction",
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
