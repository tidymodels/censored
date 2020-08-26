# These functions define the Survival Regression models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_surv_reg_survival <- function() {
  parsnip::set_new_model("surv_reg")
  parsnip::set_model_mode("surv_reg", "regression")

  # ------------------------------------------------------------------------------
  parsnip::set_model_engine("surv_reg", mode = "regression", eng = "survival")
  parsnip::set_dependency("surv_reg", eng = "survival", pkg = "survival")

  parsnip::set_model_arg(
    model = "surv_reg",
    eng = "survival",
    parsnip = "dist",
    original = "dist",
    func = list(pkg = "dials", fun = "dist"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "surv_reg",
    eng = "survival",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "survival", fun = "survreg"),
      defaults = list(model = TRUE)
    )
  )

  parsnip::set_encoding(
    model = "surv_reg",
    eng = "survival",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  parsnip::set_pred(
    model = "surv_reg",
    eng = "survival",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "response"
        )
    )
  )

  parsnip::set_pred(
    model = "surv_reg",
    eng = "survival",
    mode = "regression",
    type = "quantile",
    value = list(
      pre = NULL,
      post = survreg_quant,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "quantile",
          p = expr(quantile)
        )
    )
  )
}

make_surv_reg_flexsurv <- function() {
  parsnip::set_model_engine("surv_reg", mode = "regression", eng = "flexsurv")
  parsnip::set_dependency("surv_reg", eng = "flexsurv", pkg = "flexsurv")
  parsnip::set_dependency("surv_reg", eng = "flexsurv", pkg = "survival")

  parsnip::set_model_arg(
    model = "surv_reg",
    eng = "flexsurv",
    parsnip = "dist",
    original = "dist",
    func = list(pkg = "dials", fun = "dist"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "surv_reg",
    eng = "flexsurv",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "flexsurv", fun = "flexsurvreg"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "surv_reg",
    eng = "flexsurv",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  parsnip::set_pred(
    model = "surv_reg",
    eng = "flexsurv",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = flexsurv_mean,
      func = c(fun = "summary"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "mean"
        )
    )
  )

  parsnip::set_pred(
    model = "surv_reg",
    eng = "flexsurv",
    mode = "regression",
    type = "quantile",
    value = list(
      pre = NULL,
      post = flexsurv_quant,
      func = c(fun = "summary"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "quantile",
          quantiles = expr(quantile)
        )
    )
  )
}

# nocov end
