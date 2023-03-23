# These functions define the proportional hazards models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_proportional_hazards_survival <- function() {
  parsnip::set_model_engine(
    "proportional_hazards",
    mode = "censored regression",
    eng = "survival"
  )
  parsnip::set_dependency(
    "proportional_hazards",
    eng = "survival",
    pkg = "survival",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "proportional_hazards",
    eng = "survival",
    pkg = "censored",
    mode = "censored regression"
  )

  parsnip::set_fit(
    model = "proportional_hazards",
    eng = "survival",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "survival", fun = "coxph"),
      defaults = list(x = TRUE, model = TRUE)
    )
  )

  parsnip::set_encoding(
    model = "proportional_hazards",
    eng = "survival",
    mode = "censored regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "proportional_hazards",
    eng = "survival",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = cph_survival_pre,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_time_coxph"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "proportional_hazards",
    eng = "survival",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = cph_survival_pre,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_coxph"),
      args =
        list(
          x = quote(object$fit),
          new_data = quote(new_data),
          eval_time = rlang::expr(eval_time),
          output = "surv",
          interval = expr(interval),
          conf.int = expr(level)
        )
    )
  )

  parsnip::set_pred(
    model = "proportional_hazards",
    eng = "survival",
    mode = "censored regression",
    type = "linear_pred",
    value = list(
      pre = NULL,
      post = function(x, object) {
        unname(x)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          na.action = quote(stats::na.exclude),
          reference = "zero"
        )
    )
  )
}

make_proportional_hazards_glmnet <- function() {
  parsnip::set_model_engine(
    "proportional_hazards",
    mode = "censored regression",
    eng = "glmnet"
  )
  parsnip::set_dependency(
    "proportional_hazards",
    eng = "glmnet",
    pkg = "glmnet",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "proportional_hazards",
    eng = "glmnet",
    pkg = "censored",
    mode = "censored regression"
  )

  parsnip::set_fit(
    model = "proportional_hazards",
    eng = "glmnet",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "censored", fun = "coxnet_train"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "proportional_hazards",
    eng = "glmnet",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = TRUE
    )
  )

  parsnip::set_model_arg(
    model = "proportional_hazards",
    eng = "glmnet",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "proportional_hazards",
    eng = "glmnet",
    parsnip = "mixture",
    original = "alpha",
    func = list(pkg = "dials", fun = "mixture"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "proportional_hazards",
    eng = "glmnet",
    mode = "censored regression",
    type = "linear_pred",
    value = list(
      pre = coxnet_prepare_x,
      post = parsnip::.organize_glmnet_pred,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit$fit),
          newx = expr(new_data),
          type = "link",
          s = expr(object$spec$args$penalty)
        )
    )
  )

  parsnip::set_pred(
    model = "proportional_hazards",
    eng = "glmnet",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_coxnet"),
      args =
        list(
          object = expr(object),
          new_data = expr(new_data),
          eval_time = expr(eval_time),
          penalty = expr(object$spec$args$penalty)
        )
    )
  )

  parsnip::set_pred(
    model = "proportional_hazards",
    eng = "glmnet",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_time_coxnet"),
      args =
        list(
          object = quote(object),
          new_data = quote(new_data),
          penalty = expr(object$spec$args$penalty)
        )
    )
  )

  parsnip::set_pred(
    model = "proportional_hazards",
    eng = "glmnet",
    mode = "censored regression",
    type = "raw",
    value = list(
      pre = coxnet_prepare_x,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit$fit),
          newx = expr(new_data)
        )
    )
  )
}

# nocov end
