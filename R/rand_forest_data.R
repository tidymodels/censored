# These functions define the Random Forest models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_rand_forest_party <- function() {
  parsnip::set_model_mode("rand_forest", "censored regression")

  parsnip::set_model_engine("rand_forest", mode = "censored regression", eng = "party")
  parsnip::set_dependency("rand_forest", eng = "party", pkg = "pec")
  parsnip::set_dependency("rand_forest", eng = "party", pkg = "party")

  parsnip::set_fit(
    model = "rand_forest",
    eng = "party",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "pec", fun = "pecCforest"),
      defaults = list()
    )
  )

  set_encoding(
    model = "rand_forest",
    eng = "party",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE
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
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit$forest),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "party",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = function(x, object) {
        x <- x[, -1, drop = FALSE]
        colnames(x) <- object$spec$method$pred$survival$args$.time
        matrix_to_nested_tibbles_survival(x)
      },
      func = c(pkg = "pec", fun = "predictSurvProb"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          times = rlang::expr(pmin(c(0, .time), max(object$fit$forest@responses@variables)))
        )
    )
  )
}

# nocov end
