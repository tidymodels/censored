# These functions define the Decision tree models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_decision_tree_rpart <- function() {
  parsnip::set_model_mode("decision_tree", "censored regression")

  parsnip::set_model_engine("decision_tree", mode = "censored regression", eng = "rpart")
  parsnip::set_dependency("decision_tree", eng = "rpart", pkg =  "rpart")

  parsnip::set_fit(
    model = "decision_tree",
    eng = "rpart",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "rpart", fun = "rpart"),
      defaults = list()
    )
  )

  set_encoding(
    model = "decision_tree",
    eng = "rpart",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  # parsnip::set_pred(
  #   model = "decision_tree",
  #   eng = "survival",
  #   mode = "censored regression",
  #   type = "time",
  #   value = list(
  #     pre = NULL,
  #     post = function(x, object) {
  #       unname(summary(x)$table[, "*rmean"])
  #     },
  #     func = c(fun = "survfit"),
  #     args =
  #       list(
  #         formula = quote(object$fit),
  #         newdata = quote(new_data),
  #         na.action = stats::na.pass
  #       )
  #   )
  # )
  #
  # parsnip::set_pred(
  #   model = "decision_tree",
  #   eng = "survival",
  #   mode = "censored regression",
  #   type = "survival",
  #   value = list(
  #     pre = NULL,
  #     post = function(x, object) {
  #       colnames(x) <- object$spec$method$pred$survival$args$.time
  #       matrix_to_nested_tibbles_survival(x)
  #     },
  #     func = c(pkg = "pec", fun = "predictSurvProb"),
  #     args =
  #       list(
  #         object = quote(object$fit),
  #         newdata = quote(new_data),
  #         times = rlang::expr(.time)
  #       )
  #   )
  # )

}

# nocov end
