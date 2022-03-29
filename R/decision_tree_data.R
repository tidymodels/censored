# These functions define the Decision tree models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_decision_tree_rpart <- function() {
  parsnip::set_model_engine("decision_tree", mode = "censored regression", eng = "rpart")
  parsnip::set_dependency("decision_tree",
                          eng = "rpart",
                          pkg = "pec",
                          mode = "censored regression")
  parsnip::set_dependency("decision_tree",
                          eng = "rpart",
                          pkg = "censored",
                          mode = "censored regression")

  parsnip::set_fit(
    model = "decision_tree",
    eng = "rpart",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "pec", fun = "pecRpart"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "decision_tree",
    eng = "rpart",
    mode = "censored regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "rpart",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = unname,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit$rpart),
          newdata = quote(new_data)
        )
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "rpart",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = function(x, object) {
        time <- object$spec$method$pred$survival$args$time
        matrix_to_nested_tibbles_survival(x, time)
      },
      func = c(pkg = "pec", fun = "predictSurvProb"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          times = rlang::expr(time)
        )
    )
  )

}

make_decision_tree_party <- function() {

  parsnip::set_model_engine("decision_tree", mode = "censored regression", eng = "party")
  parsnip::set_dependency("decision_tree", eng = "party", pkg = "party")
  parsnip::set_dependency("decision_tree", eng = "party", pkg = "censored")

  parsnip::set_fit(
    model = "decision_tree",
    eng = "party",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "censored", fun = "cond_inference_surv_ctree"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "decision_tree",
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
    model = "decision_tree",
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
    model = "decision_tree",
    eng = "party",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_ctree"),
      args = list(object = quote(object$fit),
                  new_data = quote(new_data))
    )
  )

  # model args ----------------------------------------------------

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "party",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "party",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = TRUE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "party",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

}

# nocov end
