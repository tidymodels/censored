# These functions define the Decision tree models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_decision_tree_rpart <- function() {
  parsnip::set_model_engine("decision_tree", mode = "censored regression", eng = "rpart")
  parsnip::set_dependency(
    "decision_tree",
    eng = "rpart",
    pkg = "pec",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "decision_tree",
    eng = "rpart",
    pkg = "censored",
    mode = "censored regression"
  )

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
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_pecRpart"),
      args =
        list(
          object = rlang::expr(object),
          new_data = rlang::expr(new_data),
          eval_time = rlang::expr(eval_time)
        )
    )
  )
}

make_decision_tree_partykit <- function() {
  parsnip::set_model_engine("decision_tree", mode = "censored regression", eng = "partykit")
  parsnip::set_dependency(
    "decision_tree", 
    eng = "partykit", 
    pkg = "partykit", 
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "decision_tree", 
    eng = "partykit", 
    pkg = "censored", 
    mode = "censored regression"
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "partykit",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "partykit",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "partykit",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "decision_tree",
    eng = "partykit",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "parsnip", fun = "ctree_train"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "decision_tree",
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
    model = "decision_tree",
    eng = "partykit",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
    )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "partykit",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "censored", fun = "survival_prob_partykit"),
      args = list(
        object = rlang::expr(object),
        new_data = rlang::expr(new_data),
        eval_time = rlang::expr(eval_time)
      )
    )
  )
}

# nocov end
