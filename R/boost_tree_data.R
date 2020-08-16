# These functions define the Cox regression models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_boost_tree_mboost <- function() {
  parsnip::set_model_mode("boost_tree", "censored regression")

  parsnip::set_model_engine("boost_tree", mode = "censored regression", eng = "mboost")
  parsnip::set_dependency("boost_tree", eng = "mboost", pkg = "mboost")

  parsnip::set_fit(
    model = "boost_tree",
    eng = "mboost",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "family"),
      func = c(pkg = "mboost", fun = "blackboost"),
      defaults = list(family = mboost::CoxPH())
    )
  )

  set_encoding(
    model = "boost_tree",
    eng = "mboost",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE
    )
  )

  # parsnip::set_pred(
  #   model = "cox_reg",
  #   eng = "survival",
  #   mode = "censored regression",
  #   type = "survival",
  #   value = list(
  #     pre = NULL,
  #     post = function(x, object) {
  #       colnames(x) <- object$spec$method$pred$survival$args$.time
  #       res <- as_tibble(x)
  #       res <- parsnip::add_rowindex(res)
  #       res <-  pivot_longer(res, -.row,
  #                            names_to = ".time",
  #                            values_to = ".pred_survival")
  #       group_nest(res, .row, .key = ".pred")$.pred
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
  #
  parsnip::set_pred(
    model = "boost_tree",
    eng = "mboost",
    mode = "censored regression",
    type = "linear_pred",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )
}

# nocov end
