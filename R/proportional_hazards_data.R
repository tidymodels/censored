# These functions define the proportional hazards models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_proportional_hazards_survival <- function() {

  parsnip::set_model_engine("proportional_hazards", mode = "censored regression", eng = "survival")
  parsnip::set_dependency("proportional_hazards", eng = "survival", pkg = "survival")
  parsnip::set_dependency("proportional_hazards", eng = "survival", pkg = "riskRegression")
  parsnip::set_dependency("proportional_hazards", eng = "survival", pkg = "censored")

  parsnip::set_fit(
    model = "proportional_hazards",
    eng = "survival",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
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
      post = function(x, object) {
        unname(summary(x)$table[, "*rmean"])
      },
      func = c(fun = "survfit"),
      args =
        list(
          formula = quote(object$fit),
          newdata = quote(new_data),
          na.action = quote(stats::na.exclude)
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
      func = c(pkg = "censored", fun = "cph_survival_prob"),
      args =
        list(
          x = quote(object$fit),
          new_data = quote(new_data),
          .times = rlang::expr(.time)
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
        # For consistency with other models, we want the lp to increase with
        # time. For this, we change the sign
        -unname(x)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          na.action = quote(stats::na.exclude)
        )
    )
  )
}

make_proportional_hazards_glmnet <- function() {

  parsnip::set_model_engine("proportional_hazards", mode = "censored regression", eng = "glmnet")
  parsnip::set_dependency("proportional_hazards", eng = "glmnet", pkg =  "glmnet")
  parsnip::set_dependency("proportional_hazards", eng = "glmnet", pkg = "censored")

  parsnip::set_fit(
    model = "proportional_hazards",
    eng = "glmnet",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "family"),
      func = c(pkg = "censored", fun = "glmnet_fit_wrapper"),
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

  set_model_arg(
    model = "proportional_hazards",
    eng = "glmnet",
    parsnip = "penalty",
    original = "lambda",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = TRUE
  )

  set_model_arg(
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
      pre = NULL,
      post = organize_glmnet_pred,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newx = expr(as.matrix(new_data)),
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
      func = c(pkg = "censored", fun = "coxnet_survival_prob"),
      args =
        list(
          x = expr(object$fit),
          new_data = expr(new_data),
          .times = expr(.time),
          s = expr(object$spec$args$penalty),
          training_data = expr(object$training_data)
        )
    )
  )
}


# nocov end

#' Wrapper for glmnet for censored
#'
#' Not to be used directly by users
#'
#' @inheritParams glmnet::glmnet
#' @param ... additional parameters passed to glmnet::glmnet.
#' @export
#' @keywords internal
glmnet_fit_wrapper <- function(formula, data, alpha = 1, lambda = NULL, ...) {

  dots <- rlang::quos(...)
  check_dots_coxnet(dots)

  encoding_info <-
    parsnip::get_encoding("proportional_hazards") %>%
    dplyr::filter(mode == "censored regression", engine == "glmnet")

  indicators <- encoding_info %>% dplyr::pull(predictor_indicators)
  remove_intercept <- encoding_info %>% dplyr::pull(remove_intercept)

  # declare specials
  trms <- terms(formula, specials = "strata")
  has_strata <- !is.null(attr(trms, "specials")$strata)

  # Stratification: we require it to be specified on the right-hand side of the
  # formula, e.g., `Surv(time, event) ~ x + strata(s1, s2)`,
  # whereas glmnet requires strata to be specified in the response via its own
  # stratification function, e.g., `stratifySurv(Surv(time, event), s)`.
  # We extract the strata column from the model frame (as it can be a
  # combination of several variables) before removing the strata term from the
  # right-hand side of the formula for regular processing through parsnip.
  # Before passing the response to the fitting function, we stratify it with
  # column extracted prior to the formula tweaking.
  if (has_strata) {

    # glmnet only allows one strata column so we require that there is only one term
    strata_ind <- attr(trms, "specials")$strata + 1
    if (length(strata_ind) > 1) {
      rlang::abort(
        paste(
          "There should be a single 'strata' term specified using the `strata()`",
          "function. It can contain multiple strata colums, e.g., ` ~ x + strata(s1, s2)`."
        )
      )
    }

    strata <- convert_form_to_strata(formula = trms, data = data)
    trms <- remove_terms_from_rhs(trms, strata_ind)
  }

  # convert formula to x and y with parsnip function
  # TODO: discuss exporting the function from parsnip
  data_obj <- parsnip:::convert_form_to_xy_fit(
    formula = trms,
    data = data,
    composition = "matrix",
    indicators = indicators,
    remove_intercept = remove_intercept
  )

  # stratify the response Surv object
  if (has_strata) {
    data_obj$y <- glmnet::stratifySurv(data_obj$y, strata = strata)
  }

  fit <- glmnet::glmnet(data_obj$x, data_obj$y, family = "cox",
                        alpha = alpha, lambda = lambda, ...)
  res <- list(
    fit = fit,
    x = data_obj$x,
    y = data_obj$y
  )
  class(res) <- "coxnet"
  res
}

convert_form_to_strata <- function(formula,
                                   data,
                                   na.action = na.omit) {

  mf_call <- quote(model.frame(formula, data))
  mf_call$na.action <- match.call()$na.action

  mod_frame <- eval_tidy(mf_call)
  mod_terms <- attr(mod_frame, "terms")

  strata_ind <- attr(mod_terms,"specials")$strata
  strata <- purrr::pluck(mod_frame, strata_ind)

  strata
}

remove_terms_from_rhs <- function(formula, ind){
  form_terms <- attr(formula, "variables")
  rhs <- form_terms[-c(1:2, ind)]
  if (length(rhs) == 0) {
    rhs <- rlang::expr(1)
  } else if (length(rhs) > 1) {
    rhs <- purrr::reduce(rhs, function(l, r) rlang::expr(!!l + !!r))
  } else {
    rhs <- rlang::expr(!!rhs[[1]])
  }
  formula[[3]] <- rhs

  formula
}

check_dots_coxnet <- function(x) {
  bad_args <- c("subset", "weights", "contrasts", "offset")
  bad_names <- names(x) %in% bad_args
  if (any(bad_names)) {
    rlang::abort(
      glue::glue(
        "These argument(s) cannot be used to create the data: ",
        glue::glue_collapse(glue::glue("`{names(x)[bad_names]}`"), sep = ", ")
      )
    )
  }
  invisible(NULL)
}
