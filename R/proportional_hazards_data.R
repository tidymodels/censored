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
          object = expr(object),
          new_data = expr(new_data),
          .times = expr(.time),
          s = expr(object$spec$args$penalty)
        )
    )
  )
}


# nocov end

#' Wrapper for glmnet for censored
#'
#' Not to be used directly by users
#'
#' @param formula The model formula.
#' @param data The data.
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
    trms <- check_number_of_strata_terms(trms)

    strata <- convert_form_to_strata(formula = trms, data = data)
    formula_without_strata <- remove_strata(formula)
    trms <- terms(formula_without_strata, specials = "strata")
  }

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

  # TODO: remove weights and offset from data_obj?
  res <- list(
    fit = fit,
    preproc = data_obj
  )
  class(res) <- "coxnet"
  res
}

check_number_of_strata_terms <- function(mod_terms) {
  strata_terms <- attr(trms, "specials")$strata
  if (length(strata_terms) > 1) {
    rlang::abort(
      paste(
        "There should be a single 'strata' term specified using the `strata()`",
        "function. It can contain multiple strata colums, e.g., ` ~ x + strata(s1, s2)`."
      )
    )
  }
}

convert_form_to_strata <- function(formula,
                                   data,
                                   na.action = na.omit) {

  mod_frame <- model.frame(formula, data, na.action = na.action)
  mod_terms <- attr(mod_frame, "terms")

  strata_ind <- attr(mod_terms,"specials")$strata
  strata <- purrr::pluck(mod_frame, strata_ind)

  strata
}

remove_strata <- function(f) {
  rhs <- f[[3]]
  f[[3]] <- rhs %>%
    drop_strata() %>%
    check_for_intercept_model() %>%
    check_for_incorrect_strata_usage()
  f
}

# strata() must be part of a sequence of `+` calls
# only drop correct usage of strata so we can check for incorrect usage in
# its own function
drop_strata <- function(expr, in_plus = TRUE) {
  if (is_call(expr, "+", n = 2) && in_plus) {
    lhs <- drop_strata(expr[[2]], in_plus = in_plus)
    rhs <- drop_strata(expr[[3]], in_plus = in_plus)
    if (is_call(lhs, "strata")) {
      rhs
    } else if (is_call(rhs, "strata")) {
      lhs
    } else {
      rlang::call2("+", lhs, rhs)
    }
  } else if (is_call(expr)) {
    expr[-1] <- map(as.list(expr[-1]), drop_strata, in_plus = FALSE)
    expr
  } else {
    expr
  }
}
check_for_intercept_model <- function(expr) {
  if (expr == rlang::sym("1") | is_call(expr, "strata")) {
    abort("The Cox model does not contain an intercept, please add a predictor.")
  }
  expr
}
check_for_incorrect_strata_usage <- function(expr) {
  if (is_call(expr, "strata")) {
    abort("Stratification needs to be specified via `+ strata()`.")
  } else if (is_call(expr)) {
    expr[-1] <- map(as.list(expr[-1]), check_strata_usage)
    expr
  } else {
    expr
  }
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
