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
        unname(summary(x)$table[, "rmean"])
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
      func = c(pkg = "censored", fun = "survival_prob_cph"),
      args =
        list(
          x = quote(object$fit),
          new_data = quote(new_data),
          times = rlang::expr(time)
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
          na.action = quote(stats::na.exclude),
          reference = "zero"
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
      pre = coxnet_predict_pre,
      post = parsnip::.organize_glmnet_pred,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
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
          times = expr(time),
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
      pre = coxnet_predict_pre,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(object = expr(object$fit),
             newx = expr(new_data)
        )
    )
  )

}


# nocov end

#' Wrapper for glmnet for censored
#'
#' Not to be used directly by users.
#'
#' @details
#' This wrapper translates from formula interface to glmnet's matrix due to how
#' stratification can be specified. glmnet requires that the _response_ is
#' stratified via [glmnet::stratifySurv()]. censored allows specification via a
#' [survival::strata()] term on the right-hand side of the formula. The formula
#' is used to generate the stratification information needed for stratifying the
#' response. The formula without the strata term is used for generating the
#' model matrix for glmnet.
#'
#' The wrapper retains the original formula and the pre-processing elements
#' including the training data to allow for predictions from the fitted model.
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

  formula_without_strata <- remove_strata(formula)

  data_obj <- parsnip::.convert_form_to_xy_fit(
    formula = formula_without_strata,
    data = data,
    composition = "matrix",
    indicators = indicators,
    remove_intercept = remove_intercept
  )

  if (has_strata(formula)) {
    check_strata_nterms(formula)
    strata <- get_strata(formula, data)
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

has_strata <- function(formula) {
  mod_terms <- stats::terms(formula, specials = "strata")
  !is.null(attr(mod_terms, "specials")$strata)
}

# glmnet only allows one strata column so we require that there's only one term
check_strata_nterms <- function(formula) {
  mod_terms <- stats::terms(formula, specials = "strata")
  strata_terms <- attr(mod_terms, "specials")$strata
  if (length(strata_terms) > 1) {
    rlang::abort(
      paste(
        "There should be a single 'strata' term specified using the `strata()`",
        "function. It can contain multiple strata colums, e.g., ` ~ x + strata(s1, s2)`."
      )
    )
  }
  invisible(formula)
}

get_strata <- function(formula, data, na.action = stats::na.omit) {
  mod_terms <- stats::terms(formula, specials = "strata")
  mod_frame <- stats::model.frame(mod_terms, data, na.action = na.action)

  strata_ind <- attr(mod_terms,"specials")$strata
  strata <- purrr::pluck(mod_frame, strata_ind)

  strata
}

remove_strata <- function(formula) {
  if (!has_strata(formula)) {
    return(formula)
  }

  rhs <- formula[[3]]
  formula[[3]] <- rhs %>%
    drop_strata() %>%
    check_intercept_model() %>%
    check_strata_remaining()
  formula
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

check_intercept_model <- function(expr) {
  if (expr == rlang::sym("1") | is_call(expr, "strata")) {
    abort("The Cox model does not contain an intercept, please add a predictor.")
  }
  expr
}

check_strata_remaining <- function(expr) {
  if (is_call(expr, "strata")) {
    abort("Stratification needs to be specified via `+ strata()`.")
  } else if (is_call(expr)) {
    expr[-1] <- map(as.list(expr[-1]), check_strata_remaining)
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

coxnet_predict_pre <- function(new_data, object) {
  parsnip::.convert_form_to_xy_new(
    object$preproc$coxnet,
    new_data,
    composition = "matrix")$x
}
