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
#' @param call The call passed to [rlang::abort()].
#'
#' @return A fitted `glmnet` model.
#' @export
#' @keywords internal
#' @examples
#' coxnet_mod <- coxnet_train(Surv(time, status) ~ age + sex, data = lung)
coxnet_train <- function(formula,
                         data,
                         alpha = 1,
                         lambda = NULL,
                         weights = NULL,
                         ...,
                         call = caller_env()) {
  dots <- rlang::quos(...)
  check_dots_coxnet(dots, call = call)

  encoding_info <-
    parsnip::get_encoding("proportional_hazards") %>%
    dplyr::filter(mode == "censored regression", engine == "glmnet")

  indicators <- encoding_info %>% dplyr::pull(predictor_indicators)
  remove_intercept <- encoding_info %>% dplyr::pull(remove_intercept)

  formula_without_strata <- remove_strata(formula, data, call = call)

  data_obj <- parsnip::.convert_form_to_xy_fit(
    formula = formula_without_strata,
    data = data,
    composition = "matrix",
    indicators = indicators,
    remove_intercept = remove_intercept
  )

  if (has_strata(formula, data)) {
    check_strata_nterms(formula, data, call = call)
    strata <- get_strata_glmnet(formula, data)
    data_obj$y <- glmnet::stratifySurv(data_obj$y, strata = strata)
  }

  fit <- glmnet::glmnet(
    data_obj$x,
    data_obj$y,
    family = "cox",
    alpha = alpha,
    lambda = lambda,
    weights = weights,
    ...
  )

  # TODO: remove offset from data_obj?
  res <- list(
    fit = fit,
    preproc = data_obj
  )
  class(res) <- "coxnet"
  res
}

has_strata <- function(formula, data) {
  mod_terms <- stats::terms(formula, specials = "strata", data = data)
  !is.null(attr(mod_terms, "specials")$strata)
}

# glmnet only allows one strata column so we require that there's only one term
check_strata_nterms <- function(formula, data, call = caller_env()) {
  mod_terms <- stats::terms(formula, specials = "strata", data = data)
  strata_terms <- attr(mod_terms, "specials")$strata
  if (length(strata_terms) > 1) {
    rlang::abort(
      c(
        "There can only be a single 'strata' term specified using the `strata()` function.",
        i = "It can contain multiple strata columns, e.g., ` ~ x + strata(s1, s2)`."
      ),
      call = call
    )
  }
  invisible(formula)
}

get_strata_glmnet <- function(formula, data, na.action = stats::na.omit) {
  mod_terms <- stats::terms(formula, specials = "strata", data = data)
  mod_terms <- stats::delete.response(mod_terms)
  mod_frame <- stats::model.frame(mod_terms, data, na.action = na.action)

  strata_ind <- attr(mod_terms, "specials")$strata
  strata <- purrr::pluck(mod_frame, strata_ind)

  strata
}

remove_strata <- function(formula, data, call = rlang::caller_env()) {
  if (!has_strata(formula, data)) {
    return(formula)
  }

  rhs <- formula[[3]]
  formula[[3]] <- rhs %>%
    drop_strata() %>%
    check_intercept_model(call = call) %>%
    check_strata_remaining(call = call)
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

check_intercept_model <- function(expr, call = caller_env()) {
  if (expr == rlang::sym("1") | is_call(expr, "strata")) {
    abort(
      "The Cox model does not contain an intercept, please add a predictor.",
      call = call
    )
  }
  expr
}

check_strata_remaining <- function(expr, call = rlang::caller_env()) {
  if (is_call(expr, "strata")) {
    abort(
      c(
        "Stratification must be nested under a chain of `+` calls.",
        i = "# Good: ~ x1 + x2 + strata(s)",
        i = "# Bad: ~ x1 + (x2 + strata(s))"
      ),
      call = call
    )
  } else if (is_call(expr)) {
    #lapply() instead of map() to avoid map() reporting the index of where it errors
    expr[-1] <- lapply(as.list(expr[-1]), check_strata_remaining, call = call)
    expr
  } else {
    expr
  }
}

check_dots_coxnet <- function(x, call = caller_env()) {
  bad_args <- c("subset", "contrasts", "offset", "family")
  bad_names <- names(x) %in% bad_args
  if (any(bad_names)) {
    rlang::abort(
      glue::glue(
        "These argument(s) cannot be used to create the model: ",
        glue::glue_collapse(glue::glue("`{names(x)[bad_names]}`"), sep = ", ")
      ),
      call = call
    )
  }
  invisible(NULL)
}

#' @export
print._coxnet <- function(x, ...) {
  cat("parsnip model object\n\n")
  cat("Fit time: ", prettyunits::pretty_sec(x$elapsed[["elapsed"]]), "\n")

  if (inherits(x$fit$fit, "try-error")) {
    cat("Model fit failed with error:\n", x$fit, "\n")
  } else {
    print(x$fit$fit, ...)
    cat("The training data has been saved for prediction.\n")
  }
  invisible(x)
}


# prediction --------------------------------------------------------------

coxnet_prepare_x <- function(new_data, object) {
  went_through_formula_interface <- !is.null(object$preproc$coxnet)

  if (went_through_formula_interface) {
    new_x <- parsnip::.convert_form_to_xy_new(
      object$preproc$coxnet,
      new_data,
      composition = "matrix"
    )$x
  } else {
    new_x <- new_data[, object$preproc$x_var, drop = FALSE] %>%
      as.matrix()
  }

  new_x
}

# notes adapted from parsnip:

# glmnet call stack for censored regression using `predict` when object has
# classes "_coxnet" and "model_fit":
#
#  predict()
#   predict._coxnet(penalty = NULL)   <-- checks and sets penalty
#    predict.model_fit()              <-- checks for extra vars in ...
#     predict_survival()
#      predict_survival._coxnet()
#       predict_survival.model_fit()
#        survival_prob_coxnet()

# glmnet call stack for censored regression using `multi_predict(type = "linear_pred")` when object has
# classes "_coxnet" and "model_fit":
#
# 	multi_predict()
#    multi_predict._coxnet(penalty = NULL)
#      predict._coxnet(multi = TRUE)          <-- checks and sets penalty
#       predict.model_fit()                   <-- checks for extra vars in ...
#        predict_raw()
#         predict_raw._coxnet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.coxnet()

# glmnet call stack for censored regression using `multi_predict(type = "survival")` when object has
# classes "_coxnet" and "model_fit":
#
# 	multi_predict()
#    multi_predict._coxnet(penalty = NULL)
#      predict._coxnet(multi = TRUE)          <-- checks and sets penalty
#       predict.model_fit()                  <-- checks for extra vars in ...
#        predict_survival()
#         predict_survival._coxnet()
#          predict_survival.model_fit()
#           survival_prob_coxnet()

#' @export
predict._coxnet <-
  function(object, new_data, type = NULL, opts = list(), penalty = NULL, multi = FALSE, ...) {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (is.null(penalty) & !is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    }

    object$spec$args$penalty <- parsnip::.check_glmnet_penalty_predict(penalty, object, multi)

    object$spec <- eval_args(object$spec)
    predict.model_fit(object, new_data = new_data, type = type, opts = opts, ...)
  }

#' @export
predict_survival._coxnet <- function(object, new_data, ...) {
  object$spec <- eval_args(object$spec)
  NextMethod()
}

#' @export
predict_linear_pred._coxnet <- function(object,
                                        new_data,
                                        ...,
                                        increasing = TRUE) {
  res <- NextMethod()
  if (increasing) {
    # For consistency with other models, we want the lp to increase with
    # time. For this, we change the sign
    res <- -res
  }
  res
}

#' @export
predict_raw._coxnet <- function(object, new_data, opts = list(), ...) {
  object$spec <- eval_args(object$spec)
  opts$s <- object$spec$args$penalty
  NextMethod()
}


# multi_predict -----------------------------------------------------------

#' @export
multi_predict._coxnet <- function(object,
                                  new_data,
                                  type = NULL,
                                  penalty = NULL,
                                  ...) {
  dots <- list(...)

  if (any(names(dots) == "newdata")) {
    rlang::abort("Please use `new_data` instead of `newdata`.")
  }

  object$spec <- eval_args(object$spec)

  if (is.null(penalty)) {
    # See discussion in https://github.com/tidymodels/parsnip/issues/195
    if (!is.null(object$spec$args$penalty)) {
      penalty <- object$spec$args$penalty
    } else {
      penalty <- object$fit$lambda
    }
  }

  if (type == "linear_pred") {
    pred <- multi_predict_coxnet_linear_pred(
      object,
      new_data = new_data,
      opts = dots,
      penalty = penalty
    )
  } else {
    pred <- predict(
      object,
      new_data = new_data,
      type = type,
      ...,
      penalty = penalty,
      multi = TRUE
    )
  }

  pred
}

multi_predict_coxnet_linear_pred <- function(object, new_data, opts, penalty) {

  if ("increasing" %in% names(opts)) {
    increasing <- opts$increasing
    opts$increasing <- NULL
  } else {
    increasing <- TRUE
  }

  pred <- predict(
    object,
    new_data = new_data,
    type = "raw",
    opts = opts,
    penalty = penalty,
    multi = TRUE
  )

  if (increasing) {
    # For consistency with other models, we want the lp to increase with
    # time. For this, we change the sign
    pred <- -pred
  }

  # post-processing into nested tibble
  param_key <- tibble(group = colnames(pred), penalty = penalty)
  pred <- pred %>%
    as_tibble() %>%
    dplyr::mutate(.row = seq_len(nrow(pred))) %>%
    tidyr::pivot_longer(
      -.row,
      names_to = "group",
      values_to = ".pred_linear_pred"
    )
  if (utils::packageVersion("dplyr") >= "1.0.99.9000") {
    pred <- dplyr::inner_join(param_key, pred, by = "group", multiple = "all")
  } else {
    pred <- dplyr::inner_join(param_key, pred, by = "group")
  }
  pred <- pred %>%
    dplyr::select(-group) %>%
    dplyr::arrange(.row, penalty) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)
}


# prediction: time --------------------------------------------------------

#' A wrapper for survival times with coxnet models
#' @param object A fitted `_coxnet` object.
#' @param new_data Data for prediction.
#' @param penalty Penalty value(s).
#' @param ... Options to pass to [survival::survfit()].
#' @return A vector.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- proportional_hazards(penalty = 0.1) %>%
#'   set_engine("glmnet") %>%
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_time_coxnet(cox_mod, new_data = lung[1:3, ], penalty = 0.1)
survival_time_coxnet <- function(object, new_data, penalty = NULL, ...) {
  new_x <- coxnet_prepare_x(new_data, object)

  went_through_formula_interface <- !is.null(object$preproc$coxnet)
  if (went_through_formula_interface &&
    has_strata(object$formula, object$training_data)) {
    new_strata <- get_strata_glmnet(
      object$formula,
      data = new_data,
      na.action = stats::na.pass
    )
  } else {
    new_strata <- NULL
  }

  missings_in_new_data <- get_missings_coxnet(new_x, new_strata)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- rep(NA, n_missing)
      return(ret)
    }
    new_x <- new_x[-missings_in_new_data, , drop = FALSE]
    new_strata <- new_strata[-missings_in_new_data]
  }

  y <- survival::survfit(
    object$fit$fit,
    newx = new_x,
    newstrata = new_strata,
    s = penalty,
    x = object$training_data$x,
    y = object$training_data$y,
    weights = object$preproc$coxnet$weights,
    na.action = stats::na.exclude,
    ...
  )

  tabs <- summary(y)$table
  if (is.matrix(tabs)) {
    colnames(tabs) <- gsub("[[:punct:]]", "", colnames(tabs))
    res <- unname(tabs[, "rmean"])
  } else {
    names(tabs) <- gsub("[[:punct:]]", "", names(tabs))
    res <- unname(tabs["rmean"])
  }
  if (!is.null(missings_in_new_data)) {
    index_with_na <- rep(NA, n_total)
    index_with_na[-missings_in_new_data] <- seq_along(res)
    res <- res[index_with_na]
  }
  res
}


get_missings_coxnet <- function(new_x, new_strata) {
  missings_logical <- apply(cbind(new_x, new_strata), MARGIN = 1, anyNA)
  if (!any(missings_logical)) {
    return(NULL)
  }
  which(missings_logical)
}

# prediction: survival ----------------------------------------------------


#' A wrapper for survival probabilities with coxnet models
#' @param object A fitted `_coxnet` object.
#' @param new_data Data for prediction.
#' @param eval_time A vector of integers for prediction times.
#' @param time Deprecated in favor of `eval_time`. A vector of integers for prediction times.
#' @param output One of "surv" or "haz".
#' @param penalty Penalty value(s).
#' @param ... Options to pass to [survival::survfit()].
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- proportional_hazards(penalty = 0.1) %>%
#'   set_engine("glmnet") %>%
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_prob_coxnet(cox_mod, new_data = lung[1:3, ], eval_time = 300)
survival_prob_coxnet <- function(object,
                                 new_data,
                                 eval_time,
                                 time = deprecated(),
                                 output = "surv",
                                 penalty = NULL,
                                 ...) {
  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "survival_prob_coxnet(time)",
      "survival_prob_coxnet(eval_time)"
    )
    eval_time <- time
  }

  if (is.null(penalty)) {
    penalty <- object$spec$args$penalty
  }

  output <- match.arg(output, c("surv", "haz"))
  multi <- length(penalty) > 1

  new_x <- coxnet_prepare_x(new_data, object)

  went_through_formula_interface <- !is.null(object$preproc$coxnet)
  if (went_through_formula_interface &&
    has_strata(object$formula, object$training_data)) {
    new_strata <- get_strata_glmnet(
      object$formula,
      data = new_data,
      na.action = stats::na.pass
    )
  } else {
    new_strata <- NULL
  }

  n_obs <- nrow(new_data)
  missings_in_new_data <- get_missings_coxnet(new_x, new_strata)

  if (!is.null(missings_in_new_data)) {
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_obs
    if (all_missing) {
      ret <- predict_survival_na(eval_time, interval = "none")
      ret <- tibble(.pred = rep(list(ret), n_missing))
      return(ret)
    }
    new_x <- new_x[-missings_in_new_data, , drop = FALSE]
    new_strata <- new_strata[-missings_in_new_data]
  }

  y <- survival::survfit(
    object$fit$fit,
    newx = new_x,
    newstrata = new_strata,
    s = penalty,
    x = object$training_data$x,
    y = object$training_data$y,
    weights = object$preproc$coxnet$weights,
    na.action = na.exclude,
    ...
  )

  if (multi) {
    res_patched <- purrr::map(
      y,
      survfit_summary_to_patched_tibble,
      index_missing = missings_in_new_data,
      eval_time = eval_time,
      n_obs = n_obs
    )
    res <- tibble::tibble(
      penalty = penalty,
      res_patched = res_patched
    ) %>%
      tidyr::unnest(cols = res_patched) %>%
      keep_cols(output, keep_penalty = TRUE) %>%
      tidyr::nest(.pred = c(-.row)) %>%
      dplyr::select(-.row)
  } else {
    res <- survfit_summary_to_patched_tibble(
      y,
      index_missing = missings_in_new_data,
      eval_time = eval_time,
      n_obs = n_obs
    ) %>%
      keep_cols(output) %>%
      tidyr::nest(.pred = c(-.row)) %>%
      dplyr::select(-.row)
  }

  res
}
