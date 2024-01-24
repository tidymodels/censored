# prediction --------------------------------------------------------------

#' @export
predict_linear_pred._coxph <- function(object,
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

cph_survival_pre <- function(new_data, object, ..., call = caller_env()) {
  check_dots_empty()

  # Check that the stratification variable is part of `new_data`.
  # If this information is missing, survival::survfit() does not error but
  # instead returns the survival curves for _all_ strata.
  terms_x <- stats::terms(object$fit)
  terms_special <- attr(terms_x, "specials")
  has_strata <- !is.null(terms_special$strata)

  if (has_strata) {
    strata <- attr(terms_x, "term.labels")
    strata <- grep(pattern = "^strata", x = strata, value = TRUE)
    strata <- sub(pattern = "strata\\(", replacement = "", x = strata)
    strata <- sub(pattern = "\\)", replacement = "", x = strata)
    strata_available <- strata %in% names(new_data)
    strata_missing <- strata[!strata_available]

    if (length(strata_missing) > 0) {
      cli::cli_abort(
        "{.arg new_data} is missing the following stratification variable{?s}: {.code {strata_missing}}.",
        call = call
      )
    }
  }

  new_data
}

# prediction: time --------------------------------------------------------

#' A wrapper for survival times with `coxph` models
#' @param object A parsnip `model_fit` object resulting from 
#' [proportional_hazards() with engine = "survival"][parsnip::details_proportional_hazards_survival].
#' @param new_data Data for prediction
#' @return A vector.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- proportional_hazards() %>% 
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_time_coxph(cox_mod, new_data = lung[1:3, ])
survival_time_coxph <- function(object, new_data) {
  if (inherits(object, "coxph")) {
    cli::cli_abort("{.arg object} needs to be a parsnip {.cls model_fit} object, not a {.cls coxph} object.")
  }

  missings_in_new_data <- get_missings_coxph(object$fit, new_data)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- rep(NA, n_missing)
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, ]
  }

  y <- survival::survfit(object$fit, new_data, na.action = stats::na.exclude)

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

# see https://github.com/therneau/survival/issues/137
get_missings_coxph <- function(object, new_data) {
  trms <- stats::terms(object)
  trms <- stats::delete.response(trms)
  xlevels <- object$xlevels
  mod_frame <- stats::model.frame(
    trms,
    data = new_data,
    xlev = xlevels,
    na.action = stats::na.exclude
  )

  attr(mod_frame, "na.action")
}



# prediction: survival ----------------------------------------------------

#' A wrapper for survival probabilities with coxph models
#' @param object A parsnip `model_fit` object resulting from 
#' [proportional_hazards() with engine = "survival"][parsnip::details_proportional_hazards_survival].
#' @param x Deprecated. A model from `coxph()`.
#' @param new_data Data for prediction
#' @param eval_time A vector of integers for prediction times.
#' @param time Deprecated in favor of `eval_time`. A vector of integers for prediction times.
#' @param output One of `"surv"`, `"conf"`, or `"haz"`.
#' @param interval Add confidence interval for survival probability? Options
#' are `"none"` or `"confidence"`.
#' @param conf.int The confidence level.
#' @param ... Options to pass to [survival::survfit()]
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- proportional_hazards() %>% 
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_prob_coxph(cox_mod, new_data = lung[1:3, ], eval_time = 300)
survival_prob_coxph <- function(object,
                                x = deprecated(),
                                new_data,
                                eval_time,
                                time = deprecated(),
                                output = "surv",
                                interval = "none",
                                conf.int = .95,
                                ...) {
  if (inherits(object, "coxph")) {
    cli::cli_abort("{.arg object} needs to be a parsnip {.cls model_fit} object, not a {.cls coxph} object.")
  }
  if (lifecycle::is_present(x)) {
    lifecycle::deprecate_stop(
      "0.3.0",
      "survival_prob_coxph(x)",
      "survival_prob_coxph(object)"
    )
  }
  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "survival_prob_coxph(time)",
      "survival_prob_coxph(eval_time)"
    )
    eval_time <- time
  }

  interval <- rlang::arg_match(interval, c("none", "confidence"))
  output <- rlang::arg_match(output, c("surv", "conf", "haz"))
  if (output == "surv" & interval == "confidence") {
    output <- "survconf"
  }

  n_obs <- nrow(new_data)
  missings_in_new_data <- get_missings_coxph(object$fit, new_data)

  if (!is.null(missings_in_new_data)) {
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_obs
    if (all_missing) {
      ret <- predict_survival_na(eval_time, interval)
      ret <- tibble(.pred = rep(list(ret), n_missing))
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, , drop = FALSE]
  }

  surv_fit <- survival::survfit(
    object$fit,
    newdata = new_data,
    conf.int = conf.int,
    na.action = na.exclude,
    ...
  )

  res <- surv_fit %>%
    survfit_summary_to_patched_tibble(
      index_missing = missings_in_new_data,
      eval_time = eval_time,
      n_obs = n_obs
    ) %>%
    keep_cols(output) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  res
}
