#' A wrapper for fitting a null model with survival data
#'
#' Drops all predictors and fits a single Kaplan-Meier curve via
#' [survival::survfit()], giving a non-informative baseline for censored
#' regression.
#' @param formula A formula with a [survival::Surv()] response.
#' @param data A data frame.
#' @param ... Options to pass to [survival::survfit()].
#' @return A `survfit` object.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("survival")
#' survfit_null(survival::Surv(time, status) ~ ., data = survival::lung)
survfit_null <- function(formula, data, ...) {
  formula <- stats::update(formula, . ~ 1)
  survival::survfit(formula, data = data, ...)
}

#' A wrapper for survival times with null models
#' @param object A parsnip `model_fit` object resulting from a `null_model()`
#' with engine = "censored".
#' @param new_data Data for prediction.
#' @return A numeric vector of predicted median survival times.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("survival")
#' mod <- parsnip::null_model() |>
#'   parsnip::set_engine("censored") |>
#'   parsnip::set_mode("censored regression") |>
#'   fit(survival::Surv(time, status) ~ ., data = survival::lung)
#' survival_time_survfit_null(mod, new_data = survival::lung[1:3, ])
survival_time_survfit_null <- function(object, new_data) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "survfit", arg = "object$fit")
  check_data_frame(new_data)

  median_time <- stats::quantile(engine_fit, probs = 0.5)$quantile
  rep(unname(median_time), nrow(new_data))
}

#' A wrapper for survival probabilities with null models
#' @param object A parsnip `model_fit` object resulting from a `null_model()`
#' with engine = "censored".
#' @param new_data Data for prediction.
#' @param eval_time A vector of integers for prediction times.
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examplesIf rlang::is_installed("survival")
#' mod <- parsnip::null_model() |>
#'   parsnip::set_engine("censored") |>
#'   parsnip::set_mode("censored regression") |>
#'   fit(survival::Surv(time, status) ~ ., data = survival::lung)
#' survival_prob_survfit_null(mod, new_data = survival::lung[1:3, ], eval_time = 300)
survival_prob_survfit_null <- function(object, new_data, eval_time) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "survfit", arg = "object$fit")
  check_data_frame(new_data)
  check_eval_time(eval_time, allow_infinite = TRUE, allow_negative = TRUE)

  surv_summary <- summary(engine_fit, times = eval_time, extend = TRUE)
  pred_one <- tibble::tibble(
    .pred = list(
      tibble::tibble(
        .eval_time = eval_time,
        .pred_survival = surv_summary$surv
      )
    )
  )
  vctrs::vec_rep(pred_one, nrow(new_data))
}
