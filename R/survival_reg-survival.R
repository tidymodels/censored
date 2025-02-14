# The model specification is in parsnip.

# ------------------------------------------------------------------------------

survreg_quant <- function(results, object) {
  quantile_levels <- object$spec$quantile_levels

  if (!is.matrix(results)) {
    if (length(quantile_levels) < 2) {
      results <- matrix(results, ncol = 1)
    } else {
      results <- matrix(results, nrow = 1)
    }
  }

  tibble::new_tibble(
    x = list(.pred_quantile = hardhat::quantile_pred(results, quantile_levels))
  )
}

# ------------------------------------------------------------------------------
# helpers for survreg prediction

# When strata are used with parametric models, the scale is different for each
# strata. The parameter estimates are saved in a named vector. For example:
#
#  c(`ecog.ps=1` = 1.111, `ecog.ps=2` = 0.714)
#
# When getting probability estimates we need to match the scale coefficients to
# the original column in the data. The survival package has code to do this
# but it is buried inside of `predict.survreg()`

get_survreg_scale <- function(object, new_data) {
  n <- nrow(new_data)
  if (length(object$scale) == 1) {
    res <- rep(unname(object$scale), n)
  } else {
    res <- deparse_survreg_strata(object, new_data)
  }
  res
}

compute_strata <- function(object, new_data) {
  trms <- stats::delete.response(object$terms)
  new_new_data <-
    stats::model.frame(
      trms,
      data = new_data,
      na.action = na.pass,
      xlev = object$xlevels
    )
  strata_info <- survival::untangle.specials(trms, "strata", 1)
  new_new_data$.strata <-
    survival::strata(new_new_data[, strata_info$vars], shortlabel = TRUE)
  tibble::as_tibble(new_new_data)
}

deparse_survreg_strata <- function(object, new_data) {
  new_new_data <- compute_strata(object, new_data)
  lvls <- levels(new_new_data$.strata)

  # link this to scales vector
  scales <- tibble(.strata = names(object$scale), .scale = unname(object$scale))
  scales$.strata <- factor(scales$.strata, levels = lvls)

  # return a vector with appropriate estimates for new data
  new_new_data$.row <- seq_len(nrow(new_new_data))
  new_new_data <- dplyr::left_join(new_new_data, scales, by = ".strata")
  new_new_data <- new_new_data[order(new_new_data$.row), ]
  new_new_data$.scale
}

survreg_survival <- function(location, object, scale, eval_time, ...) {
  distr <- object$dist
  tibble::tibble(
    .eval_time = eval_time,
    .pred_survival = 1 - survival::psurvreg(eval_time, location, distribution = distr, scale, ...)
  )
}

#' Internal function helps for parametric survival models
#' @param object A parsnip `model_fit` object resulting from 
#' [survival_reg() with engine = "survival"][parsnip::details_survival_reg_survival].
#' @param new_data A data frame.
#' @param eval_time A vector of time points.
#' @param time Deprecated in favor of `eval_time`. A vector of time points.
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examples
#' mod <- survival_reg() %>% 
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ ., data = lung)
#' survival_prob_survreg(mod, lung[1:3, ], eval_time = 100)
#' hazard_survreg(mod, lung[1:3, ], eval_time = 100)
survival_prob_survreg <- function(object, new_data, eval_time, time = deprecated()) {
  if (inherits(object, "survreg")) {
    cli::cli_abort("{.arg object} needs to be a parsnip {.cls model_fit} object, not a {.cls survreg} object.")
  }
  if (lifecycle::is_present(time)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "survival_prob_survreg(time)",
      "survival_prob_survreg(eval_time)"
    )
    eval_time <- time
  }

  lp_estimate <- predict(object$fit, new_data, type = "lp")
  scale_estimate <- get_survreg_scale(object$fit, new_data)
  res <-
    purrr::map2(
      lp_estimate,
      scale_estimate,
      ~ survreg_survival(.x, object = object$fit, eval_time = eval_time, scale = .y)
    )
  tibble::tibble(.pred = unname(res))
}

survreg_hazard <- function(location, object, scale = object$scale, eval_time, ...) {
  distr <- object$dist
  prob <-
    survival::dsurvreg(eval_time, location, scale, distribution = distr, ...) /
      (1 - survival::psurvreg(eval_time, location, distribution = distr, scale, ...))
  tibble::tibble(
    .eval_time = eval_time,
    .pred_hazard = prob
  )
}

#' @export
#' @rdname survival_prob_survreg
hazard_survreg <- function(object, new_data, eval_time) {
  if (inherits(object, "survreg")) {
    cli::cli_abort("{.arg object} needs to be a parsnip {.cls model_fit} object, not a {.cls survreg} object.")
  }
  lp_estimate <- predict(object$fit, new_data, type = "lp")
  scale_estimate <- get_survreg_scale(object$fit, new_data)
  res <-
    purrr::map2(
      lp_estimate,
      scale_estimate,
      ~ survreg_hazard(.x, object = object$fit, eval_time = eval_time, scale = .y)
    )
  tibble::tibble(.pred = unname(res))
}
