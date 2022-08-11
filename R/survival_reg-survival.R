# The model specification is in parsnip.

# ------------------------------------------------------------------------------

survreg_quant <- function(results, object) {
  pctl <- object$spec$method$pred$quantile$args$p
  n <- nrow(results)
  p <- ncol(results)
  colnames(results) <- names0(p)

  res <- results %>%
    tibble::as_tibble(results) %>%
    dplyr::mutate(.row = 1:n) %>%
    tidyr::pivot_longer(-.row, names_to = ".label",
                        values_to = ".pred_quantile") %>%
    dplyr::arrange(.row, .label) %>%
    dplyr::mutate(.quantile = rep(pctl, n)) %>%
    dplyr::select(.row, .quantile, .pred_quantile) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  res
}

# copied form recipes
names0 <- function(num, prefix = "x") {
  if (num < 1)
    rlang::abort("`num` should be > 0.")
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
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
    stats::model.frame(trms,
                       data = new_data,
                       na.action = na.pass,
                       xlev = object$xlevels)
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


survreg_survival <- function(location, object, scale, time, ...) {
  distr <- object$dist
  tibble::tibble(
    .time = time,
    .pred_survival = 1 - survival::psurvreg(time, location, distribution = distr, scale, ...)
  )
}

#' Internal function helps for parametric survival models
#' @param object A `survreg` object.
#' @param new_data A data frame.
#' @param time A vector of time points.
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examples
#' surv_reg <- survreg(Surv(time, status) ~ ., data = lung)
#' survival_prob_survreg(surv_reg, lung[1:3, ], time = 100)
#' hazard_survreg(surv_reg, lung[1:3, ], time = 100)
survival_prob_survreg <- function(object, new_data, time) {
  lp_estimate <- predict(object, new_data, type = "lp")
  scale_estimate <- get_survreg_scale(object, new_data)
  res <-
    purrr::map2(
      lp_estimate,
      scale_estimate,
      ~ survreg_survival(.x, object = object, time = time, scale = .y)
    )
  tibble::tibble(.pred = unname(res))
}

survreg_hazard <- function(location, object, scale = object$scale, time, ...) {
  distr <- object$dist
  prob <-
    survival::dsurvreg(time, location, scale, distribution = distr, ...) /
    (1 - survival::psurvreg(time, location, distribution = distr, scale, ...))
  tibble::tibble(
    .time = time,
    .pred_hazard = prob
  )
}

#' @export
#' @rdname survival_prob_survreg
hazard_survreg <- function(object, new_data, time) {
  lp_estimate <- predict(object, new_data, type = "lp")
  scale_estimate <- get_survreg_scale(object, new_data)
  res <-
    purrr::map2(
      lp_estimate,
      scale_estimate,
      ~ survreg_hazard(.x, object = object, time = time, scale = .y)
    )
  tibble::tibble(.pred = unname(res))
}
