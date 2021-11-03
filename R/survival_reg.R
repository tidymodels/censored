# The model specification is in parsnip.

# ------------------------------------------------------------------------------

survreg_quant <- function(results, object) {
  pctl <- object$spec$method$pred$quantile$args$p
  n <- nrow(results)
  p <- ncol(results)
  colnames(results) <- names0(p)
  results <-
    results %>%
    tibble::as_tibble(results) %>%
    dplyr::mutate(.row = 1:n) %>%
    tidyr::gather(.label, .pred, -.row) %>%
    dplyr::arrange(.row, .label) %>%
    dplyr::mutate(.quantile = rep(pctl, n)) %>%
    dplyr::select(-.label)
  .row <- results[[".row"]]
  results <-
    results %>%
    dplyr::select(-.row)
  results <- split(results, .row)
  names(results) <- NULL
  tibble::tibble(.pred = results)
}

# ------------------------------------------------------------------------------

flexsurv_mean <- function(results, object) {
  results <- unclass(results)
  results <- dplyr::bind_rows(results)
  results$est
}

flexsurv_quant <- function(results, object) {
  results <- purrr::map(results, as_tibble)
  names(results) <- NULL
  results <- purrr::map(
    results,
    setNames,
    c(".quantile", ".pred_quantile", ".pred_quantile_lower", ".pred_quantile_upper")
    )
  tibble(.pred = results)
}

#' Internal function helps for parametric survival models
#' @param object A `survreg` or `flexsurvreg` object.
#' @param new_data A data frame.
#' @param time A vector of time points
#' @return A nested tibble with column name `.pred`
#' @keywords internal
#' @export
flexsurv_probs <- function(object, new_data, time, type = "survival") {
  type <- rlang::arg_match(type, c("survival", "hazard"))
  res <- summary(object, newdata = new_data, type = type, t = time, ci = FALSE)
  res <- unname(res)
  col_name <- rlang::sym(paste0(".pred_", type))
  res <- purrr::map(res, ~ dplyr::select(.x, time, est))
  res <- purrr::map(res, ~ setNames(.x, c(".time", col_name)))
  tibble::tibble(.pred = res)
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
  trms <- object$terms
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

#' @export
#' @rdname flexsurv_probs
survreg_survival_probs <- function(object, new_data, time) {
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
#' @rdname flexsurv_probs
survreg_hazard_probs <- function(object, new_data, time) {
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
