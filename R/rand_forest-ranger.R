#' Internal helper functions for ranger survival models
#'
#' @param object A parsnip `model_fit` object resulting from
#'   [rand_forest() with engine = "ranger"][parsnip::details_rand_forest_ranger].
#' @param new_data A data frame to be predicted.
#' @param eval_time A vector of times to predict the survival probability.
#' @return
#' * `survival_time_ranger()`: A numeric vector of predicted survival times.
#' * `survival_prob_ranger()`: A tibble with a list column of nested tibbles.
#' @export
#' @keywords internal
#' @name ranger_internal
#' @examplesIf rlang::is_installed("ranger")
#' mod <- rand_forest() |>
#'   set_engine("ranger") |>
#'   set_mode("censored regression") |>
#'   fit(Surv(time, status) ~ age + ph.ecog, data = na.omit(lung))
#' survival_time_ranger(mod, new_data = lung[1:3, ])
survival_time_ranger <- function(object, new_data) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "ranger", arg = "object$fit")
  check_data_frame(new_data)

  pred <- predict(engine_fit, data = new_data)
  surv_matrix <- matrix(pred$survival, nrow = nrow(new_data))
  udt <- pred$unique.death.times

  apply(surv_matrix, 1, \(srow) {
    below <- which(srow <= 0.5)
    if (length(below) == 0) Inf else udt[min(below)]
  })
}

#' @rdname ranger_internal
#' @export
#' @examplesIf rlang::is_installed("ranger")
#' mod <- rand_forest() |>
#'   set_engine("ranger") |>
#'   set_mode("censored regression") |>
#'   fit(Surv(time, status) ~ age + ph.ecog, data = na.omit(lung))
#' survival_prob_ranger(mod, new_data = lung[1:3, ], eval_time = c(250, 100))
survival_prob_ranger <- function(object, new_data, eval_time) {
  check_inherits(object, "model_fit")
  engine_fit <- hardhat::extract_fit_engine(object)
  check_inherits(engine_fit, "ranger", arg = "object$fit")
  check_data_frame(new_data)
  check_eval_time(eval_time, allow_empty = TRUE, allow_infinite = TRUE)

  pred <- predict(engine_fit, data = new_data)
  surv_matrix <- matrix(pred$survival, nrow = nrow(new_data))
  udt <- pred$unique.death.times

  n_obs <- nrow(new_data)
  n_eval_time <- length(eval_time)

  idx <- findInterval(eval_time, udt)
  cols <- pmax(idx, 1L)
  prob_matrix <- surv_matrix[, cols, drop = FALSE]
  prob_matrix[, idx == 0L] <- 1

  tibble::tibble(
    .row = rep(seq_len(n_obs), times = n_eval_time),
    .eval_time = rep(eval_time, each = n_obs),
    .pred_survival = as.numeric(prob_matrix)
  ) |>
    tidyr::nest(.pred = c(-.row)) |>
    dplyr::select(-.row)
}
