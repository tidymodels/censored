#' A wrapper for survival times with `survbagg` models
#' @param object A model from `ipred::bagging()`.
#' @param new_data Data for prediction
#' @return A vector.
#' @keywords internal
#' @export
#' @examples
#' library(ipred)
#' bagged_tree <- bagging(Surv(time, status) ~ age + ph.ecog, data = lung)
#' survival_time_survbagg(bagged_tree, lung[1:3, ])
survival_time_survbagg <- function(object, new_data) {

  missings_in_new_data <- get_missings_survbagg(object, new_data)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- rep(NA, n_missing)
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, , drop = FALSE]
  }

  y <- predict(object, newdata = new_data)

  res <- purrr::map_dbl(y, ~ quantile(.x, probs = .5)$quantile)

  if (!is.null(missings_in_new_data)) {
    index_with_na <- rep(NA, n_total)
    index_with_na[-missings_in_new_data] <- seq_along(res)
    res <- res[index_with_na]
  }
  res

}

get_missings_survbagg <- function(object, new_data) {
  object <- object$mtrees[[1]]$btree
  trms <- stats::terms(object)
  trms <- stats::delete.response(trms)
  na_action <- (object$call)$na.action %||% rpart::na.rpart
  mod_frame <- stats::model.frame(trms, data = new_data,
                                  na.action = na_action,
                                  xlev = attr(object, "xlevels"))
  attr(mod_frame, "na.action")
}

#' A wrapper for survival probabilities with `survbagg` models
#' @param object A model from `ipred::bagging()`.
#' @param new_data Data for prediction.
#' @param time A vector of prediction times.
#' @return A vctrs list of tibbles.
#' @keywords internal
#' @export
#' @examples
#' library(ipred)
#' bagged_tree <- bagging(Surv(time, status) ~ age + ph.ecog, data = lung)
#' survival_prob_survbagg(bagged_tree, lung[1:3, ], time = 100)
survival_prob_survbagg <- function(object, new_data, time) {

  missings_in_new_data <- get_missings_survbagg(object, new_data)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- predict_survival_na(time)
      ret <- tibble(.pred = rep(list(ret), n_missing))
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, , drop = FALSE]
  }

  y <- predict(object, newdata = new_data)

  res <- purrr::map(y, ~ summary(.x, times = pmin(time, max(.x$time)))$surv)
  res <- matrix(unlist(res), ncol = length(time), byrow = TRUE)

  if (!is.null(missings_in_new_data)) {
    pred_full <- matrix(NA, nrow = n_total, ncol = ncol(res))
    pred_full[-missings_in_new_data,] <- res
    res <- pred_full
  }

  res <- matrix_to_nested_tibbles_survival(res, time)
  res
}
