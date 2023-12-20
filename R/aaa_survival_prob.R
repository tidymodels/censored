keep_cols <- function(x, output, keep_penalty = FALSE) {
  if (keep_penalty) {
    cols_to_keep <- c(".row", "penalty", ".eval_time")
  } else {
    cols_to_keep <- c(".row", ".eval_time")
  }
  output_cols <- switch(output,
    surv = ".pred_survival",
    conf = c(".pred_lower", ".pred_upper"),
    survconf = c(".pred_survival", ".pred_lower", ".pred_upper"),
    haz = ".pred_hazard_cumulative"
  )
  cols_to_keep <- c(cols_to_keep, output_cols)
  dplyr::select(x, dplyr::all_of(cols_to_keep))
}

stack_survfit <- function(x, n, penalty = NULL) {
  # glmnet does not calculate confidence intervals
  if (is.null(x$lower)) x$lower <- NA_real_
  if (is.null(x$upper)) x$upper <- NA_real_

  has_strata <- any(names(x) == "strata")

  if (has_strata) {
    # All components are vectors of length {t_i x n}
    res <- tibble::tibble(
      penalty = penalty,
      .time = x$time,
      .pred_survival = x$surv,
      .pred_lower = x$lower,
      .pred_upper = x$upper,
      .pred_hazard_cumulative = x$cumhaz,
      .row = rep(seq_len(n), x$strata)
    )
  } else {
    # All components are {t x n} matrices (unless nrow(new_data) = 1)
    if (is.matrix(x$surv)) {
      times <- nrow(x$surv)
    } else {
      times <- 1
    }
    res <- tibble::tibble(
      penalty = penalty,
      .time = rep(x$time, n),
      .pred_survival = as.vector(x$surv),
      .pred_lower = as.vector(x$lower),
      .pred_upper = as.vector(x$upper),
      .pred_hazard_cumulative = as.vector(x$cumhaz),
      .row = rep(seq_len(n), each = times)
    )
  }

  res
}

prob_template <- tibble::tibble(
  .time = 0,
  .pred_survival = 1,
  .pred_lower = NA_real_,
  .pred_upper = NA_real_,
  .pred_hazard_cumulative = 0
)


predict_survival_na <- function(eval_time, interval = "none", penalty = NULL) {
  if (!is.null(penalty)) {
    n_penalty <- length(penalty)
    n_eval_time <- length(eval_time)
    ret <- tibble::new_tibble(
      list(
        penalty = rep(penalty, each = n_eval_time),
        .eval_time = rep(eval_time, times = n_penalty),
        .pred_survival = rep(NA_real_, n_penalty * n_eval_time)
      )
    )
  } else {
    ret <- tibble::new_tibble(
      list(
        .eval_time = eval_time,
        .pred_survival = rep(NA_real_, length(eval_time))
      )
    )
  }
  
  if (interval == "confidence") {
    ret <- ret %>%
      dplyr::mutate(.pred_lower = NA_real_, .pred_upper = NA_real_)
  }

  ret
}

# summary_survfit helpers -------------------------------------------------

survfit_summary_typestable <- function(object) {
  # make matrix of dimension n_times x n_obs
  sanitize_element <- function(x, n_obs) {
    if (!is.matrix(x)) {
      x <- matrix(x, ncol = n_obs)
    }
    x
  }
  # sanitize elements we care about
  elements <- available_survfit_summary_elements(object)
  for (i in elements) {
    object[[i]] <- sanitize_element(object[[i]], n_obs = length(object$n))
  }

  object
}

available_survfit_summary_elements <- function(object) {
  intersect(
    names(object),
    c("surv", "std.err", "lower", "upper", "cumhaz", "std.chaz")
  )
}

survfit_summary_patch_infinite_time <- function(object, eval_time) {
  time_neg_inf <- is.infinite(eval_time) & (eval_time < 0)
  time_inf <- is.infinite(eval_time) & (eval_time > 0)

  patch_neg_inf <- function(x, value, n_patch) {
    rbind(
      matrix(value, nrow = n_patch, ncol = ncol(x)),
      x
    )
  }
  patch_inf <- function(x, value, n_patch) {
    rbind(
      x,
      matrix(value, nrow = n_patch, ncol = ncol(x))
    )
  }

  # glmnet does not provide standard errors etc
  has_std_error <- "std.err" %in% names(object)

  if (any(time_neg_inf)) {
    object$surv <- patch_neg_inf(
      object$surv,
      value = 1,
      n_patch = sum(time_neg_inf)
    )
    object$cumhaz <- patch_neg_inf(
      object$cumhaz,
      value = 0,
      n_patch = sum(time_neg_inf)
    )
    if (has_std_error) {
      object$std.err <- patch_neg_inf(
        object$std.err,
        value = NA_real_,
        n_patch = sum(time_neg_inf)
      )
      object$lower <- patch_neg_inf(
        object$lower,
        value = NA_real_,
        n_patch = sum(time_neg_inf)
      )
      object$upper <- patch_neg_inf(
        object$upper,
        value = NA_real_,
        n_patch = sum(time_neg_inf)
      )
      object$std.chaz <- patch_neg_inf(
        object$std.chaz,
        value = NA_real_,
        n_patch = sum(time_neg_inf)
      )
    }
  }
  if (any(time_inf)) {
    object$surv <- patch_inf(object$surv, value = 0, n_patch = sum(time_inf))
    object$cumhaz <- patch_inf(
      object$cumhaz,
      value = 1,
      n_patch = sum(time_inf)
    )
    if (has_std_error) {
      object$std.err <- patch_inf(
        object$std.err,
        value = NA_real_,
        n_patch = sum(time_inf)
      )
      object$lower <- patch_inf(
        object$lower,
        value = NA_real_,
        n_patch = sum(time_inf)
      )
      object$upper <- patch_inf(
        object$upper,
        value = NA_real_,
        n_patch = sum(time_inf)
      )
      object$std.chaz <- patch_inf(
        object$std.chaz,
        value = NA_real_,
        n_patch = sum(time_inf)
      )
    }
  }

  object
}

survfit_summary_restore_time_order <- function(object, eval_time) {
  # preserve original order of `eval_time` because `summary()` returns a result for
  # an ordered vector of finite time
  # Note that this requires a survfit summary object which has already been
  # patched for infinite time points
  original_order_time <- match(eval_time, sort(eval_time))

  elements <- available_survfit_summary_elements(object)

  # restore original order of prediction time points
  for (i in elements) {
    object[[i]] <- object[[i]][original_order_time, , drop = FALSE]
  }

  object
}

survfit_summary_patch_missings <- function(object, index_missing, eval_time, n_obs) {
  if (is.null(index_missing)) {
    return(object)
  }

  patch_element <- function(x, eval_time, n_obs, index_missing) {
    full_matrix <- matrix(NA, nrow = length(eval_time), ncol = n_obs)
    full_matrix[, -index_missing] <- x
    full_matrix
  }

  elements <- available_survfit_summary_elements(object)

  for (i in elements) {
    object[[i]] <- patch_element(
      object[[i]],
      eval_time = eval_time,
      n_obs = n_obs,
      index_missing = index_missing
    )
  }

  object
}

survfit_summary_to_tibble <- function(object, eval_time, n_obs) {
  ret <- tibble::tibble(
    .row = rep(seq_len(n_obs), each = length(eval_time)),
    .eval_time = rep(eval_time, times = n_obs),
    .pred_survival = as.vector(object$surv),
    # TODO standard error
    .pred_lower = as.vector(object$lower),
    .pred_upper = as.vector(object$upper),
    .pred_hazard_cumulative = as.vector(object$cumhaz)
    # TODO standard error for cumulative hazard
  )
  ret
}

survfit_summary_to_patched_tibble <- function(object, index_missing, eval_time, n_obs) {
  object %>%
    summary(times = eval_time, extend = TRUE) %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_infinite_time(eval_time = eval_time) %>%
    survfit_summary_restore_time_order(eval_time = eval_time) %>%
    survfit_summary_patch_missings(
      index_missing = index_missing,
      eval_time = eval_time,
      n_obs = n_obs
    ) %>%
    survfit_summary_to_tibble(eval_time = eval_time, n_obs = n_obs)
}

combine_list_of_survfit_summary <- function(object, eval_time) {
  n_time <- sum(is.finite(eval_time))
  elements <- available_survfit_summary_elements(object[[1]])

  ret <- list()
  for (i in elements) {
    ret[[i]] <- purrr::map(object, purrr::pluck, i) %>%
      unlist() %>%
      matrix(nrow = n_time)
  }

  ret
}

survfit_summary_patch <- function(object, index_missing, eval_time, n_obs) {
  object %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_infinite_time(eval_time = eval_time) %>%
    survfit_summary_restore_time_order(eval_time = eval_time) %>%
    survfit_summary_patch_missings(
      index_missing = index_missing,
      eval_time = eval_time,
      n_obs = n_obs
    )
}
