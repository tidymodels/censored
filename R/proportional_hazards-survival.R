
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

cph_survival_pre <- function(new_data, object) {

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

    if (!all(strata %in% names(new_data))) {
      rlang::abort("Please provide the strata variable(s) in `new_data`.")
    }
  }

  new_data
}

# prediction: time --------------------------------------------------------

#' A wrapper for survival times with `coxph` models
#' @param object A model from `coxph()`.
#' @param new_data Data for prediction
#' @return A vector.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- coxph(Surv(time, status) ~ ., data = lung)
#' survival_time_coxph(cox_mod, new_data = lung[1:3, ])
survival_time_coxph <- function(object, new_data) {

  missings_in_new_data <- get_missings_coxph(object, new_data)
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


  y <- survival::survfit(object, new_data, na.action = stats::na.exclude)

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
  mod_frame <- stats::model.frame(trms, data = new_data, xlev = xlevels,
                                  na.action = stats::na.exclude)

  attr(mod_frame, "na.action")
}



# prediction: survival ----------------------------------------------------

#' A wrapper for survival probabilities with coxph models
#' @param x A model from `coxph()`.
#' @param new_data Data for prediction
#' @param time A vector of integers for prediction times.
#' @param output One of `"surv"`, `"conf"`, or `"haz"`.
#' @param interval Add confidence interval for survival probability? Options
#' are `"none"` or `"confidence"`.
#' @param conf.int The confidence level.
#' @param ... Options to pass to [survival::survfit()]
#' @return A tibble with a list column of nested tibbles.
#' @keywords internal
#' @export
#' @examples
#' cox_mod <- coxph(Surv(time, status) ~ ., data = lung)
#' survival_prob_coxph(cox_mod, new_data = lung[1:3, ], time = 300)
survival_prob_coxph <- function(x,
                                new_data,
                                time,
                                output = "surv",
                                interval = "none",
                                conf.int = .95,
                                ...) {
  interval <- rlang::arg_match(interval, c("none", "confidence"))
  output <- rlang::arg_match(output, c("surv", "conf", "haz"))
  if (output == "surv" & interval == "confidence") {
    output <- "survconf"
  }

  missings_in_new_data <- get_missings_coxph(x, new_data)
  if (!is.null(missings_in_new_data)) {
    n_total <- nrow(new_data)
    n_missing <- length(missings_in_new_data)
    all_missing <- n_missing == n_total
    if (all_missing) {
      ret <- predict_survival_na(time, interval)
      ret <- tibble(.pred = rep(list(ret), n_missing))
      return(ret)
    }
    new_data <- new_data[-missings_in_new_data, , drop = FALSE]
  }

  y <- survival::survfit(x, newdata = new_data, conf.int = conf.int,
                         na.action = na.exclude, ...)

  if (has_strata(x$terms)) {
    new_strata <- compute_strata(x, new_data) %>%
      dplyr::pull(.strata)
  } else {
    new_strata <- NULL
  }

  stacked_survfit <- stack_survfit(y, nrow(new_data))
  starting_rows <- stacked_survfit %>%
    dplyr::distinct(.row) %>%
    dplyr::bind_cols(prob_template)

  res <- dplyr::bind_rows(starting_rows, stacked_survfit) %>%
    interpolate_km_values(time, new_strata) %>%
    keep_cols(output) %>%
    tidyr::nest(.pred = c(-.row)) %>%
    dplyr::select(-.row)

  if (!is.null(missings_in_new_data)) {
    res <- pad_survival_na(res, missings_in_new_data, time,
                           interval, n_total)
  }
  res
}

