# ------------------------------------------------------------------------------
# Helper functions to compute inverse probability of censoring weights (ipcw)

## A few general helpers:

.extract_time <- function(x) {
  x[, "time"]
}

.extract_status <- function(x) {
  res <-   x[, "status"]
  un_vals <- sort(unique(res))
  if (identical(un_vals, 1:2) | identical(un_vals, c(1.0, 2.0))) {
    res <- res - 1
  }
  res
}

collect_surv_column <- function(data, truth) {
  # We rename this here but might want to keep it as the original name for when
  # the predictions are returned in the `.prediction` column in the tune
  # package
  surv_df <- dplyr::select(data, !!truth) %>% stats::setNames("surv")

  if (!inherits(surv_df[[1]], "Surv")) {
    rlang::abort("'truth' must correspond to a single column of class 'Surv'")
  }
  surv_df
}

# We will add an extra class to the weight column so that we can check to see if
# the correct weights are used for different performance metrics
add_wt_class <- function(x, cls) {
  class(x) <- c(class(x), cls)
  x
}

# ------------------------------------------------------------------------------
# Determine the time at which the probability of censoring is computed.
#
# Graf et al (1999) use a combination of the observed event time and the time
# at which the analysis is conducted. That paper is focused on the Brier
# score metric. The function below encodes that logic.
#
# Uno et al (2007) show equations for estimating the area under the ROC curve
# an only use the observed event times to compute the censoring probability.
# For this reason, here is no extra function to figure out the time points.

graf_weight_time <- function(surv_obj, predict_time, eps = 10^-10) {
  # TODO check for right censoring?
  event_time <- .extract_time(surv_obj)
  status <- .extract_status(surv_obj)
  is_event_before_t <- event_time <= predict_time & status == 1
  is_censored <- event_time > predict_time

  # Three possible contributions to the statistic from Graf 1999

  # Censoring time before predict_time, no contribution (Graf category 3)
  weight_time <- rep(NA_real_, length(event_time))

  # A real event prior to predict time (Graf category 1)
  weight_time[is_event_before_t] <- event_time[is_event_before_t] - eps

  # Observed time greater than predict_time (Graf category 2)
  weight_time[is_censored] <- predict_time

  tibble::tibble(surv = surv_obj, weight_time = weight_time, .time = predict_time) %>%
    parsnip::add_rowindex()
}

# ------------------------------------------------------------------------------
# Computing IPCW for Brier and ROC AUC metrics

#' Inverse Probability of Censoring Weights (IPCW) for different metrics
#'
#' These functions compute weights that are required for certain dynamic
#' measures of model performance when the outcome is a censored event time.
#'
#' @param data A data frame with a column that corresponds to an object
#' created by [survival::Surv()] for right censored event times.
#' @param truth An unquoted column name for the `Surv` object.
#' @param analysis_time A vector of times at which the analysis will take
#' place. Note that, for the current censoring model that is used, these
#' values should be less than the largest observed event time.
#' @param model_fit A parsnip model object for a censored regression model. The
#' model should have been trained with parsnip version 1.0.4 or greater.
#' @return Both functions return a column called `.censoring_weight`. Since the
#' Brier score weight calculations are dependent on the time of analysis, it
#' returns a list column with the weights for the ordered time values. The ROC
#' function is independent of analysis time so it returns a numeric column of
#' weights.
#'
#' Additional classes are given to the  `.censoring_weight` column so that we
#' can check that the correct weighting scheme is used for the requested
#' metric.
#'
#' @details
#' The weights use a "reverse Kaplan-Meier" curve to compute the estimates of
#' the probability that a data point is censored at a specific time. The
#' calculation assumes an uninformative censoring method. These values are
#' estimated with the training set data when the primary survival model is
#' trained.
#'
#' Graf _et al_ (1999) use a combination of the observed event time and the time
#' at which the analysis is conducted. That paper is focused on the Brier
#' score metric. The function below encodes that logic.
#'
#' Uno _et al_ (2007) show equations for estimating the area under the ROC curve
#' that only use the observed event times to compute the censoring probability.
#'
#' Note that there are a few reasons that the weights may be estimated as `NA`.
#' First, is when the time of analysis is greater than the larges observed time in
#' the training set. Second, for the case of Graf _et al_ (1999), any data points
#' where the censored value is prior to the time of analysis (labeled as
#' "category 3") should not contribute to the analysis since their status at
#' the time of analysis is unknown.
#'
#'
#' @references
# Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999),
# Assessment and comparison of prognostic classification schemes for survival
# data. _Statist. Med._, 18: 2529-2545. (see Section 6)
#'
#' Hajime Uno, Tianxi Cai, Lu Tian & L. J Wei (2007) Evaluating Prediction Rules
#' for t-Year Survivors With Censored Regression Models, _Journal of the
#' American Statistical Association_, 102:478, 527-537. (see 5.1, equation 10)
#' @name IPCW
#' @export
brier_survival_weights <- function(data, truth, analysis_time, model_fit) {
  # TODO Add some checks in here for various things

  # TODO when the censoring model has predictors, we can get these from the
  # model object and subset separately. We don't support this yet so this is
  # a reminder if/when we add that feature
  truth <- rlang::enquo(truth)
  surv_data <- collect_surv_column(data, truth)
  purrr::map_dfr(analysis_time, ~ graf_weight_time(surv_data$surv, .x))  %>%
    dplyr::mutate(
      cens_prob = predict(mod_fit$censor_probs, time = weight_time, as_vector = TRUE),
      .censoring_weight = 1 / cens_prob,
      .censoring_weight = add_wt_class(.censoring_weight, "brier_survival_weights")
    ) %>%
    # TODO We might want to leave .time in here but it is also in .predictions
    # so unnesting will cause a warning and re-labeling of one of the duplicate
    # columns.
    dplyr::select(.row, .censoring_weight, .time) %>%
    dplyr::select(-.time) %>%
    tidyr::nest(.censoring_weight = c(.censoring_weight)) %>%
    dplyr::select(-.row)
}


#' @export
#' @rdname IPCW
roc_survival_weights <- function(data, truth, analysis_time, model_fit) {
  # These are independent of `analysis_time` but the argument is there for
  # consistency with the other function.
  truth <- rlang::enquo(truth)
  collect_surv_column(data, truth) %>%
    dplyr::mutate(
      weight_time = .extract_time(surv),
      cens_prob = predict(mod_fit$censor_probs, time = weight_time, as_vector = TRUE),
      .censoring_weight = 1 / cens_prob,
      .censoring_weight = add_wt_class(.censoring_weight, "roc_survival_weights")
    ) %>%
    dplyr::select(.censoring_weight)
}

# ------------------------------------------------------------------------------
#' Tool for reorganizing dynamic survival predictions with censoring weights
#' @param data A data frame with columns `.censoring_weight` and `.pred`
#' @param ... Not currently used.
#' @export
unnest_survival_probs <- function(data, ...) {
  reqq <- c(".censoring_weight", ".pred")
  nm_check <- sort(intersect(reqq, names(brier_data)))
  if (!identical(reqq, nm_check)) {
    rlang::abort(
      paste("'data' should have columns:", paste0("`", reqq, "`", collapse = " and "))
    )
  }
  dots <- rlang::enquos(...)
  ipcw_list <- is.list(data$.censoring_weight[1])
  if (ipcw_list) {
    data <-
      data %>%
      parsnip::add_rowindex() %>%
      tidyr::unnest(c(.censoring_weight, .pred))
  } else {
    data <-
      data %>%
      parsnip::add_rowindex() %>%
      tidyr::unnest(c(.pred))
  }

  data <-
    data %>%
    dplyr::relocate(.row, .time, .before = c(.censoring_weight)) %>%
    dplyr::group_by(.time)

  if (length(dots) > 0) {
    data <-
      data %>%
      dplyr::select( !!!dots, .time, .censoring_weight, dplyr::starts_with(".pred"))
  }
  data
}




