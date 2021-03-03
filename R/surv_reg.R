#' General Interface for Parametric Survival Models
#'
#' `survival_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  R. The main argument for the
#'  model is:
#' \itemize{
#'   \item \code{dist}: The probability distribution of the outcome.
#' }
#' This argument is converted to its specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to its default
#'  here (`NULL`), the value is taken from the underlying model
#'  functions.
#'
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `survival_reg()`,the
#'  mode will always be "regression".
#'
#'  Since survival models typically involve censoring (and require the use of
#'  [survival::Surv()] objects), the [fit()] function will require that the
#'  survival model be specified via the formula interface.
#'
#' Also, for the `flexsurv::flexsurvfit` engine, the typical
#'  `strata` function cannot be used. To achieve the same effect,
#'  the extra parameter roles can be used (as described above).
#'
#' @inheritParams parsnip::boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "censored regression".
#' @param dist A character string for the outcome distribution. "weibull" is
#'  the default.
#' @details
#' For `survival_reg()`, the mode will always be "censored regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"flexsurv"`, `"survival"` (the default)
#' }
#'
#' @seealso [fit()], [survival::Surv()]
#' @references Jackson, C. (2016). `flexsurv`: A Platform for Parametric Survival
#'  Modeling in R. _Journal of Statistical Software_, 70(8), 1 - 33.
#' @examples
#' survival_reg()
#' # Parameters can be represented by a placeholder:
#' survival_reg(dist = varying())
#'
#' @export
survival_reg <- function(mode = "censored regression", dist = NULL) {

  args <- list(
    dist = enquo(dist)
  )

  new_model_spec(
    "survival_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

#' @export
print.survival_reg <- function(x, ...) {
  cat("Parametric Survival Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' Update a Parametric Survival Regression Specification
#'
#' If parameters need to be modified, this function can be used
#'  in lieu of recreating the object from scratch.
#'
#' @inheritParams parsnip::update.boost_tree
#' @param object A survival regression model specification.
#' @examples
#' model <- survival_reg(dist = "weibull")
#' model
#' update(model, dist = "lnorm")
#' @method update survival_reg
#' @rdname survival_reg
#' @export
update.survival_reg <- function(object, parameters = NULL, dist = NULL, fresh = FALSE, ...) {

  eng_args <- update_engine_parameters(object$eng_args, ...)

  if (!is.null(parameters)) {
    parameters <- check_final_param(parameters)
  }

  args <- list(
    dist = enquo(dist)
  )

  args <- update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
    if (length(eng_args) > 0)
      object$eng_args[names(eng_args)] <- eng_args
  }

  new_model_spec(
    "survival_reg",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}


# ------------------------------------------------------------------------------

#' @export
translate.survival_reg <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'survival'` for translation.")
    engine <- "survival"
  }
  x <- translate.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

check_args.survival_reg <- function(object) {

  if (object$engine == "flexsurv") {

    args <- lapply(object$args, rlang::eval_tidy)

    # `dist` has no default in the function
    if (all(names(args) != "dist") || is.null(args$dist))
      object$args$dist <- "weibull"
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

#' @importFrom stats setNames
#' @importFrom dplyr mutate
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

#' @importFrom dplyr bind_rows
flexsurv_mean <- function(results, object) {
  results <- unclass(results)
  results <- dplyr::bind_rows(results)
  results$est
}

#' @importFrom stats setNames
flexsurv_quant <- function(results, object) {
  results <- purrr::map(results, as_tibble)
  names(results) <- NULL
  results <- purrr::map(results, setNames, c(".quantile", ".pred", ".pred_lower", ".pred_upper"))
}

#' Internal function helps for parametric survival models
#' @param object A `survreg` or `flexsurvreg` object.
#' @param new_data A data frame.
#' @param .time A vector of time points
#' @return A nested tibble with column name `.pred`
#' @keywords internal
#' @export
flexsurv_probs <- function(object, new_data, .time, type = "survival") {
  type <- rlang::arg_match(type, c("survival", "hazard"))
  res <- summary(object, newdata = new_data, type = type, t = .time)
  res <- unname(res)
  col_name <- rlang::sym(paste0(".pred_", type))
  res <- purrr::map(res, ~ dplyr::select(.x, time, est))
  res <- purrr::map(res, ~ setNames(.x, c(".time", col_name)))
  tibble::tibble(.pred = res)
}

# ------------------------------------------------------------------------------
# helpers for survreg prediction

survreg_survival <- function(location, object, time, scale = object$scale, .time, ...) {
  distr <- object$dist
  tibble::tibble(
    .time = .time,
    .pred_survival = 1 - survival::psurvreg(.time, location, distribution = distr, scale, ...)
  )
}

#' @export
#' @rdname flexsurv_probs
survreg_survival_probs <- function(object, new_data, .time) {
  lp_pred <- predict(object, new_data, type = "lp")
  res <- purrr::map(lp_pred, survreg_survival, object = object, .time = .time)
  tibble::tibble(.pred = unname(res))
}

survreg_hazard <- function(location, object, scale = object$scale, .time, ...) {
  distr <- object$dist
  prob <-
    survival::dsurvreg(.time, location, scale, distribution = distr, ...) /
    (1 - survival::psurvreg(.time, location, distribution = distr, scale, ...))
  tibble::tibble(
    .time = .time,
    .pred_hazard = prob
  )
}

#' @export
#' @rdname flexsurv_probs
survreg_hazard_probs <- function(object, new_data, .time) {
  lp_pred <- predict(object, new_data, type = "lp")
  res <- purrr::map(lp_pred, survreg_hazard, object = object, .time = .time)
  tibble::tibble(.pred = unname(res))
}

