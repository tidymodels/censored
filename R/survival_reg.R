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

  if (!is.null(x$method$fit$args)) {
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
  res <- summary(object, newdata = new_data, type = type, t = .time, ci = FALSE)
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

deparse_survreg_strata <- function(object, new_data) {
  trms <- object$terms
  new_new_data <-
    stats::model.frame(trms,
                       data = new_data,
                       na.action = na.pass,
                       xlev = object$xlevels)
  strata_info <- survival::untangle.specials(trms, "strata", 1)
  new_new_data$.strata <-
    survival::strata(new_new_data[, strata_info$vars], shortlabel = TRUE)
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


survreg_survival <- function(location, object, time, scale, .time, ...) {
  distr <- object$dist
  tibble::tibble(
    .time = .time,
    .pred_survival = 1 - survival::psurvreg(.time, location, distribution = distr, scale, ...)
  )
}

#' @export
#' @rdname flexsurv_probs
survreg_survival_probs <- function(object, new_data, .time) {
  lp_estimate <- predict(object, new_data, type = "lp")
  scale_estimate <- get_survreg_scale(object, new_data)
  res <-
    purrr::map2(
      lp_estimate,
      scale_estimate,
      ~ survreg_survival(.x, object = object, .time = .time, scale = .y)
    )
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
  lp_estimate <- predict(object, new_data, type = "lp")
  scale_estimate <- get_survreg_scale(object, new_data)
  res <-
    purrr::map2(
      lp_estimate,
      scale_estimate,
      ~ survreg_hazard(.x, object = object, .time = .time, scale = .y)
    )
  tibble::tibble(.pred = unname(res))
}
