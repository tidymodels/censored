#' censored: parsnip Engines for Survival Models
#'
#' censored provides engines for survival models from the parsnip package.
#' The models include parametric survival models, proportional hazards models,
#' decision trees, boosted trees, bagged trees, and random forests. See the
#' "Fitting and Predicting with censored" article for various examples. See
#' below for examples of classic survival models and how to fit them with
#' censored.
#'
#' @examples
#' # Accelerated Failure Time (AFT) model
#'
#' fit_aft <- survival_reg(dist = "weibull") %>%
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ age + sex + ph.karno, data = lung)
#' predict(fit_aft, lung[1:3, ], type = "time")
#'
#'
#' # Cox's Proportional Hazards model
#'
#' fit_cox <- proportional_hazards() %>%
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ age + sex + ph.karno, data = lung)
#' predict(fit_cox, lung[1:3, ], type = "time")
#'
#'
#' # Andersen-Gill model for recurring events
#'
#' fit_ag <- proportional_hazards() %>%
#'   set_engine("survival") %>%
#'   fit(Surv(tstart, tstop, status) ~ treat + inherit + age + strata(hos.cat),
#'     data = cgd
#'   )
#' predict(fit_ag, cgd[1:3, ], type = "time")
#'
"_PACKAGE"

## usethis namespace: start
#' @importFrom dials parameters
#' @importFrom dplyr %>%
#' @importFrom generics fit fit_xy
#' @importFrom hardhat tune
#' @importFrom lifecycle deprecated
#' @importFrom parsnip eval_args multi_predict predict_raw predict_survival
#' @importFrom parsnip predict_linear_pred
#' @importFrom parsnip predict.model_fit translate
#' @importFrom purrr map map_dbl
#' @importFrom rlang %||%
#' @importFrom rlang abort call2 empty_env enquos eval_tidy expr is_call
#' @importFrom rlang new_quosure check_dots_empty caller_env
#' @importFrom stats na.exclude na.pass predict quantile setNames
#' @importFrom survival strata
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr gather pivot_longer
#' @importFrom utils getFromNamespace
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    "eval_time", ".time", "object", "new_data", ".label", ".pred", ".cuts",
    ".id", ".tmp", "engine", "predictor_indicators", ".strata", "group",
    ".pred_quantile", ".quantile", "interval", "level", ".pred_linear_pred",
    ".pred_link", ".pred_time", ".pred_survival", "next_event_time",
    "sum_component", "time_interval"
  )
)

# quiet R-CMD-check NOTEs that prodlim is unused
# (parsnip uses it for all censored regression models 
# but only has it in Suggests)
#' @importFrom prodlim prodlim
NULL
