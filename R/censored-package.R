#' censored: Parsnip Engines for Survival Models
#'
#' `censored` provides engines for survival models from the `parsnip` package.
#' The models include parametric survival models, proportional hazards models,
#' decision trees, boosted trees, bagged trees, and random forests. See the
#' "Fitting and Predicting with censored" article for various examples. See
#' below for examples of classic survival models and how to fit them with
#' censored.
#'
#' @examples
#' library(survival)
#'
#' # Accelerated Failure Time (AFT) model
#'
#' fit_aft <- survival_reg(dist = "weibull") %>%
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ age + sex + ph.karno, data = lung)
#' predict(fit_aft, lung[1:3, ], type = "time")
#'
#' # Cox's Proportional Hazards model
#'
#' fit_cox <- proportional_hazards() %>%
#'   set_engine("survival") %>%
#'   fit(Surv(time, status) ~ age + sex + ph.karno, data = lung)
#' predict(fit_cox, lung[1:3, ], type = "time")
#'
#' # Andersen-Gill model for recurring events
#'
#' fit_ag <- proportional_hazards() %>%
#'   set_engine("survival") %>%
#'   fit(Surv(tstart, tstop, status) ~ treat + inherit + age, data = cgd)
#' predict(fit_ag, cgd[1:3, ], type = "time")
#'
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
