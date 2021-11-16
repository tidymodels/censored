#' censored: Parsnip Engines for Survival Models
#'
#' `censored` provides engines for survival models from the `parsnip` package.
#'
#' @examples
#' library(survival)
#'
#' # Accelerated Failure Time (AFT) model
#'
#' fit_c <- survival_reg(engine = "survival", dist = "weibull") %>%
#'   fit(Surv(time, status) ~ age + sex + ph.karno, data = lung)
#' predict(fit_c, lung[1:3,], type = "time")
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
