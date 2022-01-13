# code for the reference values in `testthat/helper-objects.R`:
# expected parametric probability results form the rms package for row 1 of
# the lung data at times 1, 500, and 1000 using weibull model

library(rms)
mod <- psm(Surv(time, status) ~ age + sex, data = lung, dist = "weibull")
lambda <- Hazard(mod)
survival_fun <- Survival(mod)
lp <- mod$linear.predictors

prob_times <- c(0, 500, 1000)
rms_surv <- survival_fun(prob_times, lp[1])
rms_haz <- lambda(prob_times, lp[1])
