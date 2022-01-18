# code for the reference values in `testthat/data/`:
# expected parametric probability results form the rms package for row 1 of
# the lung data at times 1, 500, and 1000 using weibull model

library(rms)
mod <- psm(Surv(time, status) ~ age + sex, data = lung, dist = "weibull")
lambda <- Hazard(mod)
survival_fun <- Survival(mod)
lp <- mod$linear.predictors

prob_times <- c(0, 500, 1000)
rms_surv <- unname(survival_fun(prob_times, lp[1]))
rms_haz <- lambda(prob_times, lp[1])

# we expect the hazard at time = 0 to be 0
rms_haz[1] <- 0

saveRDS(rms_surv, "tests/testthat/data/rms_surv.rds")
saveRDS(rms_haz, "tests/testthat/data/rms_haz.rds")
