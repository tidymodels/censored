ctrl          <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl   <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl    <- control_parsnip(verbosity = 0, catch = TRUE)

run_glmnet <- utils::compareVersion("3.6.0", as.character(getRversion())) > 0

# ------------------------------------------------------------------------------
# expected parametric probability results form the rms package for row 1 of
# the lung data at times 1, 500, and 1000 using weibull model

prob_times <- c(0, 500, 1000)
rms_surv <- c(0.999512204866043, 0.156925422832357, 0.00962303155994444)
rms_haz <- c(0.000647057266134788, 0.00491209408618748, 0.00615819918734245)
