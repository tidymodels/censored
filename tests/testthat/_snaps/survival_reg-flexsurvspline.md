# survival probability prediction

    Code
      predict(f_fit, head(lung), type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# hazard prediction

    Code
      predict(f_fit, head(lung), type = "hazard")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# can handle case weights

    Code
      wt_fit$fit$call
    Output
      flexsurv::flexsurvspline(formula = Surv(time, status) ~ age + 
          sex, data = data, weights = weights, k = 1)

