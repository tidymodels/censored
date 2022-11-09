# can handle case weights

    Code
      wt_fit$fit$call
    Output
      flexsurv::flexsurvspline(formula = Surv(time, status) ~ age + 
          sex, data = data, weights = weights, k = ~1)

