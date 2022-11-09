# `fix_xy()` errors

    Code
      fit_xy(spec, x = lung_x, y = lung_y)
    Condition
      Error in `fit_xy()`:
      ! For the `'flexsurvspline'` engine, please use the formula interface via `fit()`.

# can handle case weights

    Code
      wt_fit$fit$call
    Output
      flexsurv::flexsurvspline(formula = Surv(time, status) ~ age + 
          sex, data = data, weights = weights, k = ~1)

