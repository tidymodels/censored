# survival predictions - error snapshot

    Code
      predict(f_fit, lung_ranger, type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# can handle case weights

    Code
      wt_fit$fit$call
    Output
      ranger::ranger(formula = Surv(time, event) ~ ., data = data, 
          num.trees = ~100, seed = ~1, num.threads = 1, verbose = FALSE, 
          case.weights = weights)

