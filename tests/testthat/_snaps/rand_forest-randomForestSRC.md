# non-right-censored responses error

    Code
      fit(spec, y ~ age + ph.ecog, data = d_cr)
    Condition
      Error in `censored::rfsrc_train()`:
      ! The "randomForestSRC" engine only supports right-censored data, not data with censoring type "mright".

---

    Code
      fit(spec, y ~ age + ph.ecog, data = d_interval)
    Condition
      Error in `censored::rfsrc_train()`:
      ! The "randomForestSRC" engine only supports right-censored data, not data with censoring type "interval".

---

    Code
      fit(spec, y ~ age + ph.ecog, data = d_counting)
    Condition
      Error in `censored::rfsrc_train()`:
      ! The "randomForestSRC" engine only supports right-censored data, not data with censoring type "counting".

# survival predictions - error snapshot

    Code
      predict(f_fit, lung, type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

