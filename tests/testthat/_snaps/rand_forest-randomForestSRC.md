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

# prediction helpers error informatively on bad input

    Code
      survival_time_rfsrc(raw_fit, new_data = lung[1:3, ])
    Condition
      Error in `survival_time_rfsrc()`:
      ! `object` must be a <model_fit> object, not a <rfsrc> object.

---

    Code
      survival_time_rfsrc(wrong_engine, new_data = lung[1:3, ])
    Condition
      Error in `survival_time_rfsrc()`:
      ! `object$fit` must be a <rfsrc> object, not a <coxph> object.

---

    Code
      survival_prob_rfsrc(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_rfsrc()`:
      ! `object` must be a <model_fit> object, not a <rfsrc> object.

---

    Code
      survival_prob_rfsrc(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_rfsrc()`:
      ! `object$fit` must be a <rfsrc> object, not a <coxph> object.

