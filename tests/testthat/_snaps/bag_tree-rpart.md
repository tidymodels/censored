# survival predictions - error snapshot

    Code
      predict(f_fit, lung, type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# survival_time_survbagg() errors informatively on bad input

    Code
      survival_time_survbagg(raw_fit)
    Condition
      Error in `survival_time_survbagg()`:
      ! `object` must be a <model_fit> object, not a <survbagg> object.

---

    Code
      survival_time_survbagg(wrong_engine)
    Condition
      Error in `survival_time_survbagg()`:
      ! `object$fit` must be a <survbagg> object, not a <coxph> object.

# survival_prob_survbagg() errors informatively on bad input

    Code
      survival_prob_survbagg(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_survbagg()`:
      ! `object` must be a <model_fit> object, not a <survbagg> object.

---

    Code
      survival_prob_survbagg(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_survbagg()`:
      ! `object$fit` must be a <survbagg> object, not a <coxph> object.

# survival_prob_survbagg() fails gracefully for eval_time values it can't handle

    Code
      survival_prob_survbagg(mod, new_data = lung[1:2, ], eval_time = numeric(0))
    Condition
      Error in `survival_prob_survbagg()`:
      ! `eval_time` can't be empty.

---

    Code
      survival_prob_survbagg(mod, new_data = lung[1:2, ], eval_time = c(100, NA))
    Condition
      Error in `survival_prob_survbagg()`:
      ! `eval_time` can't contain missing values.

# survival_prob_survbagg() warns about deprecated `time` argument

    Code
      pred_deprecated <- survival_prob_survbagg(mod, new_data = new_data, time = 100)
    Condition
      Warning:
      The `time` argument of `survival_prob_survbagg()` is deprecated as of censored 0.2.0.
      i Please use the `eval_time` argument instead.

