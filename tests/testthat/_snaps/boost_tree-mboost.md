# survival predictions - error snapshot

    Code
      predict(f_fit, lung, type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# survival_time_mboost() errors informatively on bad input

    Code
      survival_time_mboost(raw_fit)
    Condition
      Error in `survival_time_mboost()`:
      ! `object` must be a <model_fit> object, not a <blackboost> object.

---

    Code
      survival_time_mboost(wrong_engine)
    Condition
      Error in `survival_time_mboost()`:
      ! `object$fit` must be a <mboost> object, not a <coxph> object.

# survival_prob_mboost() errors informatively on bad input

    Code
      survival_prob_mboost(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_mboost()`:
      ! `object` must be a <model_fit> object, not a <blackboost> object.

---

    Code
      survival_prob_mboost(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_mboost()`:
      ! `object$fit` must be a <mboost> object, not a <coxph> object.

