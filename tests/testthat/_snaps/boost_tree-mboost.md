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

# multi_predict() errors informatively on bad input

    Code
      multi_predict(f_fit, new_data = new_data_3, type = "survival", trees = c(25,
        100))
    Condition
      Error in `check_pred_type_dots()`:
      ! When using `type` values of 'survival' or 'hazard', a numeric vector `eval_time` should also be given.

---

    Code
      multi_predict(f_fit, new_data = new_data_3, type = "time", eval_time = 100,
        trees = c(25, 100))
    Condition
      Error in `check_pred_type_dots()`:
      ! `eval_time` should only be passed to `predict()` when `type` is one of: 'survival', 'hazard'

---

    Code
      multi_predict(f_fit, new_data = new_data_3, type = "time", trees = c(50, 10000))
    Condition
      Error in `multi_predict()`:
      ! `trees` values must not exceed the number of boosting iterations in the fitted model (100).
      i mboost would otherwise refit additional iterations.

---

    Code
      multi_predict(f_fit, new_data = new_data_3, type = "time", trees = c(0, 50))
    Condition
      Error in `multi_predict()`:
      ! `trees` must be a vector of positive integers.

---

    Code
      multi_predict(f_fit, new_data = new_data_3, type = "time", trees = c(1.5, 50))
    Condition
      Error in `multi_predict()`:
      ! `trees` must be a vector of positive integers.

