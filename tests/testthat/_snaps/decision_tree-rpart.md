# survival predictions - error snapshot

    Code
      predict(f_fit, lung, type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# survival_prob_pecRpart() errors informatively on bad input

    Code
      survival_prob_pecRpart(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_pecRpart()`:
      ! `object` must be a <model_fit> object, not a <pecRpart> object.

---

    Code
      survival_prob_pecRpart(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_pecRpart()`:
      ! `object$fit` must be a <pecRpart> object, not a <rpart> object.

# survival_prob_pecRpart() fails gracefully for eval_time values it can't handle

    Code
      survival_prob_pecRpart(mod, new_data = lung[1:2, ], eval_time = numeric(0))
    Condition
      Error in `survival_prob_pecRpart()`:
      ! `eval_time` can't be empty.

---

    Code
      survival_prob_pecRpart(mod, new_data = lung[1:2, ], eval_time = c(100, NA))
    Condition
      Error in `survival_prob_pecRpart()`:
      ! `eval_time` can't contain missing values.

---

    Code
      survival_prob_pecRpart(mod, new_data = lung[1:2, ], eval_time = c(100, Inf))
    Condition
      Error in `survival_prob_pecRpart()`:
      ! `eval_time` can't contain infinite values.

---

    Code
      survival_prob_pecRpart(mod, new_data = lung[1:2, ], eval_time = c(100, -Inf))
    Condition
      Error in `survival_prob_pecRpart()`:
      ! `eval_time` can't contain infinite values.

