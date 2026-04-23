# survival predictions - error snapshot

    Code
      predict(f_fit, lung_orsf, type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# survival_prob_orsf() errors informatively on bad input

    Code
      survival_prob_orsf(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_orsf()`:
      ! `object` must be a <model_fit> object, not a <ObliqueForestSurvival> object.

---

    Code
      survival_prob_orsf(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_orsf()`:
      ! `object$fit` must be a <ObliqueForestSurvival> object, not a <coxph> object.

# survival_prob_orsf() fails gracefully for eval_time values it can't handle

    Code
      survival_prob_orsf(mod, new_data = lung[1:2, ], eval_time = c(100, NA))
    Condition
      Error in `survival_prob_orsf()`:
      ! `eval_time` can't contain missing values.

---

    Code
      survival_prob_orsf(mod, new_data = lung[1:2, ], eval_time = c(100, -Inf))
    Condition
      Error in `survival_prob_orsf()`:
      ! `eval_time` can't contain negative values.

---

    Code
      survival_prob_orsf(mod, new_data = lung[1:2, ], eval_time = c(100, -50))
    Condition
      Error in `survival_prob_orsf()`:
      ! `eval_time` can't contain negative values.

