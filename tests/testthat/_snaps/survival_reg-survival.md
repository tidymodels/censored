# survival probability prediction

    Code
      predict(res, head(lung), type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# survival hazard prediction

    Code
      predict(res, head(lung), type = "hazard")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# survival_prob_survreg() errors informatively on bad input

    Code
      survival_prob_survreg(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_survreg()`:
      ! `object` must be a <model_fit> object, not a <survreg> object.

---

    Code
      survival_prob_survreg(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_survreg()`:
      ! `object$fit` must be a <survreg> object, not a <coxph> object.

# hazard_survreg() errors informatively on bad input

    Code
      hazard_survreg(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `hazard_survreg()`:
      ! `object` must be a <model_fit> object, not a <survreg> object.

---

    Code
      hazard_survreg(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `hazard_survreg()`:
      ! `object$fit` must be a <survreg> object, not a <coxph> object.

