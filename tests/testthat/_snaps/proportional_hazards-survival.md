# prediction from stratified models require strata variables in new_data

    Code
      predict(f_fit, new_data = dplyr::select(lung, -inst))
    Condition
      Error in `predict_time()`:
      ! `new_data` is missing the following stratification variable: `inst`.

---

    Code
      predict(f_fit, new_data = dplyr::select(lung, -inst, -ph.ecog))
    Condition
      Error in `predict_time()`:
      ! `new_data` is missing the following stratification variables: `inst` and `ph.ecog`.

# survival predictions - error snapshot

    Code
      predict(f_fit, lung, type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

# survival predictions with strata

    Code
      predict(f_fit, new_data = new_data_s, type = "survival", eval_time = 20)
    Condition
      Error in `predict_survival()`:
      ! `new_data` is missing the following stratification variable: `enum`.

# survival_time_coxph() errors informatively on bad input

    Code
      survival_time_coxph(raw_fit)
    Condition
      Error in `survival_time_coxph()`:
      ! `object` must be a <model_fit> object, not a <coxph> object.

---

    Code
      survival_time_coxph(wrong_engine)
    Condition
      Error in `survival_time_coxph()`:
      ! `object$fit` must be a <coxph> object, not a <survreg> object.

# survival_prob_coxph() errors informatively on bad input

    Code
      survival_prob_coxph(raw_fit, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_coxph()`:
      ! `object` must be a <model_fit> object, not a <coxph> object.

---

    Code
      survival_prob_coxph(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
    Condition
      Error in `survival_prob_coxph()`:
      ! `object$fit` must be a <coxph> object, not a <survreg> object.

# survival_prob_coxph() warns about deprecated `time` argument

    Code
      pred_deprecated <- survival_prob_coxph(mod, new_data = new_data, time = 100)
    Condition
      Warning:
      The `time` argument of `survival_prob_coxph()` is deprecated as of censored 0.2.0.
      i Please use the `eval_time` argument instead.

# survival_prob_coxph() errors about deprecated `x` argument

    Code
      survival_prob_coxph(mod, x = mod$fit, new_data = lung[1:2, ], eval_time = 100)
    Condition
      Error:
      ! The `x` argument of `survival_prob_coxph()` was deprecated in censored 0.3.0 and is now defunct.
      i Please use the `object` argument instead.

# survival_prob_coxph() fails gracefully for eval_time values it can't handle

    Code
      survival_prob_coxph(cox_mod, new_data = lung[1:2, ], eval_time = numeric(0))
    Condition
      Error in `survival_prob_coxph()`:
      ! `eval_time` can't be empty.

---

    Code
      survival_prob_coxph(cox_mod, new_data = lung[1:2, ], eval_time = c(100, NA))
    Condition
      Error in `survival_prob_coxph()`:
      ! `eval_time` can't contain missing values.

