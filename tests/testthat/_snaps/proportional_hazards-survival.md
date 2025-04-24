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

