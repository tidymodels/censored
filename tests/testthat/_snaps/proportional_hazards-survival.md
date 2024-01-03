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

