# prediction from stratified models require strata variables in new_data

    Code
      predict(f_fit, new_data = dplyr::select(lung, -inst))
    Condition
      Error in `predict_time()`:
      ! Please provide the strata variable(s) in `new_data`.

