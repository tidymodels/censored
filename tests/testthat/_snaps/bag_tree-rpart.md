# survival_time_survbagg() throws an informative error with an engine object

    Code
      survival_time_survbagg(mod)
    Condition
      Error in `survival_time_survbagg()`:
      ! `object` needs to be a parsnip <model_fit> object, not a <survbagg> object.

# survival predictions - error snapshot

    Code
      predict(f_fit, lung, type = "survival")
    Condition
      Error in `predict()`:
      ! When using `type` values of "survival" or "hazard" a numeric vector `eval_time` should also be given.

