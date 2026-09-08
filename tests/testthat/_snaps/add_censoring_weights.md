# predict() with add_censoring_weights = TRUE errors when new_data is missing the survival outcome

    Code
      predict(f_fit, new_data = no_outcome, type = "survival", eval_time = c(100, 200),
      add_censoring_weights = TRUE)
    Condition
      Error in `predict_survival()`:
      ! `add_censoring_weights = TRUE` requires the survival outcome in `new_data`.
      i Add a <Surv> column, or include the variables used in the model formula ("time" and "status").

# predict() with add_censoring_weights = TRUE errors when fit_xy was used and new_data has no Surv column

    Code
      predict(xy_fit, new_data = no_outcome, type = "survival", eval_time = c(100,
        200), add_censoring_weights = TRUE)
    Condition
      Error in `predict_survival()`:
      ! `add_censoring_weights = TRUE` requires the survival outcome in `new_data`.
      i Add a <Surv> column to `new_data`.

# add_censoring_weights validates the flag

    Code
      predict(f_fit, new_data = lung, type = "survival", eval_time = c(100, 200),
      add_censoring_weights = "yes")
    Condition
      Error in `predict_survival()`:
      ! `add_censoring_weights` must be `TRUE` or `FALSE`, not the string "yes".

