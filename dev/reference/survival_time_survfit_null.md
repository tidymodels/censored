# A wrapper for survival times with null models

A wrapper for survival times with null models

## Usage

``` r
survival_time_survfit_null(object, new_data)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from a `null_model()` with
  engine = "survival".

- new_data:

  Data for prediction.

## Value

A numeric vector of predicted median survival times.

## Examples

``` r
mod <- parsnip::null_model() |>
  parsnip::set_engine("survival") |>
  parsnip::set_mode("censored regression") |>
  fit(survival::Surv(time, status) ~ ., data = survival::lung)
survival_time_survfit_null(mod, new_data = survival::lung[1:3, ])
#> [1] 310 310 310
```
