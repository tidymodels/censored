# A wrapper for survival times with `coxph` models

A wrapper for survival times with `coxph` models

## Usage

``` r
survival_time_coxph(object, new_data)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [proportional_hazards()
  with engine =
  "survival"](https://parsnip.tidymodels.org/reference/details_proportional_hazards_survival.html).

- new_data:

  Data for prediction

## Value

A vector.

## Examples

``` r
cox_mod <- proportional_hazards() |>
  set_engine("survival") |>
  fit(Surv(time, status) ~ ., data = lung)
survival_time_coxph(cox_mod, new_data = lung[1:3, ])
#> [1]       NA 470.5813       NA
```
