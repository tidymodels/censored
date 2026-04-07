# A wrapper for survival times with coxnet models

A wrapper for survival times with coxnet models

## Usage

``` r
survival_time_coxnet(object, new_data, penalty = NULL, multi = FALSE, ...)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [proportional_hazards()
  with engine =
  "glmnet"](https://parsnip.tidymodels.org/reference/details_proportional_hazards_glmnet.html).

- new_data:

  Data for prediction.

- penalty:

  Penalty value(s).

- multi:

  Allow multiple penalty values?

- ...:

  Options to pass to
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

## Value

A vector.

## Examples

``` r
cox_mod <- proportional_hazards(penalty = 0.1) |>
  set_engine("glmnet") |>
  fit(Surv(time, status) ~ ., data = lung)
survival_time_coxnet(cox_mod, new_data = lung[1:3, ], penalty = 0.1)
#> [1]       NA 425.4722       NA
```
