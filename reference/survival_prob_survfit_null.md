# A wrapper for survival probabilities with null models

A wrapper for survival probabilities with null models

## Usage

``` r
survival_prob_survfit_null(object, new_data, eval_time)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from a `null_model()` with
  engine = "survival".

- new_data:

  Data for prediction.

- eval_time:

  A vector of integers for prediction times.

## Value

A tibble with a list column of nested tibbles.

## Examples

``` r
mod <- parsnip::null_model() |>
  parsnip::set_engine("survival") |>
  parsnip::set_mode("censored regression") |>
  fit(survival::Surv(time, status) ~ ., data = survival::lung)
survival_prob_survfit_null(mod, new_data = survival::lung[1:3, ], eval_time = 300)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
```
