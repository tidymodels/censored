# A wrapper for survival probabilities with mboost models

A wrapper for survival probabilities with mboost models

## Usage

``` r
survival_prob_mboost(object, new_data, eval_time, time = deprecated())
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [boost_tree() with engine
  =
  "mboost"](https://parsnip.tidymodels.org/reference/details_boost_tree_mboost.html).

- new_data:

  Data for prediction.

- eval_time:

  A vector of integers for prediction times.

- time:

  Deprecated in favor of `eval_time`. A vector of integers for
  prediction times.

## Value

A tibble with a list column of nested tibbles.

## Examples

``` r
mod <- boost_tree() |>
  set_engine("mboost") |>
  set_mode("censored regression") |>
  fit(Surv(time, status) ~ ., data = lung)
survival_prob_mboost(mod, new_data = lung[1:3, ], eval_time = 300)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
```
