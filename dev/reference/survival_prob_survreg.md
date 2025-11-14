# Internal function helps for parametric survival models

Internal function helps for parametric survival models

## Usage

``` r
survival_prob_survreg(object, new_data, eval_time, time = deprecated())

hazard_survreg(object, new_data, eval_time)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [survival_reg() with
  engine =
  "survival"](https://parsnip.tidymodels.org/reference/details_survival_reg_survival.html).

- new_data:

  A data frame.

- eval_time:

  A vector of time points.

- time:

  Deprecated in favor of `eval_time`. A vector of time points.

## Value

A tibble with a list column of nested tibbles.

## Examples

``` r
mod <- survival_reg() |>
  set_engine("survival") |>
  fit(Surv(time, status) ~ ., data = lung)
survival_prob_survreg(mod, lung[1:3, ], eval_time = 100)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
hazard_survreg(mod, lung[1:3, ], eval_time = 100)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
```
