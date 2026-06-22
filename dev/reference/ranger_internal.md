# Internal helper functions for ranger survival models

Internal helper functions for ranger survival models

## Usage

``` r
survival_time_ranger(object, new_data)

survival_prob_ranger(object, new_data, eval_time)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [rand_forest() with engine
  =
  "ranger"](https://parsnip.tidymodels.org/reference/details_rand_forest_ranger.html).

- new_data:

  A data frame to be predicted.

- eval_time:

  A vector of times to predict the survival probability.

## Value

- `survival_time_ranger()`: A numeric vector of predicted survival
  times.

- `survival_prob_ranger()`: A tibble with a list column of nested
  tibbles.

## Examples

``` r
mod <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("censored regression") |>
  fit(Surv(time, status) ~ age + ph.ecog, data = na.omit(lung))
survival_time_ranger(mod, new_data = lung[1:3, ])
#> [1] 643 428 337
mod <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("censored regression") |>
  fit(Surv(time, status) ~ age + ph.ecog, data = na.omit(lung))
survival_prob_ranger(mod, new_data = lung[1:3, ], eval_time = c(250, 100))
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [2 × 2]>
#> 2 <tibble [2 × 2]>
#> 3 <tibble [2 × 2]>
```
