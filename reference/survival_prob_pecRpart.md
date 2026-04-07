# A wrapper for survival probabilities with pecRpart models

A wrapper for survival probabilities with pecRpart models

## Usage

``` r
survival_prob_pecRpart(object, new_data, eval_time)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [decision_tree() with
  engine =
  "rpart"](https://parsnip.tidymodels.org/reference/details_decision_tree_rpart.html).

- new_data:

  Data for prediction.

- eval_time:

  A vector of integers for prediction times.

## Value

A tibble with a list column of nested tibbles.

## Examples

``` r
mod <- decision_tree() |>
  set_mode("censored regression") |>
    set_engine("rpart") |>
    fit(Surv(time, status) ~ ., data = lung)
survival_prob_pecRpart(mod, new_data = lung[1:3, ], eval_time = 300)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
```
