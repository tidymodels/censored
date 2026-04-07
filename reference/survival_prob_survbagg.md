# A wrapper for survival probabilities with `survbagg` models

A wrapper for survival probabilities with `survbagg` models

## Usage

``` r
survival_prob_survbagg(object, new_data, eval_time, time = deprecated())
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [bag_tree() with engine =
  "rpart"](https://parsnip.tidymodels.org/reference/details_bag_tree_rpart.html).

- new_data:

  Data for prediction.

- eval_time:

  A vector of prediction times.

- time:

  Deprecated in favor of `eval_time`. A vector of prediction times.

## Value

A vctrs list of tibbles.

## Examples

``` r
bagged_tree <- bag_tree() |>
  set_engine("rpart") |>
  set_mode("censored regression") |>
  fit(Surv(time, status) ~ age + ph.ecog, data = lung)
survival_prob_survbagg(bagged_tree, lung[1:3, ], eval_time = 100)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
```
