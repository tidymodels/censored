# A wrapper for mean survival times with `mboost` models

A wrapper for mean survival times with `mboost` models

## Usage

``` r
survival_time_mboost(object, new_data)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [boost_tree() with engine
  =
  "mboost"](https://parsnip.tidymodels.org/reference/details_boost_tree_mboost.html).

- new_data:

  Data for prediction

## Value

A tibble.

## Examples

``` r
boosted_tree <- boost_tree() |>
  set_engine("mboost") |>
  set_mode("censored regression") |>
  fit(Surv(time, status) ~ age + ph.ecog, data = lung[-14, ])
survival_time_mboost(boosted_tree, new_data = lung[1:3, ])
#> # A tibble: 3 × 1
#>   .pred_time
#>        <dbl>
#> 1       370.
#> 2       337.
#> 3       540.
```
