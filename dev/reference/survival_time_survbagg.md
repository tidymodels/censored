# A wrapper for survival times with `survbagg` models

A wrapper for survival times with `survbagg` models

## Usage

``` r
survival_time_survbagg(object, new_data)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [bag_tree() with engine =
  "rpart"](https://parsnip.tidymodels.org/reference/details_bag_tree_rpart.html).

- new_data:

  Data for prediction

## Value

A vector.

## Examples

``` r
bagged_tree <- bag_tree() |>
  set_engine("rpart") |>
  set_mode("censored regression") |>
  fit(Surv(time, status) ~ age + ph.ecog, data = lung)
survival_time_survbagg(bagged_tree, lung[1:3, ])
#> [1] 310 310 429
```
