# A wrapper for survival time predictions with pecRpart models

Returns the median survival time of each leaf's Kaplan-Meier curve, as
stored in `pecRpart`'s stratified `prodlim` fit. `Inf` is returned when
the leaf's KM never crosses 0.5.

## Usage

``` r
survival_time_pecRpart(object, new_data)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [decision_tree() with
  engine =
  "rpart"](https://parsnip.tidymodels.org/reference/details_decision_tree_rpart.html).

- new_data:

  Data for prediction.

## Value

A numeric vector of predicted survival times.

## Examples

``` r
mod <- decision_tree() |>
  set_mode("censored regression") |>
    set_engine("rpart") |>
    fit(Surv(time, status) ~ ., data = lung)
survival_time_pecRpart(mod, new_data = lung[1:3, ])
#> [1] 230 230 371
```
