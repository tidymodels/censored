# Internal helper function for aorsf objects

Internal helper function for aorsf objects

## Usage

``` r
survival_prob_orsf(object, new_data, eval_time, time = deprecated())
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [rand_forest() with engine
  =
  "aorsf"](https://parsnip.tidymodels.org/reference/details_rand_forest_aorsf.html).

- new_data:

  A data frame to be predicted.

- eval_time:

  A vector of times to predict the survival probability.

- time:

  Deprecated in favor of `eval_time`. A vector of times to predict the
  survival probability.

## Value

A tibble with a list column of nested tibbles.

## Examples

``` r
mod <- rand_forest() |>
  set_engine("aorsf") |>
  set_mode("censored regression") |>
  fit(Surv(time, status) ~ age + ph.ecog, data = na.omit(lung))
preds <- survival_prob_orsf(mod, lung[1:3, ], eval_time = c(250, 100))
```
