# A wrapper for survival probabilities with coxph models

A wrapper for survival probabilities with coxph models

## Usage

``` r
survival_prob_coxph(
  object,
  x = deprecated(),
  new_data,
  eval_time,
  time = deprecated(),
  output = "surv",
  interval = "none",
  conf.int = 0.95,
  ...
)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [proportional_hazards()
  with engine =
  "survival"](https://parsnip.tidymodels.org/reference/details_proportional_hazards_survival.html).

- x:

  Deprecated. A model from `coxph()`.

- new_data:

  Data for prediction

- eval_time:

  A vector of integers for prediction times.

- time:

  Deprecated in favor of `eval_time`. A vector of integers for
  prediction times.

- output:

  One of `"surv"`, `"conf"`, or `"haz"`.

- interval:

  Add confidence interval for survival probability? Options are `"none"`
  or `"confidence"`.

- conf.int:

  The confidence level.

- ...:

  Options to pass to
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)

## Value

A tibble with a list column of nested tibbles.

## Examples

``` r
cox_mod <- proportional_hazards() |>
  set_engine("survival") |>
  fit(Surv(time, status) ~ ., data = lung)
survival_prob_coxph(cox_mod, new_data = lung[1:3, ], eval_time = 300)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
```
