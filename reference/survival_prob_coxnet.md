# A wrapper for survival probabilities with coxnet models

A wrapper for survival probabilities with coxnet models

## Usage

``` r
survival_prob_coxnet(
  object,
  new_data,
  eval_time,
  time = deprecated(),
  output = "surv",
  penalty = NULL,
  multi = FALSE,
  ...
)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [proportional_hazards()
  with engine =
  "glmnet"](https://parsnip.tidymodels.org/reference/details_proportional_hazards_glmnet.html).

- new_data:

  Data for prediction.

- eval_time:

  A vector of integers for prediction times.

- time:

  Deprecated in favor of `eval_time`. A vector of integers for
  prediction times.

- output:

  One of "surv" or "haz".

- penalty:

  Penalty value(s).

- multi:

  Allow multiple penalty values? Defaults to FALSE.

- ...:

  Options to pass to
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

## Value

A tibble with a list column of nested tibbles.

## Examples

``` r
cox_mod <- proportional_hazards(penalty = 0.1) |>
  set_engine("glmnet") |>
  fit(Surv(time, status) ~ ., data = lung)
survival_prob_coxnet(cox_mod, new_data = lung[1:3, ], eval_time = 300)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
```
