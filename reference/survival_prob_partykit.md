# A wrapper for survival probabilities with partykit models

A wrapper for survival probabilities with partykit models

## Usage

``` r
survival_prob_partykit(
  object,
  new_data,
  eval_time,
  time = deprecated(),
  output = "surv"
)
```

## Arguments

- object:

  A parsnip `model_fit` object resulting from [decision_tree() with
  engine =
  "partykit"](https://parsnip.tidymodels.org/reference/details_decision_tree_partykit.html)
  or [rand_forest() with engine =
  "partykit"](https://parsnip.tidymodels.org/reference/details_rand_forest_partykit.html).

- new_data:

  A data frame to be predicted.

- eval_time:

  A vector of times to predict the survival probability.

- time:

  Deprecated in favor of `eval_time`. A vector of times to predict the
  survival probability.

- output:

  Type of output. Can be either `"surv"` or `"haz"`.

## Value

A tibble with a list column of nested tibbles.

## Examples

``` r
tree <- decision_tree() |>
  set_mode("censored regression") |>
  set_engine("partykit") |>
  fit(Surv(time, status) ~ age + ph.ecog, data = lung)
survival_prob_partykit(tree, lung[1:3, ], eval_time = 100)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
forest <- rand_forest() |>
  set_mode("censored regression") |>
  set_engine("partykit") |>
  fit(Surv(time, status) ~ age + ph.ecog, data = lung[1:100, ])
survival_prob_partykit(forest, lung[1:3, ], eval_time = 100)
#> # A tibble: 3 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [1 × 2]>
#> 2 <tibble [1 × 2]>
#> 3 <tibble [1 × 2]>
```
