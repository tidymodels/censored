# Internal helper functions for randomForestSRC survival models

Internal helper functions for randomForestSRC survival models

## Usage

``` r
rfsrc_train(formula, data, weights = NULL, ...)

survival_time_rfsrc(object, new_data)

survival_prob_rfsrc(object, new_data, eval_time)
```

## Arguments

- formula:

  A model formula with a
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  response.

- data:

  A data frame containing the response and predictors.

- weights:

  A numeric vector of case weights, or `NULL`.

- ...:

  Additional arguments passed to
  [`randomForestSRC::rfsrc()`](https://www.randomforestsrc.org//reference/rfsrc.html).

- object:

  A parsnip `model_fit` object resulting from [rand_forest() with engine
  =
  "randomForestSRC"](https://parsnip.tidymodels.org/reference/rand_forest.html).

- new_data:

  A data frame to be predicted.

- eval_time:

  A vector of times to predict the survival probability.

## Value

- `rfsrc_train()`: a fitted `rfsrc` object.

- `survival_time_rfsrc()`: A numeric vector of predicted survival times.

- `survival_prob_rfsrc()`: A tibble with a list column of nested
  tibbles.

## Details

`survival_time_rfsrc()` returns the median survival time, i.e. the first
event time at which the predicted survival curve drops to 0.5 or below.
A curve that never crosses 0.5 has an undefined median and yields `NA`;
the helper does not impute it. For a rank-based use such as the
concordance index, these `NA`s can be imputed as `Inf` in
post-processing (a non-crossing curve marks the longest survivors, so
`Inf` ranks them at the top).
