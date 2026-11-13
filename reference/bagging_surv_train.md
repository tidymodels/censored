# Bagged trees via ipred

`bagging_surv_train()` is a wrapper for the `bagging()` function in the
ipred package that passes the main arguments for the trees on to
[`rpart::rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html).

## Usage

``` r
bagging_surv_train(
  formula,
  data,
  maxdepth = NULL,
  minsplit = NULL,
  cp = NULL,
  ...
)
```

## Arguments

- formula:

  A symbolic description of the model to be fitted.

- data:

  A data frame containing the variables in the model.

- maxdepth:

  The maximum depth of any node of the final tree.

- minsplit:

  The minimum number of observations in a node in order for a split to
  be attempted.

- cp:

  The complexity parameter.

- ...:

  Other arguments to pass to
  [`rpart::rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html)
  or [`ipred::bagging()`](https://rdrr.io/pkg/ipred/man/bagging.html),
  matched by name.

## Value

A fitted bagging model.

## Details

Arguments for
[`rpart::rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html)
which are not set take their default values from ipred, or from a
`control` object if one is passed via `...`. The main arguments and
other
[`rpart::rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html)
arguments passed via `...` take precedence over a `control` object.

## Examples

``` r
bagging_surv_train(Surv(time, status) ~ age + ph.ecog, data = lung, maxdepth = 2)
#> 
#> Bagging survival trees with 25 bootstrap replications 
#> 
#> Call: bagging.data.frame(formula = formula, data = data, control = control)
#> 
#> 
```
