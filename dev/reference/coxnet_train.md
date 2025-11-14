# Wrapper for glmnet for censored

Not to be used directly by users.

## Usage

``` r
coxnet_train(
  formula,
  data,
  alpha = 1,
  lambda = NULL,
  weights = NULL,
  ...,
  call = caller_env()
)
```

## Arguments

- formula:

  The model formula.

- data:

  The data.

- alpha:

  The elasticnet mixing parameter, with \\0\le\alpha\le 1\\. The penalty
  is defined as
  \$\$(1-\alpha)/2\|\|\beta\|\|\_2^2+\alpha\|\|\beta\|\|\_1.\$\$
  `alpha=1` is the lasso penalty, and `alpha=0` the ridge penalty.

- lambda:

  A user supplied `lambda` sequence. Typical usage is to have the
  program compute its own `lambda` sequence based on `nlambda` and
  `lambda.min.ratio`. Supplying a value of `lambda` overrides this.
  WARNING: use with care. Avoid supplying a single value for `lambda`
  (for predictions after CV use
  [`predict()`](https://rdrr.io/r/stats/predict.html) instead). Supply
  instead a decreasing sequence of `lambda` values. `glmnet` relies on
  its warms starts for speed, and its often faster to fit a whole path
  than compute a single fit.

- weights:

  observation weights. Can be total counts if responses are proportion
  matrices. Default is 1 for each observation

- ...:

  additional parameters passed to glmnet::glmnet.

- call:

  The call used in errors and warnings.

## Value

A fitted `glmnet` model.

## Details

This wrapper translates from formula interface to glmnet's matrix due to
how stratification can be specified. glmnet requires that the *response*
is stratified via
[`glmnet::stratifySurv()`](https://glmnet.stanford.edu/reference/stratifySurv.html).
censored allows specification via a
[`survival::strata()`](https://rdrr.io/pkg/survival/man/strata.html)
term on the right-hand side of the formula. The formula is used to
generate the stratification information needed for stratifying the
response. The formula without the strata term is used for generating the
model matrix for glmnet.

The wrapper retains the original formula and the pre-processing elements
including the training data to allow for predictions from the fitted
model.

## Examples

``` r
coxnet_mod <- coxnet_train(Surv(time, status) ~ age + sex, data = lung)
```
