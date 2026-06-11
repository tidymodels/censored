# A wrapper for fitting a null model with survival data

Drops all predictors and fits a single Kaplan-Meier curve via
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html),
giving a non-informative baseline for censored regression.

## Usage

``` r
survfit_null(formula, data, ...)
```

## Arguments

- formula:

  A formula with a
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  response.

- data:

  A data frame.

- ...:

  Options to pass to
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).

## Value

A `survfit` object.

## Examples

``` r
survfit_null(survival::Surv(time, status) ~ ., data = survival::lung)
#> Call: survfit(formula = formula, data = data)
#> 
#>        n events median 0.95LCL 0.95UCL
#> [1,] 228    165    310     285     363
```
