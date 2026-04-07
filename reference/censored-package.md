# censored: parsnip Engines for Survival Models

censored provides engines for survival models from the parsnip package.
The models include parametric survival models, proportional hazards
models, decision trees, boosted trees, bagged trees, and random forests.
See the "Fitting and Predicting with censored" article for various
examples. See below for examples of classic survival models and how to
fit them with censored.

## See also

Useful links:

- <https://github.com/tidymodels/censored>

- <https://censored.tidymodels.org>

- Report bugs at <https://github.com/tidymodels/censored/issues>

## Author

**Maintainer**: Hannah Frick <hannah@posit.co>
([ORCID](https://orcid.org/0000-0002-6049-5258))

Authors:

- Emil Hvitfeldt <emil.hvitfeldt@posit.co>
  ([ORCID](https://orcid.org/0000-0002-0679-1945))

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]

## Examples

``` r
# Accelerated Failure Time (AFT) model

fit_aft <- survival_reg(dist = "weibull") |>
  set_engine("survival") |>
  fit(Surv(time, status) ~ age + sex + ph.karno, data = lung)
predict(fit_aft, lung[1:3, ], type = "time")
#> # A tibble: 3 × 1
#>   .pred_time
#>        <dbl>
#> 1       355.
#> 2       374.
#> 3       416.


# Cox's Proportional Hazards model

fit_cox <- proportional_hazards() |>
  set_engine("survival") |>
  fit(Surv(time, status) ~ age + sex + ph.karno, data = lung)
predict(fit_cox, lung[1:3, ], type = "time")
#> # A tibble: 3 × 1
#>   .pred_time
#>        <dbl>
#> 1       325.
#> 2       343.
#> 3       379.


# Andersen-Gill model for recurring events

fit_ag <- proportional_hazards() |>
  set_engine("survival") |>
  fit(Surv(tstart, tstop, status) ~ treat + inherit + age + strata(hos.cat),
    data = cgd
  )
predict(fit_ag, cgd[1:3, ], type = "time")
#> # A tibble: 3 × 1
#>   .pred_time
#>        <dbl>
#> 1       319.
#> 2       319.
#> 3       319.
```
