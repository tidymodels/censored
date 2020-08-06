
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survnip

<!-- badges: start -->

[![R build
status](https://github.com/EmilHvitfeldt/survnip/workflows/R-CMD-check/badge.svg)](https://github.com/EmilHvitfeldt/survnip/actions)
[![Codecov test
coverage](https://codecov.io/gh/EmilHvitfeldt/survnip/branch/master/graph/badge.svg)](https://codecov.io/gh/EmilHvitfeldt/survnip?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`survnip` (survival + parsnip) is a “`parsnip`-adjacent” packages with
model definitions for survival analysis packages.

## Installation

This package is still in early development. You need to install the
developmental branch of parsnip as well.

``` r
# install.packages("devtools")
remotes::install_github("tidymodels/parsnip#359")
devtools::install_github("EmilHvitfeldt/survnip")
```

## Prediction Types

The addition of censored regression comes with changes. One of these
changes is the quantities we would like to predict from the model. The 3
quantities we will consider are: `"time"`, `"survival"`, and
`"linear_pred"`.

To showcase these the differences, here is a simple Cox regression model
fitted on the `lung` data set.

``` r
library(survnip)
#> Loading required package: parsnip
library(survival)

cox_mod <-
  cox_reg() %>%
  set_engine("survival") %>%
  fit(Surv(time, status) ~ age + ph.ecog, data = lung)

cox_mod
#> parsnip model object
#> 
#> Fit time:  17ms 
#> Call:
#> survival::coxph(formula = Surv(time, status) ~ age + ph.ecog, 
#>     data = data, x = TRUE)
#> 
#>             coef exp(coef) se(coef)     z        p
#> age     0.011281  1.011345 0.009319 1.211 0.226082
#> ph.ecog 0.443485  1.558128 0.115831 3.829 0.000129
#> 
#> Likelihood ratio test=19.06  on 2 df, p=7.279e-05
#> n= 227, number of events= 164 
#>    (1 observation deleted due to missingness)
```

### time

when we specify `type = "time"` then we get back the predicted survival
time of an observation based on its predictors. The survival time is the
time it takes for the observation to observe an event.

``` r
predict(cox_mod, type = "time", new_data = lung)
#> # A tibble: 228 x 1
#>    .pred_time
#>         <dbl>
#>  1       342.
#>  2       474.
#>  3       511.
#>  4       389.
#>  5       498.
#>  6       342.
#>  7       263.
#>  8       256.
#>  9       401.
#> 10       278.
#> # … with 218 more rows
```

Here we see that the first patient is predicted to have 342.355592 days
left.

### survival

when we specify `type = "survival"` then we are trying to get the
properbility of survival (not observing an event) at a given time
`.time`.

``` r
pred_vals_survival <- predict(cox_mod, 
                              type = "survival", 
                              new_data = lung, 
                              .time = c(100, 200))

pred_vals_survival
#> # A tibble: 228 x 1
#>        .pred_survival
#>    <list<tbl_df[,2]>>
#>  1            [2 × 2]
#>  2            [2 × 2]
#>  3            [2 × 2]
#>  4            [2 × 2]
#>  5            [2 × 2]
#>  6            [2 × 2]
#>  7            [2 × 2]
#>  8            [2 × 2]
#>  9            [2 × 2]
#> 10            [2 × 2]
#> # … with 218 more rows

pred_vals_survival$.pred_survival[[1]]
#> # A tibble: 2 x 2
#>   .time .pred_survival
#>   <chr>          <dbl>
#> 1 100            0.855
#> 2 200            0.649
```

here we see that the first patient has a 85.5% probability of survival
after 100 days and 64.9% probability of survival after 200 days.

### linear\_pred

when we specify `type = "linear_pred"` then we get back the linear
predictor for the observation according to the model.

``` r
predict(cox_mod, type = "linear_pred", new_data = lung)
#> # A tibble: 228 x 1
#>    .pred_linear_pred
#>                <dbl>
#>  1            0.152 
#>  2           -0.359 
#>  3           -0.495 
#>  4           -0.0401
#>  5           -0.450 
#>  6            0.152 
#>  7            0.527 
#>  8            0.561 
#>  9           -0.0852
#> 10            0.449 
#> # … with 218 more rows
```

here we see that the linear predictor of the first observation is
0.1517.
