
<!-- README.md is generated from README.Rmd. Please edit that file -->

# censored

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/censored/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/censored/actions)
[![Codecov test
coverage](https://codecov.io/gh/EmilHvitfeldt/censored/branch/main/graph/badge.svg)](https://codecov.io/gh/EmilHvitfeldt/censored?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- badges: end -->

`censored` is a “`parsnip`-adjacent” packages with model definitions for
censored regression and survival analysis models.

## Installation

This package is still in early development. You need to install the
development version of parsnip as well.

``` r
# install.packages("pak")
pak::pak("tidymodels/censored")
```

## Prediction Types

The addition of censored regression comes with changes. One of these
changes is the quantities we would like to predict from the model. The
three quantities we will consider are: `"time"`, `"survival"`, and
`"linear_pred"`.

To showcase these the differences, here is a simple Cox regression model
fitted on the `lung` data set.

``` r
library(censored)
#> Loading required package: parsnip
library(survival)

cox_mod <-
  proportional_hazards() %>%
  set_engine("survival") %>%
  fit(Surv(time, status) ~ age + ph.ecog, data = lung)

cox_mod
#> parsnip model object
#> 
#> Fit time:  12ms 
#> Call:
#> survival::coxph(formula = Surv(time, status) ~ age + ph.ecog, 
#>     data = data, model = TRUE, x = TRUE)
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

When we specify `type = "time"` then we get back the predicted survival
time of an observation based on its predictors. The survival time is the
time it takes for the observation to observe an event.

``` r
predict(cox_mod, type = "time", new_data = head(lung))
#> # A tibble: 6 × 1
#>   .pred_time
#>        <dbl>
#> 1       342.
#> 2       474.
#> 3       511.
#> 4       389.
#> 5       498.
#> 6       342.
```

Here we see that the first patient is predicted to have 342.36 days
left.

### survival

When we specify `type = "survival"` then we are trying to get the
probabilities of survival (not observing an event) up to a given time
`time`.

``` r
pred_vals_survival <- predict(cox_mod, 
                              type = "survival", 
                              new_data = head(lung), 
                              time = c(100, 200))

pred_vals_survival
#> # A tibble: 6 × 1
#>   .pred           
#>   <list>          
#> 1 <tibble [2 × 2]>
#> 2 <tibble [2 × 2]>
#> 3 <tibble [2 × 2]>
#> 4 <tibble [2 × 2]>
#> 5 <tibble [2 × 2]>
#> 6 <tibble [2 × 2]>

pred_vals_survival$.pred[[1]]
#> # A tibble: 2 × 2
#>   .time .pred_survival
#>   <dbl>          <dbl>
#> 1   100          0.855
#> 2   200          0.649
```

here we see that the first patient has a 85.5% probability of survival
up to 100 days and 64.9% probability of survival up to 200 days.

### linear\_pred

when we specify `type = "linear_pred"` then we get back the linear
predictor for the observation according to the model.

``` r
predict(cox_mod, type = "linear_pred", new_data = head(lung))
#> # A tibble: 6 × 1
#>   .pred_linear_pred
#>               <dbl>
#> 1            -1.28 
#> 2            -0.767
#> 3            -0.632
#> 4            -1.09 
#> 5            -0.677
#> 6            -1.28
```

here we see that the linear predictor of the first observation is
-1.2783.

Note that, for linear predictor prediction types, the results are
formatted for all models such that the prediction *increases* with time.
For the proportional hazards model, the sign is reversed.

## Prediction type table

| alias                 | engine   | time  | survival | linear\_pred | raw   | quantile | hazard |
|:----------------------|:---------|:------|:---------|:-------------|:------|:---------|:-------|
| bag\_tree             | rpart    | TRUE  | TRUE     | FALSE        | FALSE | FALSE    | FALSE  |
| boost\_tree           | mboost   | FALSE | TRUE     | TRUE         | FALSE | FALSE    | FALSE  |
| decision\_tree        | rpart    | TRUE  | TRUE     | FALSE        | FALSE | FALSE    | FALSE  |
| decision\_tree        | party    | TRUE  | TRUE     | FALSE        | FALSE | FALSE    | FALSE  |
| proportional\_hazards | survival | TRUE  | TRUE     | TRUE         | FALSE | FALSE    | FALSE  |
| proportional\_hazards | glmnet   | FALSE | TRUE     | TRUE         | TRUE  | FALSE    | FALSE  |
| rand\_forest          | party    | TRUE  | TRUE     | FALSE        | FALSE | FALSE    | FALSE  |
| survival\_reg         | survival | TRUE  | TRUE     | FALSE        | FALSE | TRUE     | TRUE   |
| survival\_reg         | flexsurv | TRUE  | TRUE     | FALSE        | FALSE | TRUE     | TRUE   |
