
<!-- README.md is generated from README.Rmd. Please edit that file -->

# censored

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/censored/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/censored/actions)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/censored/branch/main/graph/badge.svg)](https://codecov.io/gh/tidymodels/censored?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`censored` is a [parsnip](https://parsnip.tidymodels.org) extension
package which provides engines for various models for censored
regression and survival analysis.

## Installation

This package is still in development. You can install the development
version with

``` r
devtools::install_github("tidymodels/censored")
```

# Available models, engines, and prediction types

censored provides engines for the models in the following table.

The time to event can be predicted with `type = "time"`, the survival
probability with `type = "survival"`, the linear predictor with
`type = "linear_pred"`, the quantiles of the event time distribution
with `type = "quantile"`, and the hazard with `type = "hazard"`.

| model                 | engine   | time | survival | linear\_pred | raw | quantile | hazard |
|:----------------------|:---------|:-----|:---------|:-------------|:----|:---------|:-------|
| bag\_tree             | rpart    | ✓    | ✓        | x            | x   | x        | x      |
| boost\_tree           | mboost   | x    | ✓        | ✓            | x   | x        | x      |
| decision\_tree        | rpart    | ✓    | ✓        | x            | x   | x        | x      |
| decision\_tree        | party    | ✓    | ✓        | x            | x   | x        | x      |
| proportional\_hazards | survival | ✓    | ✓        | ✓            | x   | x        | x      |
| proportional\_hazards | glmnet   | x    | ✓        | ✓            | ✓   | x        | x      |
| rand\_forest          | party    | ✓    | ✓        | x            | x   | x        | x      |
| survival\_reg         | survival | ✓    | ✓        | x            | x   | ✓        | ✓      |
| survival\_reg         | flexsurv | ✓    | ✓        | x            | x   | ✓        | ✓      |
