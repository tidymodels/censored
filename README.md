
<!-- README.md is generated from README.Rmd. Please edit that file -->

# censored

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/censored/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/censored/actions/workflows/R-CMD-check.yaml)
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

## Available models, engines, and prediction types

censored provides engines for the models in the following table. For
examples, please see [Fitting and Predicting with
censored](https://censored.tidymodels.org/articles/articles/examples.html).

The time to event can be predicted with `type = "time"`, the survival
probability with `type = "survival"`, the linear predictor with
`type = "linear_pred"`, the quantiles of the event time distribution
with `type = "quantile"`, and the hazard with `type = "hazard"`.

| model                | engine   | time | survival | linear_pred | raw | quantile | hazard |
|:---------------------|:---------|:-----|:---------|:------------|:----|:---------|:-------|
| bag_tree             | rpart    | ✓    | ✓        | x           | x   | x        | x      |
| boost_tree           | mboost   | x    | ✓        | ✓           | x   | x        | x      |
| decision_tree        | rpart    | ✓    | ✓        | x           | x   | x        | x      |
| decision_tree        | partykit | ✓    | ✓        | x           | x   | x        | x      |
| proportional_hazards | survival | ✓    | ✓        | ✓           | x   | x        | x      |
| proportional_hazards | glmnet   | x    | ✓        | ✓           | ✓   | x        | x      |
| rand_forest          | partykit | ✓    | ✓        | x           | x   | x        | x      |
| survival_reg         | survival | ✓    | ✓        | ✓           | x   | ✓        | ✓      |
| survival_reg         | flexsurv | ✓    | ✓        | ✓           | x   | ✓        | ✓      |

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

-   For questions and discussions about tidymodels packages, modeling,
    and machine learning, please [post on RStudio
    Community](https://community.rstudio.com/new-topic?category_id=15&tags=tidymodels,question).

-   If you think you have encountered a bug, please [submit an
    issue](https://github.com/tidymodels/censored/issues).

-   Either way, learn how to create and share a
    [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
    (a minimal, reproducible example), to clearly communicate about your
    code.

-   Check out further details on [contributing guidelines for tidymodels
    packages](https://www.tidymodels.org/contribute/) and [how to get
    help](https://www.tidymodels.org/help/).
