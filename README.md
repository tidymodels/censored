
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

`survnip` is a “`parsnip`-adjacent” packages with model definitions for
survival analysis packages.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EmilHvitfeldt/survnip")
```

## Example

``` r
library(survnip)
#> Loading required package: parsnip
library(survival)

cox_mod <-
  cox_reg() %>%
  set_engine("survival") %>%
  fit(Surv(time, status) ~ age + ph.ecog, data = lung)

predict(cox_mod, new_data = lung, .time = 200)
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

predict(cox_mod, new_data = lung, type = "time", .time = 200)
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

predict(cox_mod, new_data = lung, type = "survival", .time = 200)
#> # A tibble: 228 x 1
#>        .pred_survival
#>    <list<tbl_df[,2]>>
#>  1            [1 × 2]
#>  2            [1 × 2]
#>  3            [1 × 2]
#>  4            [1 × 2]
#>  5            [1 × 2]
#>  6            [1 × 2]
#>  7            [1 × 2]
#>  8            [1 × 2]
#>  9            [1 × 2]
#> 10            [1 × 2]
#> # … with 218 more rows
```
