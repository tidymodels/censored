
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survnip

<!-- badges: start -->

<!-- badges: end -->

`rules` is a “`parsnip`-adjacent” packages with model definitions for
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
#> # A tibble: 227 x 1
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
#> # … with 217 more rows

predict(cox_mod, new_data = lung, type = "time", .time = 200)
#> # A tibble: 227 x 1
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
#> # … with 217 more rows

predict(cox_mod, new_data = lung, type = "survival", .time = 200)
#> # A tibble: 228 x 1
#>    .pred_survival[,1]
#>                 <dbl>
#>  1              0.649
#>  2              0.772
#>  3              0.798
#>  4              0.700
#>  5              0.789
#>  6              0.649
#>  7              0.533
#>  8              0.522
#>  9              0.711
#> 10              0.559
#> # … with 218 more rows
```
