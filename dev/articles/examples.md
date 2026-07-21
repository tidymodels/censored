# Fitting and Predicting with censored

These examples illustrate which models, engines, and prediction types
are available in censored. As a reminder, in parsnip,

- the **model type** differentiates basic modeling approaches, such as
  random forests, proportional hazards models, etc.,

- the **mode** denotes in what kind of modeling context it will be used
  (here, censored regression), and

- the computational **engine** indicates how the model is fit, such as
  with a specific R package implementation or even methods outside of R
  like Keras or Stan.

The following examples use the same data set throughout.

## `bag_tree()` models

With the `"rpart"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
```

      ## ── Attaching packages ───────────────────────────── tidymodels 1.5.0 ──

      ## ✔ broom        1.0.13     ✔ rsample      1.3.2 
      ## ✔ dials        1.4.4      ✔ tailor       0.1.0 
      ## ✔ dplyr        1.2.1      ✔ tidyr        1.3.2 
      ## ✔ infer        1.1.0      ✔ tune         2.1.0 
      ## ✔ modeldata    1.5.1      ✔ workflows    1.3.0 
      ## ✔ parsnip      1.6.0      ✔ workflowsets 1.1.1 
      ## ✔ purrr        1.2.2      ✔ yardstick    1.4.0 
      ## ✔ recipes      1.3.3

      ## ── Conflicts ──────────────────────────────── tidymodels_conflicts() ──
      ## ✖ purrr::discard() masks scales::discard()
      ## ✖ dplyr::filter()  masks stats::filter()
      ## ✖ dplyr::lag()     masks stats::lag()
      ## ✖ recipes::step()  masks stats::step()

``` r

  library(censored)
```

      ## Loading required package: survival

``` r

  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  bt_spec <- 
    bag_tree(cost_complexity = 0) |>
    set_engine("rpart") |> 
    set_mode("censored regression") 
  bt_spec
```

      ## Bagged Decision Tree Model Specification (censored regression)
      ## 
      ## Main Arguments:
      ##   cost_complexity = 0
      ##   min_n = 2
      ## 
      ## Computational engine: rpart

Now we create the model fit object:

``` r

  set.seed(1)
  bt_fit <- bt_spec |> fit(Surv(time, status) ~ ., data = lung_train)
  bt_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Bagging survival trees with 25 bootstrap replications 
      ## 
      ## Call: bagging.data.frame(formula = Surv(time, status) ~ ., data = data)

The holdout data can be predicted for survival probability at different
time points as well as event time.

``` r

  predict(
    bt_fit, 
    lung_test, 
    type = "survival", 
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100        0.946  
      ## 2        500        0.333  
      ## 3       1000        0.00496

``` r

  predict(bt_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1        353
      ## 2        293
      ## 3        230
      ## 4        201
      ## 5        268

## `boost_tree()` models

With the `"mboost"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  bt_spec <- 
    boost_tree(trees = 15) |>
    set_engine("mboost") |> 
    set_mode("censored regression") 
  bt_spec
```

      ## Boosted Tree Model Specification (censored regression)
      ## 
      ## Main Arguments:
      ##   trees = 15
      ## 
      ## Computational engine: mboost

Now we create the model fit object:

``` r

  set.seed(1)
  bt_fit <- bt_spec |> fit(Surv(time, status) ~ ., data = lung_train)
  bt_fit
```

      ## parsnip model object
      ## 
      ## 
      ##     Model-based Boosting
      ## 
      ## Call:
      ## mboost::blackboost(formula = formula, data = data, family = family,     control = mboost::boost_control(mstop = 15), tree_controls = partykit::ctree_control(teststat = "quadratic",         testtype = "Teststatistic", mincriterion = 0, minsplit = 10,         minbucket = 4, maxdepth = 2, saveinfo = FALSE))
      ## 
      ## 
      ##     Cox Partial Likelihood 
      ## 
      ## Loss function:  
      ## 
      ## Number of boosting iterations: mstop = 15 
      ## Step size:  0.1 
      ## Offset:  0 
      ## Number of baselearners:  1

The holdout data can be predicted for survival probability at different
time points as well as the linear predictor.

``` r

  predict(
    bt_fit, 
    lung_test,
    type = "survival",
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.867 
      ## 2        500         0.294 
      ## 3       1000         0.0441

``` r

  predict(bt_fit, lung_test, type = "linear_pred")
```

      ## # A tibble: 5 × 1
      ##   .pred_linear_pred
      ##               <dbl>
      ## 1            0.0823
      ## 2           -0.455 
      ## 3            0.0661
      ## 4           -0.724 
      ## 5           -0.724

## `decision_tree()` models

With the `"rpart"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  dt_spec <- 
    decision_tree(cost_complexity = 0) |>
    set_engine("rpart") |> 
    set_mode("censored regression") 
  dt_spec
```

      ## Decision Tree Model Specification (censored regression)
      ## 
      ## Main Arguments:
      ##   cost_complexity = 0
      ## 
      ## Computational engine: rpart

Now we create the model fit object:

``` r

  set.seed(1)
  dt_fit <- dt_spec |> fit(Surv(time, status) ~ ., data = lung_train)
  dt_fit
```

      ## parsnip model object
      ## 
      ## $rpart
      ## n= 162 
      ## 
      ## node), split, n, deviance, yval
      ##       * denotes terminal node
      ## 
      ##   1) root 162 217.089100 1.0000000  
      ##     2) ph.ecog< 1.5 125 146.610800 0.8606149  
      ##       4) pat.karno>=65 117 134.248900 0.8042241  
      ##         8) sex>=1.5 47  58.371280 0.5920010  
      ##          16) inst>=12.5 16  17.696750 0.3469493 *
      ##          17) inst< 12.5 31  36.986020 0.7601739  
      ##            34) ph.ecog< 0.5 14  21.869860 0.4765888 *
      ##            35) ph.ecog>=0.5 17  12.197510 0.9977683 *
      ##         9) sex< 1.5 70  71.035080 0.9843711  
      ##          18) wt.loss< -0.5 10   7.608541 0.6466464 *
      ##          19) wt.loss>=-0.5 60  61.204860 1.0855380  
      ##            38) inst< 18.5 51  52.890560 0.9994210  
      ##              76) pat.karno< 85 27  30.835530 0.8204259  
      ##               152) age< 65.5 16  16.499450 0.6396414 *
      ##               153) age>=65.5 11  12.211210 1.2318540 *
      ##              77) pat.karno>=85 24  20.327560 1.2436570  
      ##               154) pat.karno>=95 10   6.634957 0.7568023 *
      ##               155) pat.karno< 95 14  10.631990 1.6387150 *
      ##            39) inst>=18.5 9   6.360874 1.6566500 *
      ##       5) pat.karno< 65 8   5.011986 2.2376180 *
      ##     3) ph.ecog>=1.5 37  59.992750 1.7157640  
      ##       6) wt.loss>=21 10  10.703230 0.6678083 *
      ##       7) wt.loss< 21 27  29.918520 3.1500170  
      ##        14) sex>=1.5 12   7.395091 1.9066160 *
      ##        15) sex< 1.5 15  16.563010 4.5917120 *
      ## 
      ## $survfit
      ## 
      ## Call: prodlim::prodlim(formula = form, data = data)

      ## Stratified Kaplan-Meier estimator for the conditional event time survival function

      ## Discrete predictor variable: rpartFactor (0.34694933272507, 0.47658881486553, 0.639641354557786, 0.646646427745816, 0.667808261569019, 0.756802251840104, 0.997768280401696, 1.23185367065451, 1.638714591616, 1.65664969973098, 1.90661557969861, 2.23761769770399, 4.59171172488878)

      ## 
      ## Right-censored response of a survival model
      ## 
      ## No.Observations: 162 
      ## 
      ## Pattern:
      ##                 Freq
      ##  event          116 
      ##  right.censored 46  
      ## 
      ## $levels
      ##  [1] "0.34694933272507"  "0.47658881486553"  "0.639641354557786"
      ##  [4] "0.646646427745816" "0.667808261569019" "0.756802251840104"
      ##  [7] "0.997768280401696" "1.23185367065451"  "1.638714591616"   
      ## [10] "1.65664969973098"  "1.90661557969861"  "2.23761769770399" 
      ## [13] "4.59171172488878" 
      ## 
      ## attr(,"class")
      ## [1] "pecRpart"

The holdout data can be predicted for survival probability at different
time points as well as event time.

``` r

  predict(
    dt_fit, 
    lung_test, 
    type = "survival",
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100          0.786
      ## 2        500          0.143
      ## 3       1000         NA

``` r

  predict(dt_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1        270
      ## 2        197
      ## 3        301
      ## 4        201
      ## 5        201

With the `"partykit"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  dt_spec <- 
    decision_tree() |>
    set_engine("partykit") |> 
    set_mode("censored regression") 
  dt_spec
```

      ## Decision Tree Model Specification (censored regression)
      ## 
      ## Computational engine: partykit

Now we create the model fit object:

``` r

  set.seed(1)
  dt_fit <- dt_spec |> fit(Surv(time, status) ~ ., data = lung_train)
  dt_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Model formula:
      ## Surv(time, status) ~ inst + age + sex + ph.ecog + ph.karno + 
      ##     pat.karno + meal.cal + wt.loss
      ## 
      ## Fitted party:
      ## [1] root
      ## |   [2] ph.ecog <= 1: 363.000 (n = 125)
      ## |   [3] ph.ecog > 1
      ## |   |   [4] wt.loss <= 20
      ## |   |   |   [5] sex <= 1: 65.000 (n = 15)
      ## |   |   |   [6] sex > 1: 201.000 (n = 12)
      ## |   |   [7] wt.loss > 20: 524.000 (n = 10)
      ## 
      ## Number of inner nodes:    3
      ## Number of terminal nodes: 4

The holdout data can be predicted for survival probability at different
time points as well as event time.

``` r

  predict(
    dt_fit, 
    lung_test, 
    type = "survival",
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.896 
      ## 2        500         0.334 
      ## 3       1000         0.0719

``` r

  predict(dt_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1        363
      ## 2        363
      ## 3        363
      ## 4        201
      ## 5        201

## `proportional_hazards()` models

With the `"survival"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  ph_spec <- 
    proportional_hazards() |>
    set_engine("survival") |> 
    set_mode("censored regression") 
  ph_spec
```

      ## Proportional Hazards Model Specification (censored regression)
      ## 
      ## Computational engine: survival

Now we create the model fit object:

``` r

  set.seed(1)
  ph_fit <- ph_spec |> fit(Surv(time, status) ~ ., data = lung_train)
  ph_fit
```

      ## parsnip model object
      ## 
      ## Call:
      ## survival::coxph(formula = Surv(time, status) ~ ., data = data, 
      ##     model = TRUE, x = TRUE)
      ## 
      ##                 coef  exp(coef)   se(coef)      z       p
      ## inst      -0.0291726  0.9712488  0.0131293 -2.222 0.02629
      ## age        0.0146341  1.0147417  0.0119705  1.223 0.22151
      ## sex       -0.5977137  0.5500678  0.2051326 -2.914 0.00357
      ## ph.ecog    0.7507039  2.1184906  0.2536100  2.960 0.00308
      ## ph.karno   0.0137315  1.0138262  0.0132752  1.034 0.30096
      ## pat.karno -0.0082098  0.9918238  0.0082560 -0.994 0.32002
      ## meal.cal  -0.0001233  0.9998767  0.0002841 -0.434 0.66435
      ## wt.loss   -0.0188464  0.9813301  0.0082051 -2.297 0.02162
      ## 
      ## Likelihood ratio test=32.61  on 8 df, p=7.224e-05
      ## n= 162, number of events= 116

The holdout data can be predicted for survival probability at different
time points as well as the linear predictor and event time.

``` r

  predict(
    ph_fit, 
    lung_test, 
    type = "survival",
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.903 
      ## 2        500         0.410 
      ## 3       1000         0.0953

``` r

  predict(ph_fit, lung_test, type = "linear_pred")
```

      ## # A tibble: 5 × 1
      ##   .pred_linear_pred
      ##               <dbl>
      ## 1            -0.373
      ## 2            -1.24 
      ## 3            -0.852
      ## 4            -1.33 
      ## 5            -1.11

``` r

  predict(ph_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1       448.
      ## 2       262.
      ## 3       337.
      ## 4       246.
      ## 5       286.

With the `"glmnet"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  ph_spec <- 
    proportional_hazards(penalty = 0.1) |>
    set_engine("glmnet") |> 
    set_mode("censored regression") 
  ph_spec
```

      ## Proportional Hazards Model Specification (censored regression)
      ## 
      ## Main Arguments:
      ##   penalty = 0.1
      ## 
      ## Computational engine: glmnet

Now we create the model fit object:

``` r

  set.seed(1)
  ph_fit <- ph_spec |> fit(Surv(time, status) ~ ., data = lung_train)
```

      ## Warning: Starting in glmnet 5.1, the default Cox tie-handling method
      ## will change from 'breslow' to 'efron' (matching survival::coxph). To
      ## silence this message and lock in the v5.0 default, pass cox.ties =
      ## 'breslow' explicitly. To preview the v5.1 behavior, pass cox.ties =
      ## 'efron'.

``` r

  ph_fit
```

      ## parsnip model object
      ## 
      ## 
      ## Call:  glmnet::glmnet(x = data_obj$x, y = data_obj$y, family = "cox",      weights = weights, alpha = alpha, lambda = lambda) 
      ## 
      ##    Df %Dev   Lambda
      ## 1   0 0.00 0.221100
      ## 2   1 0.23 0.201500
      ## 3   2 0.44 0.183600
      ## 4   2 0.71 0.167300
      ## 5   2 0.96 0.152400
      ## 6   2 1.16 0.138900
      ## 7   2 1.33 0.126500
      ## 8   3 1.47 0.115300
      ## 9   4 1.60 0.105100
      ## 10  4 1.73 0.095730
      ## 11  5 1.86 0.087220
      ## 12  6 2.00 0.079470
      ## 13  6 2.20 0.072410
      ## 14  6 2.38 0.065980
      ## 15  6 2.52 0.060120
      ## 16  6 2.65 0.054780
      ## 17  6 2.75 0.049910
      ## 18  6 2.83 0.045480
      ## 19  6 2.91 0.041440
      ## 20  6 2.97 0.037760
      ## 21  6 3.02 0.034400
      ## 22  6 3.06 0.031350
      ## 23  7 3.10 0.028560
      ## 24  7 3.14 0.026020
      ## 25  7 3.17 0.023710
      ## 26  7 3.19 0.021610
      ## 27  8 3.25 0.019690
      ## 28  8 3.27 0.017940
      ## 29  8 3.30 0.016340
      ## 30  8 3.32 0.014890
      ## 31  8 3.33 0.013570
      ## 32  8 3.35 0.012360
      ## 33  8 3.36 0.011270
      ## 34  8 3.37 0.010260
      ## 35  8 3.38 0.009353
      ## 36  8 3.38 0.008522
      ## 37  8 3.39 0.007765
      ## 38  8 3.39 0.007075
      ## 39  8 3.40 0.006446
      ## 40  8 3.40 0.005874
      ## 41  8 3.41 0.005352
      ## 42  8 3.41 0.004876
      ## 43  8 3.41 0.004443
      ## 44  8 3.41 0.004048
      ## 45  8 3.41 0.003689
      ## 46  8 3.42 0.003361
      ## 47  8 3.42 0.003063
      ## 48  8 3.42 0.002790
      ## 49  8 3.42 0.002543
      ## 50  8 3.42 0.002317
      ## 51  8 3.42 0.002111
      ## 52  8 3.42 0.001923
      ## The training data has been saved for prediction.

The holdout data can be predicted for survival probability at different
time points as well as the linear predictor.

``` r

  predict(
    ph_fit, 
    lung_test, 
    type = "survival",
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.873 
      ## 2        500         0.349 
      ## 3       1000         0.0801

``` r

  predict(ph_fit, lung_test, type = "linear_pred")
```

      ## # A tibble: 5 × 1
      ##   .pred_linear_pred
      ##               <dbl>
      ## 1            0.175 
      ## 2           -0.0783
      ## 3           -0.0945
      ## 4           -0.114 
      ## 5           -0.112

## `rand_forest()` models

With the `"partykit"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  rf_spec <- 
    rand_forest(trees = 200) |>
    set_engine("partykit") |> 
    set_mode("censored regression") 
  rf_spec
```

      ## Random Forest Model Specification (censored regression)
      ## 
      ## Main Arguments:
      ##   trees = 200
      ## 
      ## Computational engine: partykit

Now we create the model fit object:

``` r

  set.seed(1)
  rf_fit <- rf_spec |> fit(Surv(time, status) ~ ., data = lung_train)
  rf_fit
```

      ## parsnip model object
      ## 
      ## $nodes
      ## $nodes[[1]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V3 <= 64
      ## |   |   |   |   [5] V8 <= 1025 *
      ## |   |   |   |   [6] V8 > 1025 *
      ## |   |   |   [7] V3 > 64 *
      ## |   |   [8] V5 > 1 *
      ## |   [9] V4 > 1
      ## |   |   [10] V5 <= 1
      ## |   |   |   [11] V5 <= 0 *
      ## |   |   |   [12] V5 > 0 *
      ## |   |   [13] V5 > 1 *
      ## 
      ## $nodes[[2]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V4 <= 1 *
      ## |   |   [4] V4 > 1 *
      ## |   [5] V5 > 0
      ## |   |   [6] V5 <= 1
      ## |   |   |   [7] V9 <= 19
      ## |   |   |   |   [8] V4 <= 1
      ## |   |   |   |   |   [9] V9 <= 6 *
      ## |   |   |   |   |   [10] V9 > 6 *
      ## |   |   |   |   [11] V4 > 1 *
      ## |   |   |   [12] V9 > 19 *
      ## |   |   [13] V5 > 1
      ## |   |   |   [14] V4 <= 1 *
      ## |   |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[3]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V5 <= 0
      ## |   |   |   [4] V2 <= 5 *
      ## |   |   |   [5] V2 > 5
      ## |   |   |   |   [6] V6 <= 90 *
      ## |   |   |   |   [7] V6 > 90 *
      ## |   |   [8] V5 > 0
      ## |   |   |   [9] V6 <= 80
      ## |   |   |   |   [10] V7 <= 70 *
      ## |   |   |   |   [11] V7 > 70
      ## |   |   |   |   |   [12] V2 <= 10 *
      ## |   |   |   |   |   [13] V2 > 10 *
      ## |   |   |   [14] V6 > 80 *
      ## |   [15] V5 > 1
      ## |   |   [16] V6 <= 60 *
      ## |   |   [17] V6 > 60 *
      ## 
      ## $nodes[[4]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V7 <= 80 *
      ## |   |   [4] V7 > 80 *
      ## |   [5] V5 > 0
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V6 <= 80
      ## |   |   |   |   [8] V3 <= 65 *
      ## |   |   |   |   [9] V3 > 65
      ## |   |   |   |   |   [10] V9 <= 7 *
      ## |   |   |   |   |   [11] V9 > 7 *
      ## |   |   |   [12] V6 > 80 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V5 <= 1 *
      ## |   |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[5]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V6 <= 80
      ## |   |   |   |   [5] V7 <= 80 *
      ## |   |   |   |   [6] V7 > 80 *
      ## |   |   |   [7] V6 > 80
      ## |   |   |   |   [8] V9 <= 12
      ## |   |   |   |   |   [9] V2 <= 11 *
      ## |   |   |   |   |   [10] V2 > 11 *
      ## |   |   |   |   [11] V9 > 12 *
      ## |   |   [12] V4 > 1
      ## |   |   |   [13] V3 <= 53 *
      ## |   |   |   [14] V3 > 53
      ## |   |   |   |   [15] V3 <= 64 *
      ## |   |   |   |   [16] V3 > 64 *
      ## |   [17] V5 > 1
      ## |   |   [18] V8 <= 925 *
      ## |   |   [19] V8 > 925 *
      ## 
      ## $nodes[[6]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 80
      ## |   |   |   [4] V8 <= 613 *
      ## |   |   |   [5] V8 > 613
      ## |   |   |   |   [6] V2 <= 10 *
      ## |   |   |   |   [7] V2 > 10 *
      ## |   |   [8] V6 > 80
      ## |   |   |   [9] V8 <= 875 *
      ## |   |   |   [10] V8 > 875
      ## |   |   |   |   [11] V9 <= 2 *
      ## |   |   |   |   [12] V9 > 2 *
      ## |   [13] V4 > 1
      ## |   |   [14] V6 <= 70 *
      ## |   |   [15] V6 > 70
      ## |   |   |   [16] V2 <= 11 *
      ## |   |   |   [17] V2 > 11 *
      ## 
      ## $nodes[[7]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V3 <= 74
      ## |   |   |   [5] V7 <= 90
      ## |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   [7] V5 > 0
      ## |   |   |   |   |   [8] V7 <= 70 *
      ## |   |   |   |   |   [9] V7 > 70
      ## |   |   |   |   |   |   [10] V9 <= 4 *
      ## |   |   |   |   |   |   [11] V9 > 4 *
      ## |   |   |   [12] V7 > 90 *
      ## |   |   [13] V3 > 74 *
      ## 
      ## $nodes[[8]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V4 <= 1
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0 *
      ## |   |   |   [7] V4 > 1
      ## |   |   |   |   [8] V9 <= 6 *
      ## |   |   |   |   [9] V9 > 6 *
      ## |   |   [10] V3 > 64
      ## |   |   |   [11] V7 <= 80 *
      ## |   |   |   [12] V7 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V4 <= 1 *
      ## |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[9]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 80
      ## |   |   |   [4] V9 <= 20
      ## |   |   |   |   [5] V6 <= 70 *
      ## |   |   |   |   [6] V6 > 70 *
      ## |   |   |   [7] V9 > 20 *
      ## |   |   [8] V6 > 80
      ## |   |   |   [9] V7 <= 80 *
      ## |   |   |   [10] V7 > 80 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 90
      ## |   |   |   [13] V9 <= 3 *
      ## |   |   |   [14] V9 > 3 *
      ## |   |   [15] V7 > 90 *
      ## 
      ## $nodes[[10]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V9 <= 3 *
      ## |   |   |   [5] V9 > 3 *
      ## |   |   [6] V3 > 64 *
      ## |   [7] V5 > 0
      ## |   |   [8] V9 <= 27
      ## |   |   |   [9] V5 <= 1
      ## |   |   |   |   [10] V9 <= 14
      ## |   |   |   |   |   [11] V4 <= 1 *
      ## |   |   |   |   |   [12] V4 > 1 *
      ## |   |   |   |   [13] V9 > 14 *
      ## |   |   |   [14] V5 > 1 *
      ## |   |   [15] V9 > 27 *
      ## 
      ## $nodes[[11]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V7 <= 90
      ## |   |   |   [4] V7 <= 70 *
      ## |   |   |   [5] V7 > 70
      ## |   |   |   |   [6] V3 <= 70
      ## |   |   |   |   |   [7] V7 <= 80 *
      ## |   |   |   |   |   [8] V7 > 80
      ## |   |   |   |   |   |   [9] V3 <= 61 *
      ## |   |   |   |   |   |   [10] V3 > 61 *
      ## |   |   |   |   [11] V3 > 70 *
      ## |   |   [12] V7 > 90 *
      ## |   [13] V5 > 1
      ## |   |   [14] V4 <= 1 *
      ## |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[12]]
      ## [1] root
      ## |   [2] V2 <= 21
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V6 <= 70 *
      ## |   |   |   [6] V6 > 70
      ## |   |   |   |   [7] V7 <= 90
      ## |   |   |   |   |   [8] V5 <= 0 *
      ## |   |   |   |   |   [9] V5 > 0
      ## |   |   |   |   |   |   [10] V9 <= 14
      ## |   |   |   |   |   |   |   [11] V7 <= 80 *
      ## |   |   |   |   |   |   |   [12] V7 > 80 *
      ## |   |   |   |   |   |   [13] V9 > 14 *
      ## |   |   |   |   [14] V7 > 90 *
      ## |   [15] V2 > 21 *
      ## 
      ## $nodes[[13]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V5 <= 0 *
      ## |   |   |   [6] V5 > 0
      ## |   |   |   |   [7] V3 <= 60 *
      ## |   |   |   |   [8] V3 > 60
      ## |   |   |   |   |   [9] V7 <= 70 *
      ## |   |   |   |   |   [10] V7 > 70 *
      ## |   [11] V4 > 1
      ## |   |   [12] V5 <= 0 *
      ## |   |   [13] V5 > 0
      ## |   |   |   [14] V5 <= 1 *
      ## |   |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[14]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V3 <= 53 *
      ## |   |   |   [5] V3 > 53
      ## |   |   |   |   [6] V9 <= 14
      ## |   |   |   |   |   [7] V9 <= 2 *
      ## |   |   |   |   |   [8] V9 > 2 *
      ## |   |   |   |   [9] V9 > 14 *
      ## |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V6 <= 80
      ## |   |   |   [13] V5 <= 1 *
      ## |   |   |   [14] V5 > 1 *
      ## |   |   [15] V6 > 80
      ## |   |   |   [16] V5 <= 0 *
      ## |   |   |   [17] V5 > 0 *
      ## 
      ## $nodes[[15]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V5 <= 1
      ## |   |   |   [5] V4 <= 1
      ## |   |   |   |   [6] V8 <= 1275
      ## |   |   |   |   |   [7] V3 <= 59 *
      ## |   |   |   |   |   [8] V3 > 59
      ## |   |   |   |   |   |   [9] V5 <= 0 *
      ## |   |   |   |   |   |   [10] V5 > 0 *
      ## |   |   |   |   [11] V8 > 1275 *
      ## |   |   |   [12] V4 > 1
      ## |   |   |   |   [13] V6 <= 90
      ## |   |   |   |   |   [14] V8 <= 875 *
      ## |   |   |   |   |   [15] V8 > 875 *
      ## |   |   |   |   [16] V6 > 90 *
      ## |   |   [17] V5 > 1 *
      ## 
      ## $nodes[[16]]
      ## [1] root
      ## |   [2] V7 <= 60
      ## |   |   [3] V9 <= 8 *
      ## |   |   [4] V9 > 8 *
      ## |   [5] V7 > 60
      ## |   |   [6] V5 <= 1
      ## |   |   |   [7] V5 <= 0
      ## |   |   |   |   [8] V6 <= 90 *
      ## |   |   |   |   [9] V6 > 90 *
      ## |   |   |   [10] V5 > 0
      ## |   |   |   |   [11] V4 <= 1
      ## |   |   |   |   |   [12] V6 <= 80 *
      ## |   |   |   |   |   [13] V6 > 80 *
      ## |   |   |   |   [14] V4 > 1 *
      ## |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[17]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V9 <= 10
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0 *
      ## |   |   |   [7] V9 > 10 *
      ## |   |   [8] V4 > 1
      ## |   |   |   [9] V6 <= 80 *
      ## |   |   |   [10] V6 > 80 *
      ## |   [11] V5 > 1
      ## |   |   [12] V9 <= 10 *
      ## |   |   [13] V9 > 10 *
      ## 
      ## $nodes[[18]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 52 *
      ## |   |   [4] V3 > 52
      ## |   |   |   [5] V5 <= 0
      ## |   |   |   |   [6] V6 <= 90 *
      ## |   |   |   |   [7] V6 > 90 *
      ## |   |   |   [8] V5 > 0
      ## |   |   |   |   [9] V4 <= 1
      ## |   |   |   |   |   [10] V3 <= 66 *
      ## |   |   |   |   |   [11] V3 > 66 *
      ## |   |   |   |   [12] V4 > 1 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 11 *
      ## |   |   [15] V9 > 11 *
      ## 
      ## $nodes[[19]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V9 <= 3 *
      ## |   |   [4] V9 > 3 *
      ## |   [5] V5 > 0
      ## |   |   [6] V9 <= 27
      ## |   |   |   [7] V5 <= 1
      ## |   |   |   |   [8] V7 <= 80
      ## |   |   |   |   |   [9] V7 <= 70 *
      ## |   |   |   |   |   [10] V7 > 70 *
      ## |   |   |   |   [11] V7 > 80 *
      ## |   |   |   [12] V5 > 1 *
      ## |   |   [13] V9 > 27 *
      ## 
      ## $nodes[[20]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V9 <= 24
      ## |   |   |   [4] V6 <= 70 *
      ## |   |   |   [5] V6 > 70 *
      ## |   |   [6] V9 > 24 *
      ## |   [7] V7 > 70
      ## |   |   [8] V4 <= 1
      ## |   |   |   [9] V5 <= 0 *
      ## |   |   |   [10] V5 > 0
      ## |   |   |   |   [11] V9 <= 1 *
      ## |   |   |   |   [12] V9 > 1 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V7 <= 90 *
      ## |   |   |   [15] V7 > 90 *
      ## 
      ## $nodes[[21]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V8 <= 1060
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0 *
      ## |   |   |   [7] V8 > 1060
      ## |   |   |   |   [8] V6 <= 90 *
      ## |   |   |   |   [9] V6 > 90 *
      ## |   |   [10] V3 > 64
      ## |   |   |   [11] V7 <= 80 *
      ## |   |   |   [12] V7 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 20 *
      ## |   |   [15] V9 > 20 *
      ## 
      ## $nodes[[22]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V5 <= 0 *
      ## |   |   |   [5] V5 > 0
      ## |   |   |   |   [6] V4 <= 1
      ## |   |   |   |   |   [7] V9 <= 10 *
      ## |   |   |   |   |   [8] V9 > 10 *
      ## |   |   |   |   [9] V4 > 1 *
      ## |   |   [10] V3 > 64
      ## |   |   |   [11] V6 <= 80 *
      ## |   |   |   [12] V6 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 11 *
      ## |   |   [15] V9 > 11 *
      ## 
      ## $nodes[[23]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V9 <= 20
      ## |   |   |   [4] V6 <= 70 *
      ## |   |   |   [5] V6 > 70
      ## |   |   |   |   [6] V2 <= 4 *
      ## |   |   |   |   [7] V2 > 4
      ## |   |   |   |   |   [8] V9 <= 5 *
      ## |   |   |   |   |   [9] V9 > 5 *
      ## |   |   [10] V9 > 20 *
      ## |   [11] V4 > 1
      ## |   |   [12] V5 <= 0 *
      ## |   |   [13] V5 > 0
      ## |   |   |   [14] V2 <= 12 *
      ## |   |   |   [15] V2 > 12 *
      ## 
      ## $nodes[[24]]
      ## [1] root
      ## |   [2] V7 <= 60
      ## |   |   [3] V9 <= 13 *
      ## |   |   [4] V9 > 13 *
      ## |   [5] V7 > 60
      ## |   |   [6] V3 <= 64
      ## |   |   |   [7] V8 <= 1150
      ## |   |   |   |   [8] V8 <= 925
      ## |   |   |   |   |   [9] V8 <= 768 *
      ## |   |   |   |   |   [10] V8 > 768 *
      ## |   |   |   |   [11] V8 > 925 *
      ## |   |   |   [12] V8 > 1150 *
      ## |   |   [13] V3 > 64
      ## |   |   |   [14] V7 <= 80 *
      ## |   |   |   [15] V7 > 80 *
      ## 
      ## $nodes[[25]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V7 <= 70 *
      ## |   |   |   [5] V7 > 70
      ## |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   [7] V5 > 0 *
      ## |   |   [8] V5 > 1 *
      ## |   [9] V4 > 1
      ## |   |   [10] V9 <= 3 *
      ## |   |   [11] V9 > 3 *
      ## 
      ## $nodes[[26]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V5 <= 0
      ## |   |   |   [4] V3 <= 64
      ## |   |   |   |   [5] V7 <= 90 *
      ## |   |   |   |   [6] V7 > 90 *
      ## |   |   |   [7] V3 > 64 *
      ## |   |   [8] V5 > 0
      ## |   |   |   [9] V6 <= 80
      ## |   |   |   |   [10] V2 <= 13
      ## |   |   |   |   |   [11] V7 <= 70 *
      ## |   |   |   |   |   [12] V7 > 70 *
      ## |   |   |   |   [13] V2 > 13 *
      ## |   |   |   [14] V6 > 80 *
      ## |   [15] V5 > 1
      ## |   |   [16] V9 <= 20 *
      ## |   |   [17] V9 > 20 *
      ## 
      ## $nodes[[27]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V7 <= 80
      ## |   |   |   |   [5] V3 <= 66 *
      ## |   |   |   |   [6] V3 > 66 *
      ## |   |   |   [7] V7 > 80
      ## |   |   |   |   [8] V8 <= 1025 *
      ## |   |   |   |   [9] V8 > 1025 *
      ## |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V9 <= -1 *
      ## |   |   [13] V9 > -1
      ## |   |   |   [14] V6 <= 70 *
      ## |   |   |   [15] V6 > 70
      ## |   |   |   |   [16] V7 <= 80 *
      ## |   |   |   |   [17] V7 > 80 *
      ## 
      ## $nodes[[28]]
      ## [1] root
      ## |   [2] V7 <= 90
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V4 <= 1
      ## |   |   |   |   [5] V8 <= 1225
      ## |   |   |   |   |   [6] V8 <= 463 *
      ## |   |   |   |   |   [7] V8 > 463
      ## |   |   |   |   |   |   [8] V6 <= 80 *
      ## |   |   |   |   |   |   [9] V6 > 80 *
      ## |   |   |   |   [10] V8 > 1225 *
      ## |   |   |   [11] V4 > 1
      ## |   |   |   |   [12] V9 <= 4 *
      ## |   |   |   |   [13] V9 > 4 *
      ## |   |   [14] V5 > 1
      ## |   |   |   [15] V4 <= 1 *
      ## |   |   |   [16] V4 > 1 *
      ## |   [17] V7 > 90 *
      ## 
      ## $nodes[[29]]
      ## [1] root
      ## |   [2] V7 <= 90
      ## |   |   [3] V8 <= 675
      ## |   |   |   [4] V7 <= 80 *
      ## |   |   |   [5] V7 > 80 *
      ## |   |   [6] V8 > 675
      ## |   |   |   [7] V7 <= 60 *
      ## |   |   |   [8] V7 > 60
      ## |   |   |   |   [9] V3 <= 64
      ## |   |   |   |   |   [10] V5 <= 0 *
      ## |   |   |   |   |   [11] V5 > 0
      ## |   |   |   |   |   |   [12] V8 <= 975 *
      ## |   |   |   |   |   |   [13] V8 > 975 *
      ## |   |   |   |   [14] V3 > 64 *
      ## |   [15] V7 > 90 *
      ## 
      ## $nodes[[30]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V9 <= 18
      ## |   |   |   [5] V4 <= 1
      ## |   |   |   |   [6] V2 <= 11 *
      ## |   |   |   |   [7] V2 > 11 *
      ## |   |   |   [8] V4 > 1
      ## |   |   |   |   [9] V6 <= 90
      ## |   |   |   |   |   [10] V5 <= 0 *
      ## |   |   |   |   |   [11] V5 > 0
      ## |   |   |   |   |   |   [12] V3 <= 56 *
      ## |   |   |   |   |   |   [13] V3 > 56 *
      ## |   |   |   |   [14] V6 > 90 *
      ## |   |   [15] V9 > 18 *
      ## 
      ## $nodes[[31]]
      ## [1] root
      ## |   [2] V7 <= 80
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V3 <= 65 *
      ## |   |   |   [5] V3 > 65 *
      ## |   |   [6] V4 > 1
      ## |   |   |   [7] V2 <= 10 *
      ## |   |   |   [8] V2 > 10 *
      ## |   [9] V7 > 80
      ## |   |   [10] V5 <= 0 *
      ## |   |   [11] V5 > 0
      ## |   |   |   [12] V2 <= 13 *
      ## |   |   |   [13] V2 > 13 *
      ## 
      ## $nodes[[32]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V8 <= 575 *
      ## |   |   |   [5] V8 > 575
      ## |   |   |   |   [6] V2 <= 16
      ## |   |   |   |   |   [7] V3 <= 60 *
      ## |   |   |   |   |   [8] V3 > 60 *
      ## |   |   |   |   [9] V2 > 16 *
      ## |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 80
      ## |   |   |   [13] V9 <= 3 *
      ## |   |   |   [14] V9 > 3 *
      ## |   |   [15] V7 > 80 *
      ## 
      ## $nodes[[33]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V9 <= 2 *
      ## |   |   [4] V9 > 2 *
      ## |   [5] V6 > 70
      ## |   |   [6] V9 <= 3
      ## |   |   |   [7] V4 <= 1 *
      ## |   |   |   [8] V4 > 1 *
      ## |   |   [9] V9 > 3
      ## |   |   |   [10] V8 <= 575 *
      ## |   |   |   [11] V8 > 575
      ## |   |   |   |   [12] V7 <= 80 *
      ## |   |   |   |   [13] V7 > 80 *
      ## 
      ## $nodes[[34]]
      ## [1] root
      ## |   [2] V2 <= 12
      ## |   |   [3] V8 <= 1175
      ## |   |   |   [4] V7 <= 90
      ## |   |   |   |   [5] V5 <= 1
      ## |   |   |   |   |   [6] V3 <= 66 *
      ## |   |   |   |   |   [7] V3 > 66 *
      ## |   |   |   |   [8] V5 > 1 *
      ## |   |   |   [9] V7 > 90 *
      ## |   |   [10] V8 > 1175 *
      ## |   [11] V2 > 12
      ## |   |   [12] V7 <= 60 *
      ## |   |   [13] V7 > 60
      ## |   |   |   [14] V2 <= 15 *
      ## |   |   |   [15] V2 > 15
      ## |   |   |   |   [16] V2 <= 21 *
      ## |   |   |   |   [17] V2 > 21 *
      ## 
      ## $nodes[[35]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 45 *
      ## |   |   [4] V3 > 45
      ## |   |   |   [5] V9 <= 4
      ## |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   [7] V5 > 0
      ## |   |   |   |   |   [8] V7 <= 80 *
      ## |   |   |   |   |   [9] V7 > 80 *
      ## |   |   |   [10] V9 > 4
      ## |   |   |   |   [11] V4 <= 1
      ## |   |   |   |   |   [12] V5 <= 0 *
      ## |   |   |   |   |   [13] V5 > 0
      ## |   |   |   |   |   |   [14] V2 <= 11 *
      ## |   |   |   |   |   |   [15] V2 > 11 *
      ## |   |   |   |   [16] V4 > 1 *
      ## |   [17] V5 > 1
      ## |   |   [18] V9 <= 10 *
      ## |   |   [19] V9 > 10 *
      ## 
      ## $nodes[[36]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V5 <= 0
      ## |   |   |   [4] V9 <= 6
      ## |   |   |   |   [5] V2 <= 5 *
      ## |   |   |   |   [6] V2 > 5 *
      ## |   |   |   [7] V9 > 6 *
      ## |   |   [8] V5 > 0
      ## |   |   |   [9] V4 <= 1
      ## |   |   |   |   [10] V3 <= 59 *
      ## |   |   |   |   [11] V3 > 59
      ## |   |   |   |   |   [12] V8 <= 825 *
      ## |   |   |   |   |   [13] V8 > 825 *
      ## |   |   |   [14] V4 > 1 *
      ## |   [15] V5 > 1
      ## |   |   [16] V7 <= 60 *
      ## |   |   [17] V7 > 60 *
      ## 
      ## $nodes[[37]]
      ## [1] root
      ## |   [2] V7 <= 60
      ## |   |   [3] V9 <= 12 *
      ## |   |   [4] V9 > 12 *
      ## |   [5] V7 > 60
      ## |   |   [6] V8 <= 1100
      ## |   |   |   [7] V4 <= 1
      ## |   |   |   |   [8] V9 <= 20
      ## |   |   |   |   |   [9] V6 <= 80 *
      ## |   |   |   |   |   [10] V6 > 80 *
      ## |   |   |   |   [11] V9 > 20 *
      ## |   |   |   [12] V4 > 1
      ## |   |   |   |   [13] V7 <= 80 *
      ## |   |   |   |   [14] V7 > 80 *
      ## |   |   [15] V8 > 1100
      ## |   |   |   [16] V9 <= 2 *
      ## |   |   |   [17] V9 > 2 *
      ## 
      ## $nodes[[38]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V5 <= 1
      ## |   |   |   [5] V4 <= 1
      ## |   |   |   |   [6] V5 <= 0
      ## |   |   |   |   |   [7] V9 <= 5 *
      ## |   |   |   |   |   [8] V9 > 5 *
      ## |   |   |   |   [9] V5 > 0
      ## |   |   |   |   |   [10] V3 <= 63 *
      ## |   |   |   |   |   [11] V3 > 63 *
      ## |   |   |   [12] V4 > 1
      ## |   |   |   |   [13] V9 <= 4 *
      ## |   |   |   |   [14] V9 > 4 *
      ## |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[39]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V9 <= 20
      ## |   |   |   [4] V4 <= 1 *
      ## |   |   |   [5] V4 > 1 *
      ## |   |   [6] V9 > 20 *
      ## |   [7] V7 > 70
      ## |   |   [8] V5 <= 0
      ## |   |   |   [9] V6 <= 90 *
      ## |   |   |   [10] V6 > 90 *
      ## |   |   [11] V5 > 0
      ## |   |   |   [12] V2 <= 12
      ## |   |   |   |   [13] V6 <= 80
      ## |   |   |   |   |   [14] V9 <= 14 *
      ## |   |   |   |   |   [15] V9 > 14 *
      ## |   |   |   |   [16] V6 > 80 *
      ## |   |   |   [17] V2 > 12 *
      ## 
      ## $nodes[[40]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V7 <= 90
      ## |   |   |   [4] V9 <= 4
      ## |   |   |   |   [5] V4 <= 1 *
      ## |   |   |   |   [6] V4 > 1 *
      ## |   |   |   [7] V9 > 4
      ## |   |   |   |   [8] V4 <= 1
      ## |   |   |   |   |   [9] V7 <= 80
      ## |   |   |   |   |   |   [10] V7 <= 70 *
      ## |   |   |   |   |   |   [11] V7 > 70 *
      ## |   |   |   |   |   [12] V7 > 80 *
      ## |   |   |   |   [13] V4 > 1 *
      ## |   |   [14] V7 > 90 *
      ## |   [15] V5 > 1
      ## |   |   [16] V3 <= 65 *
      ## |   |   [17] V3 > 65 *
      ## 
      ## $nodes[[41]]
      ## [1] root
      ## |   [2] V3 <= 67
      ## |   |   [3] V6 <= 80
      ## |   |   |   [4] V3 <= 56 *
      ## |   |   |   [5] V3 > 56
      ## |   |   |   |   [6] V2 <= 15 *
      ## |   |   |   |   [7] V2 > 15 *
      ## |   |   [8] V6 > 80
      ## |   |   |   [9] V5 <= 0
      ## |   |   |   |   [10] V3 <= 56 *
      ## |   |   |   |   [11] V3 > 56 *
      ## |   |   |   [12] V5 > 0 *
      ## |   [13] V3 > 67
      ## |   |   [14] V9 <= 14
      ## |   |   |   [15] V6 <= 80
      ## |   |   |   |   [16] V5 <= 1 *
      ## |   |   |   |   [17] V5 > 1 *
      ## |   |   |   [18] V6 > 80 *
      ## |   |   [19] V9 > 14 *
      ## 
      ## $nodes[[42]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 70
      ## |   |   |   [4] V5 <= 1
      ## |   |   |   |   [5] V7 <= 70 *
      ## |   |   |   |   [6] V7 > 70
      ## |   |   |   |   |   [7] V5 <= 0 *
      ## |   |   |   |   |   [8] V5 > 0 *
      ## |   |   |   [9] V5 > 1 *
      ## |   |   [10] V3 > 70 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 70 *
      ## |   |   [13] V7 > 70
      ## |   |   |   [14] V2 <= 12
      ## |   |   |   |   [15] V5 <= 0 *
      ## |   |   |   |   [16] V5 > 0 *
      ## |   |   |   [17] V2 > 12 *
      ## 
      ## $nodes[[43]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V6 <= 90 *
      ## |   |   [4] V6 > 90 *
      ## |   [5] V5 > 0
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V2 <= 7
      ## |   |   |   |   [8] V7 <= 60 *
      ## |   |   |   |   [9] V7 > 60 *
      ## |   |   |   [10] V2 > 7
      ## |   |   |   |   [11] V6 <= 70 *
      ## |   |   |   |   [12] V6 > 70 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V8 <= 675 *
      ## |   |   |   [15] V8 > 675 *
      ## 
      ## $nodes[[44]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V5 <= 0
      ## |   |   |   [4] V6 <= 90 *
      ## |   |   |   [5] V6 > 90 *
      ## |   |   [6] V5 > 0
      ## |   |   |   [7] V4 <= 1
      ## |   |   |   |   [8] V7 <= 60 *
      ## |   |   |   |   [9] V7 > 60
      ## |   |   |   |   |   [10] V6 <= 80 *
      ## |   |   |   |   |   [11] V6 > 80 *
      ## |   |   |   [12] V4 > 1 *
      ## |   [13] V5 > 1
      ## |   |   [14] V7 <= 60 *
      ## |   |   [15] V7 > 60 *
      ## 
      ## $nodes[[45]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 67
      ## |   |   |   [4] V6 <= 70 *
      ## |   |   |   [5] V6 > 70
      ## |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   [7] V5 > 0
      ## |   |   |   |   |   [8] V3 <= 57 *
      ## |   |   |   |   |   [9] V3 > 57 *
      ## |   |   [10] V3 > 67
      ## |   |   |   [11] V9 <= 10 *
      ## |   |   |   [12] V9 > 10 *
      ## |   [13] V4 > 1
      ## |   |   [14] V5 <= 0 *
      ## |   |   [15] V5 > 0
      ## |   |   |   [16] V9 <= 0 *
      ## |   |   |   [17] V9 > 0 *
      ## 
      ## $nodes[[46]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V7 <= 90 *
      ## |   |   [4] V7 > 90 *
      ## |   [5] V5 > 0
      ## |   |   [6] V7 <= 60 *
      ## |   |   [7] V7 > 60
      ## |   |   |   [8] V2 <= 5 *
      ## |   |   |   [9] V2 > 5
      ## |   |   |   |   [10] V3 <= 59 *
      ## |   |   |   |   [11] V3 > 59
      ## |   |   |   |   |   [12] V5 <= 1
      ## |   |   |   |   |   |   [13] V6 <= 80 *
      ## |   |   |   |   |   |   [14] V6 > 80 *
      ## |   |   |   |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[47]]
      ## [1] root
      ## |   [2] V3 <= 64
      ## |   |   [3] V8 <= 1175
      ## |   |   |   [4] V5 <= 0 *
      ## |   |   |   [5] V5 > 0
      ## |   |   |   |   [6] V8 <= 925
      ## |   |   |   |   |   [7] V9 <= 14 *
      ## |   |   |   |   |   [8] V9 > 14 *
      ## |   |   |   |   [9] V8 > 925 *
      ## |   |   [10] V8 > 1175 *
      ## |   [11] V3 > 64
      ## |   |   [12] V9 <= 20
      ## |   |   |   [13] V6 <= 70 *
      ## |   |   |   [14] V6 > 70
      ## |   |   |   |   [15] V5 <= 0 *
      ## |   |   |   |   [16] V5 > 0 *
      ## |   |   [17] V9 > 20 *
      ## 
      ## $nodes[[48]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60 *
      ## |   [5] V6 > 70
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V3 <= 65
      ## |   |   |   |   [8] V7 <= 80 *
      ## |   |   |   |   [9] V7 > 80 *
      ## |   |   |   [10] V3 > 65 *
      ## |   |   [11] V4 > 1
      ## |   |   |   [12] V5 <= 0 *
      ## |   |   |   [13] V5 > 0 *
      ## 
      ## $nodes[[49]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V7 <= 70 *
      ## |   |   [4] V7 > 70
      ## |   |   |   [5] V9 <= 12
      ## |   |   |   |   [6] V6 <= 80 *
      ## |   |   |   |   [7] V6 > 80
      ## |   |   |   |   |   [8] V5 <= 0
      ## |   |   |   |   |   |   [9] V3 <= 51 *
      ## |   |   |   |   |   |   [10] V3 > 51 *
      ## |   |   |   |   |   [11] V5 > 0 *
      ## |   |   |   [12] V9 > 12 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 20 *
      ## |   |   [15] V9 > 20 *
      ## 
      ## $nodes[[50]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V9 <= 20
      ## |   |   |   |   [5] V6 <= 70 *
      ## |   |   |   |   [6] V6 > 70 *
      ## |   |   |   [7] V9 > 20 *
      ## |   |   [8] V4 > 1 *
      ## |   [9] V7 > 70
      ## |   |   [10] V3 <= 63
      ## |   |   |   [11] V4 <= 1
      ## |   |   |   |   [12] V5 <= 0 *
      ## |   |   |   |   [13] V5 > 0 *
      ## |   |   |   [14] V4 > 1 *
      ## |   |   [15] V3 > 63
      ## |   |   |   [16] V9 <= 3 *
      ## |   |   |   [17] V9 > 3 *
      ## 
      ## $nodes[[51]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V9 <= 20
      ## |   |   |   [4] V2 <= 3 *
      ## |   |   |   [5] V2 > 3 *
      ## |   |   [6] V9 > 20 *
      ## |   [7] V7 > 70
      ## |   |   [8] V3 <= 63
      ## |   |   |   [9] V4 <= 1 *
      ## |   |   |   [10] V4 > 1 *
      ## |   |   [11] V3 > 63
      ## |   |   |   [12] V8 <= 1100
      ## |   |   |   |   [13] V4 <= 1 *
      ## |   |   |   |   [14] V4 > 1 *
      ## |   |   |   [15] V8 > 1100 *
      ## 
      ## $nodes[[52]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V4 <= 1 *
      ## |   |   [4] V4 > 1 *
      ## |   [5] V6 > 70
      ## |   |   [6] V5 <= 0
      ## |   |   |   [7] V3 <= 63
      ## |   |   |   |   [8] V8 <= 768 *
      ## |   |   |   |   [9] V8 > 768 *
      ## |   |   |   [10] V3 > 63 *
      ## |   |   [11] V5 > 0
      ## |   |   |   [12] V4 <= 1
      ## |   |   |   |   [13] V8 <= 825 *
      ## |   |   |   |   [14] V8 > 825 *
      ## |   |   |   [15] V4 > 1
      ## |   |   |   |   [16] V3 <= 63 *
      ## |   |   |   |   [17] V3 > 63 *
      ## 
      ## $nodes[[53]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V5 <= 1
      ## |   |   |   |   [6] V3 <= 65
      ## |   |   |   |   |   [7] V5 <= 0 *
      ## |   |   |   |   |   [8] V5 > 0
      ## |   |   |   |   |   |   [9] V3 <= 53 *
      ## |   |   |   |   |   |   [10] V3 > 53 *
      ## |   |   |   |   [11] V3 > 65 *
      ## |   |   |   [12] V5 > 1 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V7 <= 80 *
      ## |   |   |   [15] V7 > 80 *
      ## 
      ## $nodes[[54]]
      ## [1] root
      ## |   [2] V3 <= 45 *
      ## |   [3] V3 > 45
      ## |   |   [4] V7 <= 70
      ## |   |   |   [5] V2 <= 3 *
      ## |   |   |   [6] V2 > 3
      ## |   |   |   |   [7] V9 <= 13
      ## |   |   |   |   |   [8] V8 <= 1025 *
      ## |   |   |   |   |   [9] V8 > 1025 *
      ## |   |   |   |   [10] V9 > 13 *
      ## |   |   [11] V7 > 70
      ## |   |   |   [12] V5 <= 0 *
      ## |   |   |   [13] V5 > 0
      ## |   |   |   |   [14] V4 <= 1
      ## |   |   |   |   |   [15] V7 <= 90 *
      ## |   |   |   |   |   [16] V7 > 90 *
      ## |   |   |   |   [17] V4 > 1 *
      ## 
      ## $nodes[[55]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V7 <= 80
      ## |   |   |   [5] V8 <= 538 *
      ## |   |   |   [6] V8 > 538
      ## |   |   |   |   [7] V6 <= 80 *
      ## |   |   |   |   [8] V6 > 80 *
      ## |   |   [9] V7 > 80
      ## |   |   |   [10] V2 <= 10
      ## |   |   |   |   [11] V7 <= 90 *
      ## |   |   |   |   [12] V7 > 90 *
      ## |   |   |   [13] V2 > 10
      ## |   |   |   |   [14] V4 <= 1 *
      ## |   |   |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[56]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V7 <= 80 *
      ## |   |   [4] V7 > 80 *
      ## |   [5] V5 > 0
      ## |   |   [6] V2 <= 10
      ## |   |   |   [7] V5 <= 1 *
      ## |   |   |   [8] V5 > 1 *
      ## |   |   [9] V2 > 10
      ## |   |   |   [10] V9 <= 20
      ## |   |   |   |   [11] V7 <= 80 *
      ## |   |   |   |   [12] V7 > 80 *
      ## |   |   |   [13] V9 > 20 *
      ## 
      ## $nodes[[57]]
      ## [1] root
      ## |   [2] V3 <= 48 *
      ## |   [3] V3 > 48
      ## |   |   [4] V7 <= 80
      ## |   |   |   [5] V5 <= 0 *
      ## |   |   |   [6] V5 > 0
      ## |   |   |   |   [7] V3 <= 63 *
      ## |   |   |   |   [8] V3 > 63
      ## |   |   |   |   |   [9] V9 <= 20 *
      ## |   |   |   |   |   [10] V9 > 20 *
      ## |   |   [11] V7 > 80
      ## |   |   |   [12] V5 <= 0 *
      ## |   |   |   [13] V5 > 0
      ## |   |   |   |   [14] V7 <= 90 *
      ## |   |   |   |   [15] V7 > 90 *
      ## 
      ## $nodes[[58]]
      ## [1] root
      ## |   [2] V3 <= 44 *
      ## |   [3] V3 > 44
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V7 <= 60 *
      ## |   |   |   [6] V7 > 60
      ## |   |   |   |   [7] V2 <= 11
      ## |   |   |   |   |   [8] V3 <= 64 *
      ## |   |   |   |   |   [9] V3 > 64 *
      ## |   |   |   |   [10] V2 > 11
      ## |   |   |   |   |   [11] V9 <= 5 *
      ## |   |   |   |   |   [12] V9 > 5 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V5 <= 1
      ## |   |   |   |   [15] V2 <= 12 *
      ## |   |   |   |   [16] V2 > 12 *
      ## |   |   |   [17] V5 > 1 *
      ## 
      ## $nodes[[59]]
      ## [1] root
      ## |   [2] V8 <= 488 *
      ## |   [3] V8 > 488
      ## |   |   [4] V5 <= 0
      ## |   |   |   [5] V4 <= 1 *
      ## |   |   |   [6] V4 > 1 *
      ## |   |   [7] V5 > 0
      ## |   |   |   [8] V9 <= 20
      ## |   |   |   |   [9] V8 <= 1100
      ## |   |   |   |   |   [10] V5 <= 1
      ## |   |   |   |   |   |   [11] V2 <= 12 *
      ## |   |   |   |   |   |   [12] V2 > 12 *
      ## |   |   |   |   |   [13] V5 > 1 *
      ## |   |   |   |   [14] V8 > 1100 *
      ## |   |   |   [15] V9 > 20 *
      ## 
      ## $nodes[[60]]
      ## [1] root
      ## |   [2] V6 <= 80
      ## |   |   [3] V9 <= 20
      ## |   |   |   [4] V5 <= 1
      ## |   |   |   |   [5] V7 <= 80 *
      ## |   |   |   |   [6] V7 > 80 *
      ## |   |   |   [7] V5 > 1 *
      ## |   |   [8] V9 > 20 *
      ## |   [9] V6 > 80
      ## |   |   [10] V4 <= 1
      ## |   |   |   [11] V2 <= 13
      ## |   |   |   |   [12] V9 <= 5 *
      ## |   |   |   |   [13] V9 > 5 *
      ## |   |   |   [14] V2 > 13 *
      ## |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[61]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V7 <= 70 *
      ## |   |   |   [5] V7 > 70
      ## |   |   |   |   [6] V8 <= 1039 *
      ## |   |   |   |   [7] V8 > 1039 *
      ## |   |   [8] V4 > 1
      ## |   |   |   [9] V6 <= 80 *
      ## |   |   |   [10] V6 > 80
      ## |   |   |   |   [11] V9 <= 2 *
      ## |   |   |   |   [12] V9 > 2 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 10 *
      ## |   |   [15] V9 > 10 *
      ## 
      ## $nodes[[62]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 63
      ## |   |   |   [4] V8 <= 1025
      ## |   |   |   |   [5] V4 <= 1 *
      ## |   |   |   |   [6] V4 > 1 *
      ## |   |   |   [7] V8 > 1025
      ## |   |   |   |   [8] V2 <= 5 *
      ## |   |   |   |   [9] V2 > 5 *
      ## |   |   [10] V3 > 63
      ## |   |   |   [11] V6 <= 80 *
      ## |   |   |   [12] V6 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V2 <= 5 *
      ## |   |   [15] V2 > 5 *
      ## 
      ## $nodes[[63]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V8 <= 910
      ## |   |   |   [4] V9 <= 20 *
      ## |   |   |   [5] V9 > 20 *
      ## |   |   [6] V8 > 910
      ## |   |   |   [7] V8 <= 1225
      ## |   |   |   |   [8] V5 <= 0 *
      ## |   |   |   |   [9] V5 > 0 *
      ## |   |   |   [10] V8 > 1225 *
      ## |   [11] V4 > 1
      ## |   |   [12] V8 <= 825
      ## |   |   |   [13] V6 <= 80 *
      ## |   |   |   [14] V6 > 80 *
      ## |   |   [15] V8 > 825
      ## |   |   |   [16] V5 <= 0 *
      ## |   |   |   [17] V5 > 0 *
      ## 
      ## $nodes[[64]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 65
      ## |   |   |   [4] V5 <= 0 *
      ## |   |   |   [5] V5 > 0 *
      ## |   |   [6] V3 > 65
      ## |   |   |   [7] V8 <= 925 *
      ## |   |   |   [8] V8 > 925 *
      ## |   [9] V4 > 1
      ## |   |   [10] V5 <= 1
      ## |   |   |   [11] V7 <= 80 *
      ## |   |   |   [12] V7 > 80 *
      ## |   |   [13] V5 > 1 *
      ## 
      ## $nodes[[65]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V6 <= 90 *
      ## |   |   [4] V6 > 90 *
      ## |   [5] V5 > 0
      ## |   |   [6] V5 <= 1
      ## |   |   |   [7] V4 <= 1
      ## |   |   |   |   [8] V9 <= 2 *
      ## |   |   |   |   [9] V9 > 2 *
      ## |   |   |   [10] V4 > 1
      ## |   |   |   |   [11] V8 <= 825 *
      ## |   |   |   |   [12] V8 > 825 *
      ## |   |   [13] V5 > 1
      ## |   |   |   [14] V8 <= 925 *
      ## |   |   |   [15] V8 > 925 *
      ## 
      ## $nodes[[66]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V8 <= 730 *
      ## |   |   |   [6] V8 > 730
      ## |   |   |   |   [7] V6 <= 80 *
      ## |   |   |   |   [8] V6 > 80
      ## |   |   |   |   |   [9] V2 <= 13 *
      ## |   |   |   |   |   [10] V2 > 13 *
      ## |   |   [11] V4 > 1
      ## |   |   |   [12] V3 <= 54 *
      ## |   |   |   [13] V3 > 54
      ## |   |   |   |   [14] V2 <= 10 *
      ## |   |   |   |   [15] V2 > 10 *
      ## 
      ## $nodes[[67]]
      ## [1] root
      ## |   [2] V3 <= 71
      ## |   |   [3] V6 <= 70 *
      ## |   |   [4] V6 > 70
      ## |   |   |   [5] V4 <= 1
      ## |   |   |   |   [6] V6 <= 80 *
      ## |   |   |   |   [7] V6 > 80
      ## |   |   |   |   |   [8] V5 <= 0 *
      ## |   |   |   |   |   [9] V5 > 0 *
      ## |   |   |   [10] V4 > 1
      ## |   |   |   |   [11] V2 <= 12 *
      ## |   |   |   |   [12] V2 > 12 *
      ## |   [13] V3 > 71 *
      ## 
      ## $nodes[[68]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 80
      ## |   |   |   [4] V3 <= 69
      ## |   |   |   |   [5] V2 <= 13 *
      ## |   |   |   |   [6] V2 > 13 *
      ## |   |   |   [7] V3 > 69 *
      ## |   |   [8] V6 > 80
      ## |   |   |   [9] V2 <= 13
      ## |   |   |   |   [10] V2 <= 6 *
      ## |   |   |   |   [11] V2 > 6 *
      ## |   |   |   [12] V2 > 13 *
      ## |   [13] V4 > 1
      ## |   |   [14] V8 <= 1060
      ## |   |   |   [15] V6 <= 80 *
      ## |   |   |   [16] V6 > 80 *
      ## |   |   [17] V8 > 1060 *
      ## 
      ## $nodes[[69]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V2 <= 6 *
      ## |   |   |   [6] V2 > 6
      ## |   |   |   |   [7] V3 <= 63 *
      ## |   |   |   |   [8] V3 > 63 *
      ## |   [9] V4 > 1
      ## |   |   [10] V7 <= 90
      ## |   |   |   [11] V9 <= 0 *
      ## |   |   |   [12] V9 > 0
      ## |   |   |   |   [13] V5 <= 1 *
      ## |   |   |   |   [14] V5 > 1 *
      ## |   |   [15] V7 > 90 *
      ## 
      ## $nodes[[70]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 71
      ## |   |   |   [4] V9 <= 20
      ## |   |   |   |   [5] V6 <= 90
      ## |   |   |   |   |   [6] V8 <= 1125
      ## |   |   |   |   |   |   [7] V9 <= 7 *
      ## |   |   |   |   |   |   [8] V9 > 7 *
      ## |   |   |   |   |   [9] V8 > 1125 *
      ## |   |   |   |   [10] V6 > 90 *
      ## |   |   |   [11] V9 > 20 *
      ## |   |   [12] V3 > 71 *
      ## |   [13] V4 > 1
      ## |   |   [14] V5 <= 0 *
      ## |   |   [15] V5 > 0
      ## |   |   |   [16] V2 <= 12 *
      ## |   |   |   [17] V2 > 12 *
      ## 
      ## $nodes[[71]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V7 <= 90 *
      ## |   |   |   [5] V7 > 90 *
      ## |   |   [6] V3 > 64 *
      ## |   [7] V5 > 0
      ## |   |   [8] V7 <= 60 *
      ## |   |   [9] V7 > 60
      ## |   |   |   [10] V4 <= 1
      ## |   |   |   |   [11] V9 <= 20
      ## |   |   |   |   |   [12] V8 <= 1025 *
      ## |   |   |   |   |   [13] V8 > 1025 *
      ## |   |   |   |   [14] V9 > 20 *
      ## |   |   |   [15] V4 > 1
      ## |   |   |   |   [16] V2 <= 12 *
      ## |   |   |   |   [17] V2 > 12 *
      ## 
      ## $nodes[[72]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V2 <= 3 *
      ## |   |   [4] V2 > 3
      ## |   |   |   [5] V4 <= 1 *
      ## |   |   |   [6] V4 > 1 *
      ## |   [7] V7 > 70
      ## |   |   [8] V5 <= 0
      ## |   |   |   [9] V7 <= 90 *
      ## |   |   |   [10] V7 > 90 *
      ## |   |   [11] V5 > 0
      ## |   |   |   [12] V4 <= 1
      ## |   |   |   |   [13] V3 <= 59 *
      ## |   |   |   |   [14] V3 > 59 *
      ## |   |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[73]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V6 <= 80
      ## |   |   |   |   [5] V7 <= 80 *
      ## |   |   |   |   [6] V7 > 80 *
      ## |   |   |   [7] V6 > 80
      ## |   |   |   |   [8] V3 <= 65 *
      ## |   |   |   |   [9] V3 > 65 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V6 <= 80 *
      ## |   |   |   [12] V6 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 11 *
      ## |   |   [15] V9 > 11 *
      ## 
      ## $nodes[[74]]
      ## [1] root
      ## |   [2] V6 <= 80
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V9 <= 20
      ## |   |   |   |   [5] V9 <= 8 *
      ## |   |   |   |   [6] V9 > 8 *
      ## |   |   |   [7] V9 > 20 *
      ## |   |   [8] V4 > 1
      ## |   |   |   [9] V6 <= 60 *
      ## |   |   |   [10] V6 > 60 *
      ## |   [11] V6 > 80
      ## |   |   [12] V4 <= 1
      ## |   |   |   [13] V9 <= 5 *
      ## |   |   |   [14] V9 > 5 *
      ## |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[75]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V5 <= 0 *
      ## |   |   |   [5] V5 > 0
      ## |   |   |   |   [6] V9 <= 14
      ## |   |   |   |   |   [7] V7 <= 80 *
      ## |   |   |   |   |   [8] V7 > 80 *
      ## |   |   |   |   [9] V9 > 14 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V9 <= 10 *
      ## |   |   |   [12] V9 > 10 *
      ## |   [13] V5 > 1
      ## |   |   [14] V2 <= 13 *
      ## |   |   [15] V2 > 13 *
      ## 
      ## $nodes[[76]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V3 <= 64
      ## |   |   |   [5] V6 <= 80 *
      ## |   |   |   [6] V6 > 80
      ## |   |   |   |   [7] V2 <= 13 *
      ## |   |   |   |   [8] V2 > 13 *
      ## |   |   [9] V3 > 64
      ## |   |   |   [10] V8 <= 575 *
      ## |   |   |   [11] V8 > 575
      ## |   |   |   |   [12] V8 <= 910 *
      ## |   |   |   |   [13] V8 > 910
      ## |   |   |   |   |   [14] V8 <= 1100 *
      ## |   |   |   |   |   [15] V8 > 1100 *
      ## 
      ## $nodes[[77]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V5 <= 1
      ## |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   [7] V5 > 0
      ## |   |   |   |   |   [8] V8 <= 825 *
      ## |   |   |   |   |   [9] V8 > 825
      ## |   |   |   |   |   |   [10] V7 <= 80 *
      ## |   |   |   |   |   |   [11] V7 > 80 *
      ## |   |   |   [12] V5 > 1 *
      ## |   [13] V4 > 1
      ## |   |   [14] V9 <= -1 *
      ## |   |   [15] V9 > -1
      ## |   |   |   [16] V7 <= 80 *
      ## |   |   |   [17] V7 > 80 *
      ## 
      ## $nodes[[78]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V3 <= 68
      ## |   |   |   |   [6] V6 <= 80 *
      ## |   |   |   |   [7] V6 > 80
      ## |   |   |   |   |   [8] V7 <= 80 *
      ## |   |   |   |   |   [9] V7 > 80 *
      ## |   |   |   [10] V3 > 68 *
      ## |   [11] V4 > 1
      ## |   |   [12] V9 <= -1 *
      ## |   |   [13] V9 > -1
      ## |   |   |   [14] V6 <= 70 *
      ## |   |   |   [15] V6 > 70
      ## |   |   |   |   [16] V8 <= 925 *
      ## |   |   |   |   [17] V8 > 925 *
      ## 
      ## $nodes[[79]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V5 <= 0
      ## |   |   |   [4] V8 <= 463 *
      ## |   |   |   [5] V8 > 463
      ## |   |   |   |   [6] V7 <= 80 *
      ## |   |   |   |   [7] V7 > 80 *
      ## |   |   [8] V5 > 0
      ## |   |   |   [9] V6 <= 80
      ## |   |   |   |   [10] V9 <= 0 *
      ## |   |   |   |   [11] V9 > 0
      ## |   |   |   |   |   [12] V3 <= 66 *
      ## |   |   |   |   |   [13] V3 > 66 *
      ## |   |   |   [14] V6 > 80
      ## |   |   |   |   [15] V4 <= 1 *
      ## |   |   |   |   [16] V4 > 1 *
      ## |   [17] V5 > 1
      ## |   |   [18] V4 <= 1 *
      ## |   |   [19] V4 > 1 *
      ## 
      ## $nodes[[80]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V7 <= 90
      ## |   |   |   [4] V6 <= 80
      ## |   |   |   |   [5] V2 <= 11 *
      ## |   |   |   |   [6] V2 > 11 *
      ## |   |   |   [7] V6 > 80
      ## |   |   |   |   [8] V7 <= 80 *
      ## |   |   |   |   [9] V7 > 80 *
      ## |   |   [10] V7 > 90 *
      ## |   [11] V5 > 1
      ## |   |   [12] V2 <= 13 *
      ## |   |   [13] V2 > 13 *
      ## 
      ## $nodes[[81]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V2 <= 11 *
      ## |   |   [4] V2 > 11 *
      ## |   [5] V5 > 0
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V9 <= 20
      ## |   |   |   |   [8] V6 <= 70 *
      ## |   |   |   |   [9] V6 > 70
      ## |   |   |   |   |   [10] V9 <= 8 *
      ## |   |   |   |   |   [11] V9 > 8 *
      ## |   |   |   [12] V9 > 20 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V9 <= 10 *
      ## |   |   |   [15] V9 > 10 *
      ## 
      ## $nodes[[82]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V3 <= 65
      ## |   |   |   |   [5] V9 <= 1 *
      ## |   |   |   |   [6] V9 > 1
      ## |   |   |   |   |   [7] V2 <= 4 *
      ## |   |   |   |   |   [8] V2 > 4 *
      ## |   |   |   [9] V3 > 65
      ## |   |   |   |   [10] V6 <= 80 *
      ## |   |   |   |   [11] V6 > 80 *
      ## |   |   [12] V4 > 1
      ## |   |   |   [13] V7 <= 80 *
      ## |   |   |   [14] V7 > 80 *
      ## |   [15] V5 > 1
      ## |   |   [16] V9 <= 20 *
      ## |   |   [17] V9 > 20 *
      ## 
      ## $nodes[[83]]
      ## [1] root
      ## |   [2] V3 <= 70
      ## |   |   [3] V8 <= 925
      ## |   |   |   [4] V5 <= 0 *
      ## |   |   |   [5] V5 > 0
      ## |   |   |   |   [6] V2 <= 6 *
      ## |   |   |   |   [7] V2 > 6 *
      ## |   |   [8] V8 > 925
      ## |   |   |   [9] V4 <= 1
      ## |   |   |   |   [10] V8 <= 1025 *
      ## |   |   |   |   [11] V8 > 1025 *
      ## |   |   |   [12] V4 > 1 *
      ## |   [13] V3 > 70 *
      ## 
      ## $nodes[[84]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V2 <= 12
      ## |   |   |   [4] V9 <= 3 *
      ## |   |   |   [5] V9 > 3 *
      ## |   |   [6] V2 > 12 *
      ## |   [7] V5 > 0
      ## |   |   [8] V3 <= 50 *
      ## |   |   [9] V3 > 50
      ## |   |   |   [10] V5 <= 1
      ## |   |   |   |   [11] V2 <= 13
      ## |   |   |   |   |   [12] V9 <= 14 *
      ## |   |   |   |   |   [13] V9 > 14 *
      ## |   |   |   |   [14] V2 > 13 *
      ## |   |   |   [15] V5 > 1
      ## |   |   |   |   [16] V2 <= 13 *
      ## |   |   |   |   [17] V2 > 13 *
      ## 
      ## $nodes[[85]]
      ## [1] root
      ## |   [2] V7 <= 60
      ## |   |   [3] V9 <= 13 *
      ## |   |   [4] V9 > 13 *
      ## |   [5] V7 > 60
      ## |   |   [6] V6 <= 70 *
      ## |   |   [7] V6 > 70
      ## |   |   |   [8] V2 <= 13
      ## |   |   |   |   [9] V5 <= 0 *
      ## |   |   |   |   [10] V5 > 0
      ## |   |   |   |   |   [11] V9 <= 13 *
      ## |   |   |   |   |   [12] V9 > 13 *
      ## |   |   |   [13] V2 > 13 *
      ## 
      ## $nodes[[86]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V9 <= 2 *
      ## |   |   [4] V9 > 2
      ## |   |   |   [5] V3 <= 70
      ## |   |   |   |   [6] V6 <= 80
      ## |   |   |   |   |   [7] V3 <= 62 *
      ## |   |   |   |   |   [8] V3 > 62 *
      ## |   |   |   |   [9] V6 > 80 *
      ## |   |   |   [10] V3 > 70 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 90
      ## |   |   |   [13] V2 <= 12
      ## |   |   |   |   [14] V2 <= 3 *
      ## |   |   |   |   [15] V2 > 3 *
      ## |   |   |   [16] V2 > 12 *
      ## |   |   [17] V7 > 90 *
      ## 
      ## $nodes[[87]]
      ## [1] root
      ## |   [2] V7 <= 60
      ## |   |   [3] V9 <= 14 *
      ## |   |   [4] V9 > 14 *
      ## |   [5] V7 > 60
      ## |   |   [6] V5 <= 1
      ## |   |   |   [7] V9 <= 5
      ## |   |   |   |   [8] V6 <= 80 *
      ## |   |   |   |   [9] V6 > 80
      ## |   |   |   |   |   [10] V7 <= 80 *
      ## |   |   |   |   |   [11] V7 > 80
      ## |   |   |   |   |   |   [12] V5 <= 0 *
      ## |   |   |   |   |   |   [13] V5 > 0 *
      ## |   |   |   [14] V9 > 5
      ## |   |   |   |   [15] V4 <= 1 *
      ## |   |   |   |   [16] V4 > 1 *
      ## |   |   [17] V5 > 1 *
      ## 
      ## $nodes[[88]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V8 <= 875
      ## |   |   |   [4] V3 <= 64 *
      ## |   |   |   [5] V3 > 64 *
      ## |   |   [6] V8 > 875
      ## |   |   |   [7] V7 <= 70 *
      ## |   |   |   [8] V7 > 70
      ## |   |   |   |   [9] V5 <= 0 *
      ## |   |   |   |   [10] V5 > 0 *
      ## |   [11] V4 > 1
      ## |   |   [12] V6 <= 70 *
      ## |   |   [13] V6 > 70
      ## |   |   |   [14] V9 <= 3 *
      ## |   |   |   [15] V9 > 3 *
      ## 
      ## $nodes[[89]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 70 *
      ## |   |   [4] V6 > 70
      ## |   |   |   [5] V2 <= 16
      ## |   |   |   |   [6] V8 <= 575 *
      ## |   |   |   |   [7] V8 > 575
      ## |   |   |   |   |   [8] V5 <= 0 *
      ## |   |   |   |   |   [9] V5 > 0 *
      ## |   |   |   [10] V2 > 16 *
      ## |   [11] V4 > 1
      ## |   |   [12] V6 <= 70 *
      ## |   |   [13] V6 > 70
      ## |   |   |   [14] V9 <= 14
      ## |   |   |   |   [15] V2 <= 5 *
      ## |   |   |   |   [16] V2 > 5 *
      ## |   |   |   [17] V9 > 14 *
      ## 
      ## $nodes[[90]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 0 *
      ## |   |   [4] V5 > 0
      ## |   |   |   [5] V8 <= 1275
      ## |   |   |   |   [6] V5 <= 1
      ## |   |   |   |   |   [7] V6 <= 80 *
      ## |   |   |   |   |   [8] V6 > 80 *
      ## |   |   |   |   [9] V5 > 1 *
      ## |   |   |   [10] V8 > 1275 *
      ## |   [11] V4 > 1
      ## |   |   [12] V6 <= 70 *
      ## |   |   [13] V6 > 70
      ## |   |   |   [14] V7 <= 80 *
      ## |   |   |   [15] V7 > 80 *
      ## 
      ## $nodes[[91]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60 *
      ## |   [5] V7 > 70
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V5 <= 0 *
      ## |   |   |   [8] V5 > 0
      ## |   |   |   |   [9] V7 <= 80 *
      ## |   |   |   |   [10] V7 > 80 *
      ## |   |   [11] V4 > 1
      ## |   |   |   [12] V7 <= 80 *
      ## |   |   |   [13] V7 > 80 *
      ## 
      ## $nodes[[92]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V7 <= 70 *
      ## |   |   [4] V7 > 70
      ## |   |   |   [5] V4 <= 1
      ## |   |   |   |   [6] V9 <= 5 *
      ## |   |   |   |   [7] V9 > 5 *
      ## |   |   |   [8] V4 > 1
      ## |   |   |   |   [9] V2 <= 12
      ## |   |   |   |   |   [10] V3 <= 64 *
      ## |   |   |   |   |   [11] V3 > 64 *
      ## |   |   |   |   [12] V2 > 12 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 11 *
      ## |   |   [15] V9 > 11 *
      ## 
      ## $nodes[[93]]
      ## [1] root
      ## |   [2] V3 <= 51 *
      ## |   [3] V3 > 51
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V7 <= 80
      ## |   |   |   |   [6] V3 <= 65 *
      ## |   |   |   |   [7] V3 > 65
      ## |   |   |   |   |   [8] V9 <= 17 *
      ## |   |   |   |   |   [9] V9 > 17 *
      ## |   |   |   [10] V7 > 80
      ## |   |   |   |   [11] V8 <= 993 *
      ## |   |   |   |   [12] V8 > 993 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V5 <= 0 *
      ## |   |   |   [15] V5 > 0
      ## |   |   |   |   [16] V3 <= 60 *
      ## |   |   |   |   [17] V3 > 60 *
      ## 
      ## $nodes[[94]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V9 <= 12
      ## |   |   |   [4] V5 <= 0
      ## |   |   |   |   [5] V4 <= 1 *
      ## |   |   |   |   [6] V4 > 1 *
      ## |   |   |   [7] V5 > 0
      ## |   |   |   |   [8] V4 <= 1 *
      ## |   |   |   |   [9] V4 > 1 *
      ## |   |   [10] V9 > 12
      ## |   |   |   [11] V4 <= 1 *
      ## |   |   |   [12] V4 > 1 *
      ## |   [13] V5 > 1
      ## |   |   [14] V7 <= 60 *
      ## |   |   [15] V7 > 60 *
      ## 
      ## $nodes[[95]]
      ## [1] root
      ## |   [2] V3 <= 46 *
      ## |   [3] V3 > 46
      ## |   |   [4] V7 <= 60
      ## |   |   |   [5] V9 <= 13 *
      ## |   |   |   [6] V9 > 13 *
      ## |   |   [7] V7 > 60
      ## |   |   |   [8] V6 <= 70 *
      ## |   |   |   [9] V6 > 70
      ## |   |   |   |   [10] V3 <= 63
      ## |   |   |   |   |   [11] V5 <= 0 *
      ## |   |   |   |   |   [12] V5 > 0 *
      ## |   |   |   |   [13] V3 > 63
      ## |   |   |   |   |   [14] V8 <= 993 *
      ## |   |   |   |   |   [15] V8 > 993 *
      ## 
      ## $nodes[[96]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V3 <= 64
      ## |   |   |   [5] V9 <= 3
      ## |   |   |   |   [6] V9 <= -1 *
      ## |   |   |   |   [7] V9 > -1 *
      ## |   |   |   [8] V9 > 3
      ## |   |   |   |   [9] V5 <= 0 *
      ## |   |   |   |   [10] V5 > 0 *
      ## |   |   [11] V3 > 64
      ## |   |   |   [12] V5 <= 1
      ## |   |   |   |   [13] V3 <= 68 *
      ## |   |   |   |   [14] V3 > 68 *
      ## |   |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[97]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 0 *
      ## |   |   [4] V5 > 0
      ## |   |   |   [5] V9 <= 24
      ## |   |   |   |   [6] V6 <= 70 *
      ## |   |   |   |   [7] V6 > 70 *
      ## |   |   |   [8] V9 > 24 *
      ## |   [9] V4 > 1
      ## |   |   [10] V5 <= 1
      ## |   |   |   [11] V9 <= 2 *
      ## |   |   |   [12] V9 > 2 *
      ## |   |   [13] V5 > 1 *
      ## 
      ## $nodes[[98]]
      ## [1] root
      ## |   [2] V7 <= 90
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V9 <= 2
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0 *
      ## |   |   |   [7] V9 > 2
      ## |   |   |   |   [8] V8 <= 1175
      ## |   |   |   |   |   [9] V6 <= 80 *
      ## |   |   |   |   |   [10] V6 > 80 *
      ## |   |   |   |   [11] V8 > 1175 *
      ## |   |   [12] V5 > 1
      ## |   |   |   [13] V9 <= 20 *
      ## |   |   |   [14] V9 > 20 *
      ## |   [15] V7 > 90 *
      ## 
      ## $nodes[[99]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 0 *
      ## |   |   [4] V5 > 0
      ## |   |   |   [5] V8 <= 925
      ## |   |   |   |   [6] V8 <= 513 *
      ## |   |   |   |   [7] V8 > 513 *
      ## |   |   |   [8] V8 > 925
      ## |   |   |   |   [9] V3 <= 68
      ## |   |   |   |   |   [10] V2 <= 12 *
      ## |   |   |   |   |   [11] V2 > 12 *
      ## |   |   |   |   [12] V3 > 68 *
      ## |   [13] V4 > 1
      ## |   |   [14] V6 <= 80
      ## |   |   |   [15] V2 <= 13 *
      ## |   |   |   [16] V2 > 13 *
      ## |   |   [17] V6 > 80 *
      ## 
      ## $nodes[[100]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V5 <= 0 *
      ## |   |   |   [6] V5 > 0
      ## |   |   |   |   [7] V8 <= 993 *
      ## |   |   |   |   [8] V8 > 993
      ## |   |   |   |   |   [9] V5 <= 1 *
      ## |   |   |   |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V9 <= 14
      ## |   |   |   [13] V6 <= 80 *
      ## |   |   |   [14] V6 > 80 *
      ## |   |   [15] V9 > 14 *
      ## 
      ## $nodes[[101]]
      ## [1] root
      ## |   [2] V8 <= 488
      ## |   |   [3] V9 <= 20 *
      ## |   |   [4] V9 > 20 *
      ## |   [5] V8 > 488
      ## |   |   [6] V5 <= 0
      ## |   |   |   [7] V4 <= 1 *
      ## |   |   |   [8] V4 > 1 *
      ## |   |   [9] V5 > 0
      ## |   |   |   [10] V7 <= 80
      ## |   |   |   |   [11] V6 <= 80
      ## |   |   |   |   |   [12] V3 <= 56 *
      ## |   |   |   |   |   [13] V3 > 56
      ## |   |   |   |   |   |   [14] V3 <= 65 *
      ## |   |   |   |   |   |   [15] V3 > 65 *
      ## |   |   |   |   [16] V6 > 80 *
      ## |   |   |   [17] V7 > 80
      ## |   |   |   |   [18] V8 <= 910 *
      ## |   |   |   |   [19] V8 > 910 *
      ## 
      ## $nodes[[102]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V2 <= 13 *
      ## |   |   [4] V2 > 13 *
      ## |   [5] V6 > 70
      ## |   |   [6] V7 <= 90
      ## |   |   |   [7] V4 <= 1
      ## |   |   |   |   [8] V3 <= 59 *
      ## |   |   |   |   [9] V3 > 59
      ## |   |   |   |   |   [10] V5 <= 0 *
      ## |   |   |   |   |   [11] V5 > 0 *
      ## |   |   |   [12] V4 > 1
      ## |   |   |   |   [13] V3 <= 62 *
      ## |   |   |   |   [14] V3 > 62 *
      ## |   |   [15] V7 > 90 *
      ## 
      ## $nodes[[103]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60 *
      ## |   [5] V6 > 70
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V3 <= 60 *
      ## |   |   |   [8] V3 > 60
      ## |   |   |   |   [9] V7 <= 70 *
      ## |   |   |   |   [10] V7 > 70 *
      ## |   |   [11] V4 > 1
      ## |   |   |   [12] V6 <= 80 *
      ## |   |   |   [13] V6 > 80 *
      ## 
      ## $nodes[[104]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V8 <= 463 *
      ## |   |   [4] V8 > 463
      ## |   |   |   [5] V4 <= 1 *
      ## |   |   |   [6] V4 > 1 *
      ## |   [7] V5 > 0
      ## |   |   [8] V4 <= 1
      ## |   |   |   [9] V3 <= 71
      ## |   |   |   |   [10] V2 <= 15
      ## |   |   |   |   |   [11] V9 <= 17
      ## |   |   |   |   |   |   [12] V9 <= 1 *
      ## |   |   |   |   |   |   [13] V9 > 1 *
      ## |   |   |   |   |   [14] V9 > 17 *
      ## |   |   |   |   [15] V2 > 15 *
      ## |   |   |   [16] V3 > 71 *
      ## |   |   [17] V4 > 1
      ## |   |   |   [18] V5 <= 1 *
      ## |   |   |   [19] V5 > 1 *
      ## 
      ## $nodes[[105]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V4 <= 1 *
      ## |   |   [4] V4 > 1 *
      ## |   [5] V6 > 70
      ## |   |   [6] V5 <= 0
      ## |   |   |   [7] V2 <= 13
      ## |   |   |   |   [8] V8 <= 725 *
      ## |   |   |   |   [9] V8 > 725 *
      ## |   |   |   [10] V2 > 13 *
      ## |   |   [11] V5 > 0
      ## |   |   |   [12] V8 <= 588 *
      ## |   |   |   [13] V8 > 588
      ## |   |   |   |   [14] V7 <= 70 *
      ## |   |   |   |   [15] V7 > 70
      ## |   |   |   |   |   [16] V3 <= 64 *
      ## |   |   |   |   |   [17] V3 > 64 *
      ## 
      ## $nodes[[106]]
      ## [1] root
      ## |   [2] V2 <= 12
      ## |   |   [3] V6 <= 90
      ## |   |   |   [4] V2 <= 2 *
      ## |   |   |   [5] V2 > 2
      ## |   |   |   |   [6] V3 <= 59 *
      ## |   |   |   |   [7] V3 > 59
      ## |   |   |   |   |   [8] V4 <= 1 *
      ## |   |   |   |   |   [9] V4 > 1 *
      ## |   |   [10] V6 > 90 *
      ## |   [11] V2 > 12
      ## |   |   [12] V2 <= 16 *
      ## |   |   [13] V2 > 16 *
      ## 
      ## $nodes[[107]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V7 <= 70 *
      ## |   |   |   [5] V7 > 70
      ## |   |   |   |   [6] V9 <= 8
      ## |   |   |   |   |   [7] V5 <= 0 *
      ## |   |   |   |   |   [8] V5 > 0 *
      ## |   |   |   |   [9] V9 > 8 *
      ## |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V9 <= -2 *
      ## |   |   [13] V9 > -2
      ## |   |   |   [14] V6 <= 80 *
      ## |   |   |   [15] V6 > 80 *
      ## 
      ## $nodes[[108]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V9 <= 1 *
      ## |   |   |   [5] V9 > 1
      ## |   |   |   |   [6] V2 <= 11
      ## |   |   |   |   |   [7] V7 <= 80 *
      ## |   |   |   |   |   [8] V7 > 80 *
      ## |   |   |   |   [9] V2 > 11 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V8 <= 538 *
      ## |   |   |   [12] V8 > 538
      ## |   |   |   |   [13] V9 <= 0 *
      ## |   |   |   |   [14] V9 > 0 *
      ## |   [15] V5 > 1 *
      ## 
      ## $nodes[[109]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 70 *
      ## |   |   [4] V6 > 70
      ## |   |   |   [5] V9 <= 5
      ## |   |   |   |   [6] V3 <= 63 *
      ## |   |   |   |   [7] V3 > 63 *
      ## |   |   |   [8] V9 > 5
      ## |   |   |   |   [9] V8 <= 1100
      ## |   |   |   |   |   [10] V9 <= 14 *
      ## |   |   |   |   |   [11] V9 > 14 *
      ## |   |   |   |   [12] V8 > 1100 *
      ## |   [13] V4 > 1
      ## |   |   [14] V6 <= 80 *
      ## |   |   [15] V6 > 80 *
      ## 
      ## $nodes[[110]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 71
      ## |   |   |   [4] V5 <= 1
      ## |   |   |   |   [5] V2 <= 16
      ## |   |   |   |   |   [6] V7 <= 80
      ## |   |   |   |   |   |   [7] V9 <= 5 *
      ## |   |   |   |   |   |   [8] V9 > 5 *
      ## |   |   |   |   |   [9] V7 > 80 *
      ## |   |   |   |   [10] V2 > 16 *
      ## |   |   |   [11] V5 > 1 *
      ## |   |   [12] V3 > 71 *
      ## |   [13] V4 > 1
      ## |   |   [14] V7 <= 80
      ## |   |   |   [15] V5 <= 1 *
      ## |   |   |   [16] V5 > 1 *
      ## |   |   [17] V7 > 80 *
      ## 
      ## $nodes[[111]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 70 *
      ## |   |   [4] V6 > 70
      ## |   |   |   [5] V7 <= 90
      ## |   |   |   |   [6] V9 <= 14
      ## |   |   |   |   |   [7] V8 <= 1175
      ## |   |   |   |   |   |   [8] V9 <= 5 *
      ## |   |   |   |   |   |   [9] V9 > 5 *
      ## |   |   |   |   |   [10] V8 > 1175 *
      ## |   |   |   |   [11] V9 > 14 *
      ## |   |   |   [12] V7 > 90 *
      ## |   [13] V4 > 1
      ## |   |   [14] V9 <= 2 *
      ## |   |   [15] V9 > 2
      ## |   |   |   [16] V2 <= 12 *
      ## |   |   |   [17] V2 > 12 *
      ## 
      ## $nodes[[112]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V9 <= 6 *
      ## |   |   [4] V9 > 6 *
      ## |   [5] V5 > 0
      ## |   |   [6] V9 <= 27
      ## |   |   |   [7] V5 <= 1
      ## |   |   |   |   [8] V4 <= 1
      ## |   |   |   |   |   [9] V2 <= 11 *
      ## |   |   |   |   |   [10] V2 > 11 *
      ## |   |   |   |   [11] V4 > 1 *
      ## |   |   |   [12] V5 > 1 *
      ## |   |   [13] V9 > 27 *
      ## 
      ## $nodes[[113]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V9 <= 5
      ## |   |   |   [4] V6 <= 80 *
      ## |   |   |   [5] V6 > 80
      ## |   |   |   |   [6] V6 <= 90 *
      ## |   |   |   |   [7] V6 > 90 *
      ## |   |   [8] V9 > 5
      ## |   |   |   [9] V3 <= 64
      ## |   |   |   |   [10] V7 <= 80 *
      ## |   |   |   |   [11] V7 > 80 *
      ## |   |   |   [12] V3 > 64 *
      ## |   [13] V5 > 1
      ## |   |   [14] V4 <= 1 *
      ## |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[114]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V8 <= 925
      ## |   |   |   |   [6] V3 <= 64 *
      ## |   |   |   |   [7] V3 > 64 *
      ## |   |   |   [8] V8 > 925
      ## |   |   |   |   [9] V5 <= 0 *
      ## |   |   |   |   [10] V5 > 0 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 80 *
      ## |   |   [13] V7 > 80
      ## |   |   |   [14] V5 <= 0 *
      ## |   |   |   [15] V5 > 0 *
      ## 
      ## $nodes[[115]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V7 <= 90
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0
      ## |   |   |   |   |   [7] V3 <= 53 *
      ## |   |   |   |   |   [8] V3 > 53 *
      ## |   |   |   [9] V7 > 90 *
      ## |   |   [10] V3 > 64
      ## |   |   |   [11] V6 <= 80 *
      ## |   |   |   [12] V6 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V6 <= 60 *
      ## |   |   [15] V6 > 60 *
      ## 
      ## $nodes[[116]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V8 <= 1275
      ## |   |   |   [4] V5 <= 1
      ## |   |   |   |   [5] V9 <= 11
      ## |   |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   |   [7] V5 > 0 *
      ## |   |   |   |   [8] V9 > 11 *
      ## |   |   |   [9] V5 > 1 *
      ## |   |   [10] V8 > 1275 *
      ## |   [11] V4 > 1
      ## |   |   [12] V2 <= 11 *
      ## |   |   [13] V2 > 11 *
      ## 
      ## $nodes[[117]]
      ## [1] root
      ## |   [2] V6 <= 80
      ## |   |   [3] V9 <= 27
      ## |   |   |   [4] V9 <= 1 *
      ## |   |   |   [5] V9 > 1
      ## |   |   |   |   [6] V5 <= 1 *
      ## |   |   |   |   [7] V5 > 1 *
      ## |   |   [8] V9 > 27 *
      ## |   [9] V6 > 80
      ## |   |   [10] V3 <= 64
      ## |   |   |   [11] V4 <= 1 *
      ## |   |   |   [12] V4 > 1 *
      ## |   |   [13] V3 > 64 *
      ## 
      ## $nodes[[118]]
      ## [1] root
      ## |   [2] V3 <= 45 *
      ## |   [3] V3 > 45
      ## |   |   [4] V5 <= 1
      ## |   |   |   [5] V2 <= 1 *
      ## |   |   |   [6] V2 > 1
      ## |   |   |   |   [7] V7 <= 90
      ## |   |   |   |   |   [8] V3 <= 66
      ## |   |   |   |   |   |   [9] V5 <= 0 *
      ## |   |   |   |   |   |   [10] V5 > 0 *
      ## |   |   |   |   |   [11] V3 > 66 *
      ## |   |   |   |   [12] V7 > 90 *
      ## |   |   [13] V5 > 1
      ## |   |   |   [14] V2 <= 13 *
      ## |   |   |   [15] V2 > 13 *
      ## 
      ## $nodes[[119]]
      ## [1] root
      ## |   [2] V3 <= 64
      ## |   |   [3] V5 <= 0 *
      ## |   |   [4] V5 > 0
      ## |   |   |   [5] V3 <= 50 *
      ## |   |   |   [6] V3 > 50
      ## |   |   |   |   [7] V8 <= 768 *
      ## |   |   |   |   [8] V8 > 768
      ## |   |   |   |   |   [9] V9 <= 5 *
      ## |   |   |   |   |   [10] V9 > 5 *
      ## |   [11] V3 > 64
      ## |   |   [12] V2 <= 5 *
      ## |   |   [13] V2 > 5
      ## |   |   |   [14] V8 <= 875 *
      ## |   |   |   [15] V8 > 875
      ## |   |   |   |   [16] V2 <= 15 *
      ## |   |   |   |   [17] V2 > 15 *
      ## 
      ## $nodes[[120]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V5 <= 0 *
      ## |   |   |   [5] V5 > 0
      ## |   |   |   |   [6] V6 <= 80 *
      ## |   |   |   |   [7] V6 > 80 *
      ## |   |   [8] V4 > 1
      ## |   |   |   [9] V9 <= 1 *
      ## |   |   |   [10] V9 > 1 *
      ## |   [11] V5 > 1
      ## |   |   [12] V8 <= 910 *
      ## |   |   [13] V8 > 910 *
      ## 
      ## $nodes[[121]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V7 <= 90
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0
      ## |   |   |   |   |   [7] V7 <= 70 *
      ## |   |   |   |   |   [8] V7 > 70 *
      ## |   |   |   [9] V7 > 90 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V6 <= 80 *
      ## |   |   |   [12] V6 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V4 <= 1 *
      ## |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[122]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V5 <= 1 *
      ## |   |   [4] V5 > 1 *
      ## |   [5] V6 > 70
      ## |   |   [6] V7 <= 70 *
      ## |   |   [7] V7 > 70
      ## |   |   |   [8] V3 <= 64
      ## |   |   |   |   [9] V2 <= 12
      ## |   |   |   |   |   [10] V9 <= 1 *
      ## |   |   |   |   |   [11] V9 > 1 *
      ## |   |   |   |   [12] V2 > 12 *
      ## |   |   |   [13] V3 > 64
      ## |   |   |   |   [14] V8 <= 1030 *
      ## |   |   |   |   [15] V8 > 1030 *
      ## 
      ## $nodes[[123]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V3 <= 60 *
      ## |   |   [4] V3 > 60 *
      ## |   [5] V5 > 0
      ## |   |   [6] V6 <= 70
      ## |   |   |   [7] V2 <= 13 *
      ## |   |   |   [8] V2 > 13 *
      ## |   |   [9] V6 > 70
      ## |   |   |   [10] V7 <= 90
      ## |   |   |   |   [11] V3 <= 54 *
      ## |   |   |   |   [12] V3 > 54
      ## |   |   |   |   |   [13] V4 <= 1 *
      ## |   |   |   |   |   [14] V4 > 1 *
      ## |   |   |   [15] V7 > 90 *
      ## 
      ## $nodes[[124]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 0 *
      ## |   |   [4] V5 > 0
      ## |   |   |   [5] V5 <= 1
      ## |   |   |   |   [6] V3 <= 63 *
      ## |   |   |   |   [7] V3 > 63 *
      ## |   |   |   [8] V5 > 1 *
      ## |   [9] V4 > 1
      ## |   |   [10] V6 <= 60 *
      ## |   |   [11] V6 > 60
      ## |   |   |   [12] V3 <= 64
      ## |   |   |   |   [13] V3 <= 58 *
      ## |   |   |   |   [14] V3 > 58 *
      ## |   |   |   [15] V3 > 64 *
      ## 
      ## $nodes[[125]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V9 <= 24
      ## |   |   |   [4] V4 <= 1
      ## |   |   |   |   [5] V5 <= 1 *
      ## |   |   |   |   [6] V5 > 1 *
      ## |   |   |   [7] V4 > 1 *
      ## |   |   [8] V9 > 24 *
      ## |   [9] V7 > 70
      ## |   |   [10] V4 <= 1
      ## |   |   |   [11] V9 <= 6
      ## |   |   |   |   [12] V5 <= 0 *
      ## |   |   |   |   [13] V5 > 0 *
      ## |   |   |   [14] V9 > 6 *
      ## |   |   [15] V4 > 1
      ## |   |   |   [16] V7 <= 90
      ## |   |   |   |   [17] V2 <= 12 *
      ## |   |   |   |   [18] V2 > 12 *
      ## |   |   |   [19] V7 > 90 *
      ## 
      ## $nodes[[126]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V2 <= 3 *
      ## |   |   [4] V2 > 3
      ## |   |   |   [5] V3 <= 63 *
      ## |   |   |   [6] V3 > 63
      ## |   |   |   |   [7] V9 <= 11 *
      ## |   |   |   |   [8] V9 > 11 *
      ## |   [9] V7 > 70
      ## |   |   [10] V7 <= 90
      ## |   |   |   [11] V9 <= 6
      ## |   |   |   |   [12] V6 <= 80 *
      ## |   |   |   |   [13] V6 > 80 *
      ## |   |   |   [14] V9 > 6
      ## |   |   |   |   [15] V6 <= 80 *
      ## |   |   |   |   [16] V6 > 80 *
      ## |   |   [17] V7 > 90 *
      ## 
      ## $nodes[[127]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V6 <= 80
      ## |   |   |   [4] V7 <= 80 *
      ## |   |   |   [5] V7 > 80 *
      ## |   |   [6] V6 > 80
      ## |   |   |   [7] V4 <= 1
      ## |   |   |   |   [8] V8 <= 875 *
      ## |   |   |   |   [9] V8 > 875
      ## |   |   |   |   |   [10] V2 <= 13 *
      ## |   |   |   |   |   [11] V2 > 13 *
      ## |   |   |   [12] V4 > 1
      ## |   |   |   |   [13] V3 <= 58 *
      ## |   |   |   |   [14] V3 > 58 *
      ## |   [15] V5 > 1
      ## |   |   [16] V2 <= 12 *
      ## |   |   [17] V2 > 12 *
      ## 
      ## $nodes[[128]]
      ## [1] root
      ## |   [2] V3 <= 45 *
      ## |   [3] V3 > 45
      ## |   |   [4] V6 <= 80
      ## |   |   |   [5] V4 <= 1
      ## |   |   |   |   [6] V9 <= 20
      ## |   |   |   |   |   [7] V6 <= 70 *
      ## |   |   |   |   |   [8] V6 > 70 *
      ## |   |   |   |   [9] V9 > 20 *
      ## |   |   |   [10] V4 > 1
      ## |   |   |   |   [11] V6 <= 70 *
      ## |   |   |   |   [12] V6 > 70 *
      ## |   |   [13] V6 > 80
      ## |   |   |   [14] V7 <= 80 *
      ## |   |   |   [15] V7 > 80
      ## |   |   |   |   [16] V9 <= 3 *
      ## |   |   |   |   [17] V9 > 3 *
      ## 
      ## $nodes[[129]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V4 <= 1 *
      ## |   |   [4] V4 > 1 *
      ## |   [5] V5 > 0
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V5 <= 1
      ## |   |   |   |   [8] V7 <= 80
      ## |   |   |   |   |   [9] V7 <= 70 *
      ## |   |   |   |   |   [10] V7 > 70 *
      ## |   |   |   |   [11] V7 > 80 *
      ## |   |   |   [12] V5 > 1 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V2 <= 12 *
      ## |   |   |   [15] V2 > 12 *
      ## 
      ## $nodes[[130]]
      ## [1] root
      ## |   [2] V3 <= 47 *
      ## |   [3] V3 > 47
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V7 <= 60 *
      ## |   |   |   [6] V7 > 60
      ## |   |   |   |   [7] V2 <= 16
      ## |   |   |   |   |   [8] V8 <= 875 *
      ## |   |   |   |   |   [9] V8 > 875
      ## |   |   |   |   |   |   [10] V7 <= 80 *
      ## |   |   |   |   |   |   [11] V7 > 80 *
      ## |   |   |   |   [12] V2 > 16 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V6 <= 90
      ## |   |   |   |   [15] V7 <= 80 *
      ## |   |   |   |   [16] V7 > 80 *
      ## |   |   |   [17] V6 > 90 *
      ## 
      ## $nodes[[131]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V3 <= 70
      ## |   |   |   [5] V7 <= 90
      ## |   |   |   |   [6] V5 <= 0
      ## |   |   |   |   |   [7] V8 <= 1039 *
      ## |   |   |   |   |   [8] V8 > 1039 *
      ## |   |   |   |   [9] V5 > 0
      ## |   |   |   |   |   [10] V9 <= 12 *
      ## |   |   |   |   |   [11] V9 > 12 *
      ## |   |   |   [12] V7 > 90 *
      ## |   |   [13] V3 > 70 *
      ## 
      ## $nodes[[132]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 70 *
      ## |   |   [4] V6 > 70
      ## |   |   |   [5] V2 <= 4 *
      ## |   |   |   [6] V2 > 4
      ## |   |   |   |   [7] V3 <= 60 *
      ## |   |   |   |   [8] V3 > 60
      ## |   |   |   |   |   [9] V8 <= 1030
      ## |   |   |   |   |   |   [10] V6 <= 80 *
      ## |   |   |   |   |   |   [11] V6 > 80 *
      ## |   |   |   |   |   [12] V8 > 1030 *
      ## |   [13] V4 > 1
      ## |   |   [14] V5 <= 1
      ## |   |   |   [15] V3 <= 60
      ## |   |   |   |   [16] V6 <= 80 *
      ## |   |   |   |   [17] V6 > 80 *
      ## |   |   |   [18] V3 > 60 *
      ## |   |   [19] V5 > 1 *
      ## 
      ## $nodes[[133]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V8 <= 1125
      ## |   |   |   |   [5] V9 <= 14
      ## |   |   |   |   |   [6] V7 <= 70 *
      ## |   |   |   |   |   [7] V7 > 70 *
      ## |   |   |   |   [8] V9 > 14 *
      ## |   |   |   [9] V8 > 1125 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V7 <= 90
      ## |   |   |   |   [12] V2 <= 12
      ## |   |   |   |   |   [13] V8 <= 925 *
      ## |   |   |   |   |   [14] V8 > 925 *
      ## |   |   |   |   [15] V2 > 12 *
      ## |   |   |   [16] V7 > 90 *
      ## |   [17] V5 > 1 *
      ## 
      ## $nodes[[134]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V9 <= 20
      ## |   |   |   [4] V4 <= 1 *
      ## |   |   |   [5] V4 > 1 *
      ## |   |   [6] V9 > 20 *
      ## |   [7] V6 > 70
      ## |   |   [8] V7 <= 70 *
      ## |   |   [9] V7 > 70
      ## |   |   |   [10] V5 <= 0
      ## |   |   |   |   [11] V7 <= 90 *
      ## |   |   |   |   [12] V7 > 90 *
      ## |   |   |   [13] V5 > 0
      ## |   |   |   |   [14] V7 <= 90
      ## |   |   |   |   |   [15] V2 <= 12 *
      ## |   |   |   |   |   [16] V2 > 12 *
      ## |   |   |   |   [17] V7 > 90 *
      ## 
      ## $nodes[[135]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V3 <= 65
      ## |   |   |   |   [5] V2 <= 4 *
      ## |   |   |   |   [6] V2 > 4
      ## |   |   |   |   |   [7] V8 <= 1025 *
      ## |   |   |   |   |   [8] V8 > 1025 *
      ## |   |   |   [9] V3 > 65 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V7 <= 80 *
      ## |   |   |   [12] V7 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V2 <= 13
      ## |   |   |   [15] V8 <= 910 *
      ## |   |   |   [16] V8 > 910 *
      ## |   |   [17] V2 > 13 *
      ## 
      ## $nodes[[136]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V3 <= 65
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0 *
      ## |   |   |   [7] V3 > 65
      ## |   |   |   |   [8] V9 <= 7 *
      ## |   |   |   |   [9] V9 > 7 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V9 <= 0 *
      ## |   |   |   [12] V9 > 0 *
      ## |   [13] V5 > 1
      ## |   |   [14] V2 <= 13 *
      ## |   |   [15] V2 > 13 *
      ## 
      ## $nodes[[137]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 71
      ## |   |   |   [4] V8 <= 925 *
      ## |   |   |   [5] V8 > 925
      ## |   |   |   |   [6] V3 <= 53 *
      ## |   |   |   |   [7] V3 > 53
      ## |   |   |   |   |   [8] V6 <= 80 *
      ## |   |   |   |   |   [9] V6 > 80 *
      ## |   |   [10] V3 > 71 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 90
      ## |   |   |   [13] V2 <= 15
      ## |   |   |   |   [14] V5 <= 0 *
      ## |   |   |   |   [15] V5 > 0 *
      ## |   |   |   [16] V2 > 15 *
      ## |   |   [17] V7 > 90 *
      ## 
      ## $nodes[[138]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V3 <= 64
      ## |   |   |   |   [5] V9 <= 2 *
      ## |   |   |   |   [6] V9 > 2 *
      ## |   |   |   [7] V3 > 64 *
      ## |   |   [8] V4 > 1
      ## |   |   |   [9] V2 <= 11 *
      ## |   |   |   [10] V2 > 11 *
      ## |   [11] V5 > 1
      ## |   |   [12] V4 <= 1 *
      ## |   |   [13] V4 > 1 *
      ## 
      ## $nodes[[139]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V3 <= 68
      ## |   |   |   |   [5] V6 <= 80 *
      ## |   |   |   |   [6] V6 > 80
      ## |   |   |   |   |   [7] V2 <= 5 *
      ## |   |   |   |   |   [8] V2 > 5 *
      ## |   |   |   [9] V3 > 68 *
      ## |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V9 <= -1 *
      ## |   |   [13] V9 > -1
      ## |   |   |   [14] V7 <= 70 *
      ## |   |   |   [15] V7 > 70
      ## |   |   |   |   [16] V6 <= 80 *
      ## |   |   |   |   [17] V6 > 80 *
      ## 
      ## $nodes[[140]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V4 <= 1 *
      ## |   |   [4] V4 > 1 *
      ## |   [5] V6 > 70
      ## |   |   [6] V3 <= 64
      ## |   |   |   [7] V2 <= 10
      ## |   |   |   |   [8] V7 <= 80 *
      ## |   |   |   |   [9] V7 > 80 *
      ## |   |   |   [10] V2 > 10
      ## |   |   |   |   [11] V6 <= 80 *
      ## |   |   |   |   [12] V6 > 80 *
      ## |   |   [13] V3 > 64
      ## |   |   |   [14] V8 <= 1030 *
      ## |   |   |   [15] V8 > 1030 *
      ## 
      ## $nodes[[141]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V7 <= 70 *
      ## |   |   [4] V7 > 70
      ## |   |   |   [5] V4 <= 1
      ## |   |   |   |   [6] V6 <= 80 *
      ## |   |   |   |   [7] V6 > 80
      ## |   |   |   |   |   [8] V8 <= 1039 *
      ## |   |   |   |   |   [9] V8 > 1039 *
      ## |   |   |   [10] V4 > 1
      ## |   |   |   |   [11] V3 <= 51 *
      ## |   |   |   |   [12] V3 > 51
      ## |   |   |   |   |   [13] V2 <= 6 *
      ## |   |   |   |   |   [14] V2 > 6 *
      ## |   [15] V5 > 1
      ## |   |   [16] V2 <= 13 *
      ## |   |   [17] V2 > 13 *
      ## 
      ## $nodes[[142]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V7 <= 90
      ## |   |   |   |   [5] V7 <= 70 *
      ## |   |   |   |   [6] V7 > 70
      ## |   |   |   |   |   [7] V9 <= 1 *
      ## |   |   |   |   |   [8] V9 > 1 *
      ## |   |   |   [9] V7 > 90 *
      ## |   |   [10] V3 > 64
      ## |   |   |   [11] V5 <= 0 *
      ## |   |   |   [12] V5 > 0
      ## |   |   |   |   [13] V3 <= 69 *
      ## |   |   |   |   [14] V3 > 69 *
      ## |   [15] V5 > 1
      ## |   |   [16] V9 <= 20 *
      ## |   |   [17] V9 > 20 *
      ## 
      ## $nodes[[143]]
      ## [1] root
      ## |   [2] V6 <= 60 *
      ## |   [3] V6 > 60
      ## |   |   [4] V5 <= 1
      ## |   |   |   [5] V9 <= 8
      ## |   |   |   |   [6] V7 <= 80 *
      ## |   |   |   |   [7] V7 > 80
      ## |   |   |   |   |   [8] V6 <= 90
      ## |   |   |   |   |   |   [9] V8 <= 975 *
      ## |   |   |   |   |   |   [10] V8 > 975 *
      ## |   |   |   |   |   [11] V6 > 90 *
      ## |   |   |   [12] V9 > 8
      ## |   |   |   |   [13] V4 <= 1
      ## |   |   |   |   |   [14] V2 <= 11 *
      ## |   |   |   |   |   [15] V2 > 11 *
      ## |   |   |   |   [16] V4 > 1 *
      ## |   |   [17] V5 > 1 *
      ## 
      ## $nodes[[144]]
      ## [1] root
      ## |   [2] V3 <= 63
      ## |   |   [3] V2 <= 10
      ## |   |   |   [4] V5 <= 0 *
      ## |   |   |   [5] V5 > 0 *
      ## |   |   [6] V2 > 10
      ## |   |   |   [7] V8 <= 1025 *
      ## |   |   |   [8] V8 > 1025 *
      ## |   [9] V3 > 63
      ## |   |   [10] V9 <= 27
      ## |   |   |   [11] V5 <= 1
      ## |   |   |   |   [12] V5 <= 0 *
      ## |   |   |   |   [13] V5 > 0
      ## |   |   |   |   |   [14] V7 <= 70 *
      ## |   |   |   |   |   [15] V7 > 70 *
      ## |   |   |   [16] V5 > 1 *
      ## |   |   [17] V9 > 27 *
      ## 
      ## $nodes[[145]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V9 <= 20
      ## |   |   |   [4] V4 <= 1 *
      ## |   |   |   [5] V4 > 1 *
      ## |   |   [6] V9 > 20 *
      ## |   [7] V6 > 70
      ## |   |   [8] V4 <= 1
      ## |   |   |   [9] V9 <= 6 *
      ## |   |   |   [10] V9 > 6
      ## |   |   |   |   [11] V7 <= 80 *
      ## |   |   |   |   [12] V7 > 80 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V9 <= 14
      ## |   |   |   |   [15] V2 <= 11 *
      ## |   |   |   |   [16] V2 > 11 *
      ## |   |   |   [17] V9 > 14 *
      ## 
      ## $nodes[[146]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V7 <= 90
      ## |   |   |   [4] V5 <= 0
      ## |   |   |   |   [5] V8 <= 775 *
      ## |   |   |   |   [6] V8 > 775 *
      ## |   |   |   [7] V5 > 0
      ## |   |   |   |   [8] V4 <= 1
      ## |   |   |   |   |   [9] V7 <= 80 *
      ## |   |   |   |   |   [10] V7 > 80 *
      ## |   |   |   |   [11] V4 > 1 *
      ## |   |   [12] V7 > 90 *
      ## |   [13] V5 > 1
      ## |   |   [14] V6 <= 60 *
      ## |   |   [15] V6 > 60 *
      ## 
      ## $nodes[[147]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V5 <= 0
      ## |   |   |   [5] V3 <= 64 *
      ## |   |   |   [6] V3 > 64 *
      ## |   |   [7] V5 > 0
      ## |   |   |   [8] V5 <= 1
      ## |   |   |   |   [9] V6 <= 80
      ## |   |   |   |   |   [10] V2 <= 13
      ## |   |   |   |   |   |   [11] V7 <= 70 *
      ## |   |   |   |   |   |   [12] V7 > 70 *
      ## |   |   |   |   |   [13] V2 > 13 *
      ## |   |   |   |   [14] V6 > 80 *
      ## |   |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[148]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V6 <= 80 *
      ## |   |   |   [5] V6 > 80
      ## |   |   |   |   [6] V8 <= 1175
      ## |   |   |   |   |   [7] V2 <= 11 *
      ## |   |   |   |   |   [8] V2 > 11 *
      ## |   |   |   |   [9] V8 > 1175 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V3 <= 63
      ## |   |   |   |   [12] V2 <= 10 *
      ## |   |   |   |   [13] V2 > 10 *
      ## |   |   |   [14] V3 > 63 *
      ## |   [15] V5 > 1
      ## |   |   [16] V6 <= 60 *
      ## |   |   [17] V6 > 60 *
      ## 
      ## $nodes[[149]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V2 <= 4 *
      ## |   |   |   [5] V2 > 4
      ## |   |   |   |   [6] V9 <= 15
      ## |   |   |   |   |   [7] V7 <= 90
      ## |   |   |   |   |   |   [8] V8 <= 993 *
      ## |   |   |   |   |   |   [9] V8 > 993 *
      ## |   |   |   |   |   [10] V7 > 90 *
      ## |   |   |   |   [11] V9 > 15 *
      ## |   |   [12] V5 > 1 *
      ## |   [13] V4 > 1
      ## |   |   [14] V5 <= 0 *
      ## |   |   [15] V5 > 0
      ## |   |   |   [16] V5 <= 1 *
      ## |   |   |   [17] V5 > 1 *
      ## 
      ## $nodes[[150]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60 *
      ## |   [5] V6 > 70
      ## |   |   [6] V5 <= 0
      ## |   |   |   [7] V7 <= 80 *
      ## |   |   |   [8] V7 > 80 *
      ## |   |   [9] V5 > 0
      ## |   |   |   [10] V9 <= 14
      ## |   |   |   |   [11] V7 <= 80
      ## |   |   |   |   |   [12] V2 <= 13 *
      ## |   |   |   |   |   [13] V2 > 13 *
      ## |   |   |   |   [14] V7 > 80 *
      ## |   |   |   [15] V9 > 14 *
      ## 
      ## $nodes[[151]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V2 <= 12 *
      ## |   |   [4] V2 > 12 *
      ## |   [5] V6 > 70
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V3 <= 47 *
      ## |   |   |   [8] V3 > 47
      ## |   |   |   |   [9] V8 <= 1125
      ## |   |   |   |   |   [10] V3 <= 60 *
      ## |   |   |   |   |   [11] V3 > 60
      ## |   |   |   |   |   |   [12] V7 <= 80 *
      ## |   |   |   |   |   |   [13] V7 > 80 *
      ## |   |   |   |   [14] V8 > 1125 *
      ## |   |   [15] V4 > 1
      ## |   |   |   [16] V9 <= 0 *
      ## |   |   |   [17] V9 > 0 *
      ## 
      ## $nodes[[152]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V7 <= 70
      ## |   |   |   [4] V9 <= 3 *
      ## |   |   |   [5] V9 > 3 *
      ## |   |   [6] V7 > 70 *
      ## |   [7] V6 > 70
      ## |   |   [8] V4 <= 1
      ## |   |   |   [9] V5 <= 0 *
      ## |   |   |   [10] V5 > 0
      ## |   |   |   |   [11] V6 <= 80 *
      ## |   |   |   |   [12] V6 > 80 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V6 <= 90
      ## |   |   |   |   [15] V7 <= 80 *
      ## |   |   |   |   [16] V7 > 80 *
      ## |   |   |   [17] V6 > 90 *
      ## 
      ## $nodes[[153]]
      ## [1] root
      ## |   [2] V3 <= 65
      ## |   |   [3] V7 <= 90
      ## |   |   |   [4] V9 <= 3 *
      ## |   |   |   [5] V9 > 3
      ## |   |   |   |   [6] V6 <= 70 *
      ## |   |   |   |   [7] V6 > 70
      ## |   |   |   |   |   [8] V2 <= 4 *
      ## |   |   |   |   |   [9] V2 > 4 *
      ## |   |   [10] V7 > 90 *
      ## |   [11] V3 > 65
      ## |   |   [12] V3 <= 71
      ## |   |   |   [13] V8 <= 875 *
      ## |   |   |   [14] V8 > 875
      ## |   |   |   |   [15] V2 <= 12 *
      ## |   |   |   |   [16] V2 > 12 *
      ## |   |   [17] V3 > 71 *
      ## 
      ## $nodes[[154]]
      ## [1] root
      ## |   [2] V3 <= 44 *
      ## |   [3] V3 > 44
      ## |   |   [4] V5 <= 1
      ## |   |   |   [5] V9 <= 23
      ## |   |   |   |   [6] V4 <= 1
      ## |   |   |   |   |   [7] V6 <= 90
      ## |   |   |   |   |   |   [8] V2 <= 7 *
      ## |   |   |   |   |   |   [9] V2 > 7 *
      ## |   |   |   |   |   [10] V6 > 90 *
      ## |   |   |   |   [11] V4 > 1
      ## |   |   |   |   |   [12] V2 <= 12 *
      ## |   |   |   |   |   [13] V2 > 12 *
      ## |   |   |   [14] V9 > 23 *
      ## |   |   [15] V5 > 1
      ## |   |   |   [16] V6 <= 60 *
      ## |   |   |   [17] V6 > 60 *
      ## 
      ## $nodes[[155]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V3 <= 70
      ## |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   [7] V5 > 0
      ## |   |   |   |   |   [8] V2 <= 13
      ## |   |   |   |   |   |   [9] V2 <= 11 *
      ## |   |   |   |   |   |   [10] V2 > 11 *
      ## |   |   |   |   |   [11] V2 > 13 *
      ## |   |   |   [12] V3 > 70 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V7 <= 90
      ## |   |   |   |   [15] V2 <= 12 *
      ## |   |   |   |   [16] V2 > 12 *
      ## |   |   |   [17] V7 > 90 *
      ## 
      ## $nodes[[156]]
      ## [1] root
      ## |   [2] V7 <= 90
      ## |   |   [3] V3 <= 63
      ## |   |   |   [4] V7 <= 60 *
      ## |   |   |   [5] V7 > 60
      ## |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   [7] V5 > 0
      ## |   |   |   |   |   [8] V4 <= 1 *
      ## |   |   |   |   |   [9] V4 > 1 *
      ## |   |   [10] V3 > 63
      ## |   |   |   [11] V8 <= 1125
      ## |   |   |   |   [12] V2 <= 15
      ## |   |   |   |   |   [13] V3 <= 71
      ## |   |   |   |   |   |   [14] V3 <= 68 *
      ## |   |   |   |   |   |   [15] V3 > 68 *
      ## |   |   |   |   |   [16] V3 > 71 *
      ## |   |   |   |   [17] V2 > 15 *
      ## |   |   |   [18] V8 > 1125 *
      ## |   [19] V7 > 90 *
      ## 
      ## $nodes[[157]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 60
      ## |   |   |   [4] V5 <= 0 *
      ## |   |   |   [5] V5 > 0 *
      ## |   |   [6] V3 > 60
      ## |   |   |   [7] V9 <= 14
      ## |   |   |   |   [8] V4 <= 1
      ## |   |   |   |   |   [9] V9 <= 2 *
      ## |   |   |   |   |   [10] V9 > 2 *
      ## |   |   |   |   [11] V4 > 1 *
      ## |   |   |   [12] V9 > 14 *
      ## |   [13] V5 > 1
      ## |   |   [14] V6 <= 60 *
      ## |   |   [15] V6 > 60 *
      ## 
      ## $nodes[[158]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V9 <= 16 *
      ## |   |   [4] V9 > 16 *
      ## |   [5] V6 > 70
      ## |   |   [6] V7 <= 60 *
      ## |   |   [7] V7 > 60
      ## |   |   |   [8] V4 <= 1
      ## |   |   |   |   [9] V3 <= 60 *
      ## |   |   |   |   [10] V3 > 60
      ## |   |   |   |   |   [11] V3 <= 68 *
      ## |   |   |   |   |   [12] V3 > 68 *
      ## |   |   |   [13] V4 > 1
      ## |   |   |   |   [14] V5 <= 0 *
      ## |   |   |   |   [15] V5 > 0 *
      ## 
      ## $nodes[[159]]
      ## [1] root
      ## |   [2] V3 <= 45 *
      ## |   [3] V3 > 45
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V9 <= 27
      ## |   |   |   |   [6] V6 <= 70 *
      ## |   |   |   |   [7] V6 > 70
      ## |   |   |   |   |   [8] V7 <= 90
      ## |   |   |   |   |   |   [9] V8 <= 1100
      ## |   |   |   |   |   |   |   [10] V2 <= 11 *
      ## |   |   |   |   |   |   |   [11] V2 > 11 *
      ## |   |   |   |   |   |   [12] V8 > 1100 *
      ## |   |   |   |   |   [13] V7 > 90 *
      ## |   |   |   [14] V9 > 27 *
      ## |   |   [15] V4 > 1
      ## |   |   |   [16] V7 <= 80
      ## |   |   |   |   [17] V5 <= 1 *
      ## |   |   |   |   [18] V5 > 1 *
      ## |   |   |   [19] V7 > 80 *
      ## 
      ## $nodes[[160]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 70
      ## |   |   |   [4] V6 <= 70 *
      ## |   |   |   [5] V6 > 70
      ## |   |   |   |   [6] V9 <= 6 *
      ## |   |   |   |   [7] V9 > 6
      ## |   |   |   |   |   [8] V8 <= 875 *
      ## |   |   |   |   |   [9] V8 > 875 *
      ## |   |   [10] V3 > 70 *
      ## |   [11] V4 > 1
      ## |   |   [12] V9 <= 0 *
      ## |   |   [13] V9 > 0
      ## |   |   |   [14] V8 <= 825 *
      ## |   |   |   [15] V8 > 825 *
      ## 
      ## $nodes[[161]]
      ## [1] root
      ## |   [2] V2 <= 15
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V5 <= 0
      ## |   |   |   |   [5] V9 <= 5 *
      ## |   |   |   |   [6] V9 > 5 *
      ## |   |   |   [7] V5 > 0
      ## |   |   |   |   [8] V4 <= 1
      ## |   |   |   |   |   [9] V6 <= 80 *
      ## |   |   |   |   |   [10] V6 > 80 *
      ## |   |   |   |   [11] V4 > 1 *
      ## |   |   [12] V5 > 1 *
      ## |   [13] V2 > 15
      ## |   |   [14] V2 <= 16 *
      ## |   |   [15] V2 > 16 *
      ## 
      ## $nodes[[162]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V5 <= 0
      ## |   |   |   [4] V9 <= 6
      ## |   |   |   |   [5] V2 <= 12 *
      ## |   |   |   |   [6] V2 > 12 *
      ## |   |   |   [7] V9 > 6 *
      ## |   |   [8] V5 > 0
      ## |   |   |   [9] V8 <= 925
      ## |   |   |   |   [10] V9 <= 14 *
      ## |   |   |   |   [11] V9 > 14 *
      ## |   |   |   [12] V8 > 925
      ## |   |   |   |   [13] V8 <= 1060 *
      ## |   |   |   |   [14] V8 > 1060 *
      ## |   [15] V5 > 1
      ## |   |   [16] V2 <= 7 *
      ## |   |   [17] V2 > 7 *
      ## 
      ## $nodes[[163]]
      ## [1] root
      ## |   [2] V3 <= 45 *
      ## |   [3] V3 > 45
      ## |   |   [4] V5 <= 0
      ## |   |   |   [5] V3 <= 64 *
      ## |   |   |   [6] V3 > 64 *
      ## |   |   [7] V5 > 0
      ## |   |   |   [8] V9 <= 27
      ## |   |   |   |   [9] V6 <= 70
      ## |   |   |   |   |   [10] V8 <= 1025 *
      ## |   |   |   |   |   [11] V8 > 1025 *
      ## |   |   |   |   [12] V6 > 70
      ## |   |   |   |   |   [13] V4 <= 1
      ## |   |   |   |   |   |   [14] V6 <= 80 *
      ## |   |   |   |   |   |   [15] V6 > 80 *
      ## |   |   |   |   |   [16] V4 > 1 *
      ## |   |   |   [17] V9 > 27 *
      ## 
      ## $nodes[[164]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V7 <= 70 *
      ## |   |   |   [5] V7 > 70
      ## |   |   |   |   [6] V9 <= 12
      ## |   |   |   |   |   [7] V3 <= 60 *
      ## |   |   |   |   |   [8] V3 > 60 *
      ## |   |   |   |   [9] V9 > 12 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V5 <= 0 *
      ## |   |   |   [12] V5 > 0 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 10 *
      ## |   |   [15] V9 > 10 *
      ## 
      ## $nodes[[165]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V8 <= 1275
      ## |   |   |   [4] V6 <= 70 *
      ## |   |   |   [5] V6 > 70
      ## |   |   |   |   [6] V2 <= 11
      ## |   |   |   |   |   [7] V2 <= 6 *
      ## |   |   |   |   |   [8] V2 > 6 *
      ## |   |   |   |   [9] V2 > 11 *
      ## |   |   [10] V8 > 1275 *
      ## |   [11] V4 > 1
      ## |   |   [12] V5 <= 0 *
      ## |   |   [13] V5 > 0
      ## |   |   |   [14] V2 <= 13 *
      ## |   |   |   [15] V2 > 13 *
      ## 
      ## $nodes[[166]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V5 <= 0
      ## |   |   |   |   [5] V8 <= 1025 *
      ## |   |   |   |   [6] V8 > 1025 *
      ## |   |   |   [7] V5 > 0
      ## |   |   |   |   [8] V8 <= 875 *
      ## |   |   |   |   [9] V8 > 875 *
      ## |   |   [10] V3 > 64
      ## |   |   |   [11] V6 <= 80 *
      ## |   |   |   [12] V6 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V2 <= 13 *
      ## |   |   [15] V2 > 13 *
      ## 
      ## $nodes[[167]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V6 <= 80
      ## |   |   |   [4] V5 <= 1 *
      ## |   |   |   [5] V5 > 1
      ## |   |   |   |   [6] V9 <= 20 *
      ## |   |   |   |   [7] V9 > 20 *
      ## |   |   [8] V6 > 80 *
      ## |   [9] V7 > 70
      ## |   |   [10] V5 <= 0
      ## |   |   |   [11] V2 <= 13 *
      ## |   |   |   [12] V2 > 13 *
      ## |   |   [13] V5 > 0
      ## |   |   |   [14] V3 <= 53 *
      ## |   |   |   [15] V3 > 53
      ## |   |   |   |   [16] V6 <= 80 *
      ## |   |   |   |   [17] V6 > 80 *
      ## 
      ## $nodes[[168]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 71
      ## |   |   |   [4] V5 <= 1
      ## |   |   |   |   [5] V8 <= 1150
      ## |   |   |   |   |   [6] V3 <= 64 *
      ## |   |   |   |   |   [7] V3 > 64 *
      ## |   |   |   |   [8] V8 > 1150 *
      ## |   |   |   [9] V5 > 1 *
      ## |   |   [10] V3 > 71 *
      ## |   [11] V4 > 1
      ## |   |   [12] V2 <= 10 *
      ## |   |   [13] V2 > 10
      ## |   |   |   [14] V9 <= 3 *
      ## |   |   |   [15] V9 > 3 *
      ## 
      ## $nodes[[169]]
      ## [1] root
      ## |   [2] V3 <= 46 *
      ## |   [3] V3 > 46
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V5 <= 1
      ## |   |   |   |   [6] V7 <= 90
      ## |   |   |   |   |   [7] V8 <= 1125
      ## |   |   |   |   |   |   [8] V2 <= 11 *
      ## |   |   |   |   |   |   [9] V2 > 11 *
      ## |   |   |   |   |   [10] V8 > 1125 *
      ## |   |   |   |   [11] V7 > 90 *
      ## |   |   |   [12] V5 > 1 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V5 <= 0 *
      ## |   |   |   [15] V5 > 0
      ## |   |   |   |   [16] V2 <= 16 *
      ## |   |   |   |   [17] V2 > 16 *
      ## 
      ## $nodes[[170]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 48 *
      ## |   |   [4] V3 > 48
      ## |   |   |   [5] V5 <= 1
      ## |   |   |   |   [6] V7 <= 90
      ## |   |   |   |   |   [7] V7 <= 70 *
      ## |   |   |   |   |   [8] V7 > 70 *
      ## |   |   |   |   [9] V7 > 90 *
      ## |   |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V5 <= 1
      ## |   |   |   [13] V7 <= 80 *
      ## |   |   |   [14] V7 > 80 *
      ## |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[171]]
      ## [1] root
      ## |   [2] V3 <= 66
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V7 <= 70 *
      ## |   |   |   [5] V7 > 70
      ## |   |   |   |   [6] V6 <= 80 *
      ## |   |   |   |   [7] V6 > 80 *
      ## |   |   [8] V4 > 1
      ## |   |   |   [9] V2 <= 16
      ## |   |   |   |   [10] V5 <= 0 *
      ## |   |   |   |   [11] V5 > 0 *
      ## |   |   |   [12] V2 > 16 *
      ## |   [13] V3 > 66
      ## |   |   [14] V4 <= 1
      ## |   |   |   [15] V5 <= 1 *
      ## |   |   |   [16] V5 > 1 *
      ## |   |   [17] V4 > 1 *
      ## 
      ## $nodes[[172]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V9 <= 20
      ## |   |   |   [4] V4 <= 1 *
      ## |   |   |   [5] V4 > 1 *
      ## |   |   [6] V9 > 20 *
      ## |   [7] V7 > 70
      ## |   |   [8] V3 <= 63
      ## |   |   |   [9] V4 <= 1
      ## |   |   |   |   [10] V6 <= 80 *
      ## |   |   |   |   [11] V6 > 80 *
      ## |   |   |   [12] V4 > 1 *
      ## |   |   [13] V3 > 63
      ## |   |   |   [14] V6 <= 80 *
      ## |   |   |   [15] V6 > 80 *
      ## 
      ## $nodes[[173]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 80
      ## |   |   |   [4] V5 <= 1 *
      ## |   |   |   [5] V5 > 1 *
      ## |   |   [6] V6 > 80
      ## |   |   |   [7] V9 <= -1 *
      ## |   |   |   [8] V9 > -1
      ## |   |   |   |   [9] V9 <= 4 *
      ## |   |   |   |   [10] V9 > 4 *
      ## |   [11] V4 > 1
      ## |   |   [12] V2 <= 11
      ## |   |   |   [13] V6 <= 80 *
      ## |   |   |   [14] V6 > 80 *
      ## |   |   [15] V2 > 11 *
      ## 
      ## $nodes[[174]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V9 <= 17
      ## |   |   |   |   [5] V9 <= 8
      ## |   |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   |   [7] V5 > 0 *
      ## |   |   |   |   [8] V9 > 8 *
      ## |   |   |   [9] V9 > 17 *
      ## |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 60 *
      ## |   |   [13] V7 > 60
      ## |   |   |   [14] V5 <= 0 *
      ## |   |   |   [15] V5 > 0
      ## |   |   |   |   [16] V8 <= 825 *
      ## |   |   |   |   [17] V8 > 825 *
      ## 
      ## $nodes[[175]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 80
      ## |   |   |   [4] V9 <= 20
      ## |   |   |   |   [5] V5 <= 1 *
      ## |   |   |   |   [6] V5 > 1 *
      ## |   |   |   [7] V9 > 20 *
      ## |   |   [8] V6 > 80
      ## |   |   |   [9] V9 <= 6 *
      ## |   |   |   [10] V9 > 6 *
      ## |   [11] V4 > 1
      ## |   |   [12] V9 <= -1 *
      ## |   |   [13] V9 > -1
      ## |   |   |   [14] V5 <= 0 *
      ## |   |   |   [15] V5 > 0
      ## |   |   |   |   [16] V6 <= 70 *
      ## |   |   |   |   [17] V6 > 70 *
      ## 
      ## $nodes[[176]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 64
      ## |   |   |   [4] V2 <= 12
      ## |   |   |   |   [5] V3 <= 50 *
      ## |   |   |   |   [6] V3 > 50
      ## |   |   |   |   |   [7] V5 <= 0 *
      ## |   |   |   |   |   [8] V5 > 0 *
      ## |   |   |   [9] V2 > 12 *
      ## |   |   [10] V3 > 64
      ## |   |   |   [11] V9 <= 10
      ## |   |   |   |   [12] V5 <= 0 *
      ## |   |   |   |   [13] V5 > 0 *
      ## |   |   |   [14] V9 > 10 *
      ## |   [15] V5 > 1
      ## |   |   [16] V6 <= 60 *
      ## |   |   [17] V6 > 60 *
      ## 
      ## $nodes[[177]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V2 <= 15
      ## |   |   |   [4] V5 <= 1
      ## |   |   |   |   [5] V8 <= 1175
      ## |   |   |   |   |   [6] V6 <= 80 *
      ## |   |   |   |   |   [7] V6 > 80 *
      ## |   |   |   |   [8] V8 > 1175 *
      ## |   |   |   [9] V5 > 1 *
      ## |   |   [10] V2 > 15 *
      ## |   [11] V4 > 1
      ## |   |   [12] V6 <= 70 *
      ## |   |   [13] V6 > 70
      ## |   |   |   [14] V8 <= 975 *
      ## |   |   |   [15] V8 > 975 *
      ## 
      ## $nodes[[178]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V3 <= 63
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0 *
      ## |   |   |   [7] V3 > 63
      ## |   |   |   |   [8] V9 <= 17
      ## |   |   |   |   |   [9] V6 <= 80 *
      ## |   |   |   |   |   [10] V6 > 80 *
      ## |   |   |   |   [11] V9 > 17 *
      ## |   |   [12] V4 > 1
      ## |   |   |   [13] V5 <= 0 *
      ## |   |   |   [14] V5 > 0 *
      ## |   [15] V5 > 1
      ## |   |   [16] V3 <= 62 *
      ## |   |   [17] V3 > 62 *
      ## 
      ## $nodes[[179]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V3 <= 51 *
      ## |   |   [4] V3 > 51
      ## |   |   |   [5] V5 <= 0
      ## |   |   |   |   [6] V9 <= 5 *
      ## |   |   |   |   [7] V9 > 5 *
      ## |   |   |   [8] V5 > 0
      ## |   |   |   |   [9] V6 <= 80
      ## |   |   |   |   |   [10] V9 <= 14 *
      ## |   |   |   |   |   [11] V9 > 14 *
      ## |   |   |   |   [12] V6 > 80
      ## |   |   |   |   |   [13] V8 <= 875 *
      ## |   |   |   |   |   [14] V8 > 875 *
      ## |   [15] V5 > 1
      ## |   |   [16] V8 <= 413 *
      ## |   |   [17] V8 > 413 *
      ## 
      ## $nodes[[180]]
      ## [1] root
      ## |   [2] V7 <= 60 *
      ## |   [3] V7 > 60
      ## |   |   [4] V4 <= 1
      ## |   |   |   [5] V7 <= 90
      ## |   |   |   |   [6] V8 <= 910 *
      ## |   |   |   |   [7] V8 > 910
      ## |   |   |   |   |   [8] V9 <= -1 *
      ## |   |   |   |   |   [9] V9 > -1
      ## |   |   |   |   |   |   [10] V2 <= 11 *
      ## |   |   |   |   |   |   [11] V2 > 11 *
      ## |   |   |   [12] V7 > 90 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V7 <= 80 *
      ## |   |   |   [15] V7 > 80
      ## |   |   |   |   [16] V9 <= 6 *
      ## |   |   |   |   [17] V9 > 6 *
      ## 
      ## $nodes[[181]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V6 <= 70 *
      ## |   |   |   [6] V6 > 70
      ## |   |   |   |   [7] V3 <= 63
      ## |   |   |   |   |   [8] V3 <= 58 *
      ## |   |   |   |   |   [9] V3 > 58 *
      ## |   |   |   |   [10] V3 > 63 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 80
      ## |   |   |   [13] V3 <= 60 *
      ## |   |   |   [14] V3 > 60 *
      ## |   |   [15] V7 > 80 *
      ## 
      ## $nodes[[182]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V8 <= 925 *
      ## |   |   |   [5] V8 > 925
      ## |   |   |   |   [6] V5 <= 0 *
      ## |   |   |   |   [7] V5 > 0 *
      ## |   |   [8] V4 > 1
      ## |   |   |   [9] V8 <= 925 *
      ## |   |   |   [10] V8 > 925 *
      ## |   [11] V5 > 1
      ## |   |   [12] V9 <= 11 *
      ## |   |   [13] V9 > 11 *
      ## 
      ## $nodes[[183]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V8 <= 588 *
      ## |   |   [4] V8 > 588
      ## |   |   |   [5] V7 <= 80 *
      ## |   |   |   [6] V7 > 80 *
      ## |   [7] V5 > 0
      ## |   |   [8] V2 <= 12
      ## |   |   |   [9] V6 <= 70 *
      ## |   |   |   [10] V6 > 70
      ## |   |   |   |   [11] V3 <= 64 *
      ## |   |   |   |   [12] V3 > 64 *
      ## |   |   [13] V2 > 12
      ## |   |   |   [14] V9 <= 15
      ## |   |   |   |   [15] V4 <= 1 *
      ## |   |   |   |   [16] V4 > 1 *
      ## |   |   |   [17] V9 > 15 *
      ## 
      ## $nodes[[184]]
      ## [1] root
      ## |   [2] V3 <= 65
      ## |   |   [3] V6 <= 70 *
      ## |   |   [4] V6 > 70
      ## |   |   |   [5] V8 <= 1060
      ## |   |   |   |   [6] V6 <= 80 *
      ## |   |   |   |   [7] V6 > 80
      ## |   |   |   |   |   [8] V7 <= 80 *
      ## |   |   |   |   |   [9] V7 > 80 *
      ## |   |   |   [10] V8 > 1060 *
      ## |   [11] V3 > 65
      ## |   |   [12] V4 <= 1
      ## |   |   |   [13] V5 <= 1 *
      ## |   |   |   [14] V5 > 1 *
      ## |   |   [15] V4 > 1 *
      ## 
      ## $nodes[[185]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V4 <= 1 *
      ## |   |   [4] V4 > 1 *
      ## |   [5] V6 > 70
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V9 <= 2 *
      ## |   |   |   [8] V9 > 2
      ## |   |   |   |   [9] V5 <= 0 *
      ## |   |   |   |   [10] V5 > 0
      ## |   |   |   |   |   [11] V7 <= 70 *
      ## |   |   |   |   |   [12] V7 > 70 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V3 <= 59 *
      ## |   |   |   [15] V3 > 59 *
      ## 
      ## $nodes[[186]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V8 <= 575 *
      ## |   |   [4] V8 > 575
      ## |   |   |   [5] V8 <= 1025
      ## |   |   |   |   [6] V9 <= 7
      ## |   |   |   |   |   [7] V4 <= 1 *
      ## |   |   |   |   |   [8] V4 > 1 *
      ## |   |   |   |   [9] V9 > 7 *
      ## |   |   |   [10] V8 > 1025
      ## |   |   |   |   [11] V4 <= 1
      ## |   |   |   |   |   [12] V9 <= 2 *
      ## |   |   |   |   |   [13] V9 > 2 *
      ## |   |   |   |   [14] V4 > 1 *
      ## |   [15] V5 > 1
      ## |   |   [16] V2 <= 13 *
      ## |   |   [17] V2 > 13 *
      ## 
      ## $nodes[[187]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V3 <= 71
      ## |   |   |   [4] V9 <= 15
      ## |   |   |   |   [5] V9 <= -1 *
      ## |   |   |   |   [6] V9 > -1
      ## |   |   |   |   |   [7] V2 <= 5 *
      ## |   |   |   |   |   [8] V2 > 5
      ## |   |   |   |   |   |   [9] V9 <= 8 *
      ## |   |   |   |   |   |   [10] V9 > 8 *
      ## |   |   |   [11] V9 > 15 *
      ## |   |   [12] V3 > 71 *
      ## |   [13] V4 > 1
      ## |   |   [14] V9 <= 13
      ## |   |   |   [15] V5 <= 0 *
      ## |   |   |   [16] V5 > 0 *
      ## |   |   [17] V9 > 13 *
      ## 
      ## $nodes[[188]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 0 *
      ## |   |   [4] V5 > 0
      ## |   |   |   [5] V9 <= 24
      ## |   |   |   |   [6] V5 <= 1
      ## |   |   |   |   |   [7] V7 <= 70 *
      ## |   |   |   |   |   [8] V7 > 70 *
      ## |   |   |   |   [9] V5 > 1 *
      ## |   |   |   [10] V9 > 24 *
      ## |   [11] V4 > 1
      ## |   |   [12] V5 <= 0 *
      ## |   |   [13] V5 > 0
      ## |   |   |   [14] V2 <= 12 *
      ## |   |   |   [15] V2 > 12 *
      ## 
      ## $nodes[[189]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V7 <= 60 *
      ## |   |   [4] V7 > 60
      ## |   |   |   [5] V5 <= 0 *
      ## |   |   |   [6] V5 > 0
      ## |   |   |   |   [7] V3 <= 70
      ## |   |   |   |   |   [8] V8 <= 993 *
      ## |   |   |   |   |   [9] V8 > 993 *
      ## |   |   |   |   [10] V3 > 70 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 90
      ## |   |   |   [13] V7 <= 80 *
      ## |   |   |   [14] V7 > 80 *
      ## |   |   [15] V7 > 90 *
      ## 
      ## $nodes[[190]]
      ## [1] root
      ## |   [2] V3 <= 70
      ## |   |   [3] V7 <= 90
      ## |   |   |   [4] V4 <= 1
      ## |   |   |   |   [5] V5 <= 0 *
      ## |   |   |   |   [6] V5 > 0
      ## |   |   |   |   |   [7] V8 <= 1025 *
      ## |   |   |   |   |   [8] V8 > 1025 *
      ## |   |   |   [9] V4 > 1
      ## |   |   |   |   [10] V5 <= 0 *
      ## |   |   |   |   [11] V5 > 0 *
      ## |   |   [12] V7 > 90 *
      ## |   [13] V3 > 70
      ## |   |   [14] V5 <= 1 *
      ## |   |   [15] V5 > 1 *
      ## 
      ## $nodes[[191]]
      ## [1] root
      ## |   [2] V3 <= 71
      ## |   |   [3] V2 <= 21
      ## |   |   |   [4] V6 <= 70 *
      ## |   |   |   [5] V6 > 70
      ## |   |   |   |   [6] V3 <= 64
      ## |   |   |   |   |   [7] V5 <= 0 *
      ## |   |   |   |   |   [8] V5 > 0
      ## |   |   |   |   |   |   [9] V7 <= 80 *
      ## |   |   |   |   |   |   [10] V7 > 80 *
      ## |   |   |   |   [11] V3 > 64 *
      ## |   |   [12] V2 > 21 *
      ## |   [13] V3 > 71 *
      ## 
      ## $nodes[[192]]
      ## [1] root
      ## |   [2] V6 <= 70
      ## |   |   [3] V2 <= 7 *
      ## |   |   [4] V2 > 7 *
      ## |   [5] V6 > 70
      ## |   |   [6] V9 <= 5
      ## |   |   |   [7] V7 <= 90
      ## |   |   |   |   [8] V6 <= 80 *
      ## |   |   |   |   [9] V6 > 80 *
      ## |   |   |   [10] V7 > 90 *
      ## |   |   [11] V9 > 5
      ## |   |   |   [12] V3 <= 64
      ## |   |   |   |   [13] V3 <= 56 *
      ## |   |   |   |   [14] V3 > 56 *
      ## |   |   |   [15] V3 > 64 *
      ## 
      ## $nodes[[193]]
      ## [1] root
      ## |   [2] V5 <= 0
      ## |   |   [3] V9 <= 3 *
      ## |   |   [4] V9 > 3
      ## |   |   |   [5] V2 <= 4 *
      ## |   |   |   [6] V2 > 4 *
      ## |   [7] V5 > 0
      ## |   |   [8] V7 <= 60 *
      ## |   |   [9] V7 > 60
      ## |   |   |   [10] V2 <= 5 *
      ## |   |   |   [11] V2 > 5
      ## |   |   |   |   [12] V9 <= 7 *
      ## |   |   |   |   [13] V9 > 7 *
      ## 
      ## $nodes[[194]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V5 <= 0
      ## |   |   |   [4] V7 <= 80 *
      ## |   |   |   [5] V7 > 80 *
      ## |   |   [6] V5 > 0
      ## |   |   |   [7] V4 <= 1
      ## |   |   |   |   [8] V6 <= 80
      ## |   |   |   |   |   [9] V9 <= 15 *
      ## |   |   |   |   |   [10] V9 > 15 *
      ## |   |   |   |   [11] V6 > 80 *
      ## |   |   |   [12] V4 > 1
      ## |   |   |   |   [13] V7 <= 80 *
      ## |   |   |   |   [14] V7 > 80 *
      ## |   [15] V5 > 1
      ## |   |   [16] V2 <= 13 *
      ## |   |   [17] V2 > 13 *
      ## 
      ## $nodes[[195]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V9 <= 27
      ## |   |   |   [4] V5 <= 1
      ## |   |   |   |   [5] V2 <= 11
      ## |   |   |   |   |   [6] V7 <= 70 *
      ## |   |   |   |   |   [7] V7 > 70 *
      ## |   |   |   |   [8] V2 > 11 *
      ## |   |   |   [9] V5 > 1 *
      ## |   |   [10] V9 > 27 *
      ## |   [11] V4 > 1
      ## |   |   [12] V2 <= 12
      ## |   |   |   [13] V6 <= 80 *
      ## |   |   |   [14] V6 > 80 *
      ## |   |   [15] V2 > 12 *
      ## 
      ## $nodes[[196]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V6 <= 70 *
      ## |   |   [4] V6 > 70
      ## |   |   |   [5] V5 <= 0 *
      ## |   |   |   [6] V5 > 0
      ## |   |   |   |   [7] V3 <= 59 *
      ## |   |   |   |   [8] V3 > 59
      ## |   |   |   |   |   [9] V9 <= 8 *
      ## |   |   |   |   |   [10] V9 > 8 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 80
      ## |   |   |   [13] V9 <= 0 *
      ## |   |   |   [14] V9 > 0 *
      ## |   |   [15] V7 > 80 *
      ## 
      ## $nodes[[197]]
      ## [1] root
      ## |   [2] V7 <= 60
      ## |   |   [3] V5 <= 1 *
      ## |   |   [4] V5 > 1 *
      ## |   [5] V7 > 60
      ## |   |   [6] V4 <= 1
      ## |   |   |   [7] V8 <= 488 *
      ## |   |   |   [8] V8 > 488
      ## |   |   |   |   [9] V2 <= 15
      ## |   |   |   |   |   [10] V5 <= 0 *
      ## |   |   |   |   |   [11] V5 > 0 *
      ## |   |   |   |   [12] V2 > 15 *
      ## |   |   [13] V4 > 1
      ## |   |   |   [14] V5 <= 0 *
      ## |   |   |   [15] V5 > 0
      ## |   |   |   |   [16] V3 <= 65 *
      ## |   |   |   |   [17] V3 > 65 *
      ## 
      ## $nodes[[198]]
      ## [1] root
      ## |   [2] V5 <= 1
      ## |   |   [3] V4 <= 1
      ## |   |   |   [4] V6 <= 80
      ## |   |   |   |   [5] V9 <= 14 *
      ## |   |   |   |   [6] V9 > 14 *
      ## |   |   |   [7] V6 > 80
      ## |   |   |   |   [8] V9 <= 6 *
      ## |   |   |   |   [9] V9 > 6 *
      ## |   |   [10] V4 > 1
      ## |   |   |   [11] V6 <= 80 *
      ## |   |   |   [12] V6 > 80 *
      ## |   [13] V5 > 1
      ## |   |   [14] V9 <= 10 *
      ## |   |   [15] V9 > 10 *
      ## 
      ## $nodes[[199]]
      ## [1] root
      ## |   [2] V4 <= 1
      ## |   |   [3] V5 <= 1
      ## |   |   |   [4] V3 <= 65
      ## |   |   |   |   [5] V9 <= 1 *
      ## |   |   |   |   [6] V9 > 1
      ## |   |   |   |   |   [7] V3 <= 56 *
      ## |   |   |   |   |   [8] V3 > 56 *
      ## |   |   |   [9] V3 > 65 *
      ## |   |   [10] V5 > 1 *
      ## |   [11] V4 > 1
      ## |   |   [12] V7 <= 90
      ## |   |   |   [13] V5 <= 0 *
      ## |   |   |   [14] V5 > 0
      ## |   |   |   |   [15] V9 <= 0 *
      ## |   |   |   |   [16] V9 > 0 *
      ## |   |   [17] V7 > 90 *
      ## 
      ## $nodes[[200]]
      ## [1] root
      ## |   [2] V7 <= 70
      ## |   |   [3] V6 <= 70
      ## |   |   |   [4] V9 <= 11 *
      ## |   |   |   [5] V9 > 11 *
      ## |   |   [6] V6 > 70 *
      ## |   [7] V7 > 70
      ## |   |   [8] V4 <= 1
      ## |   |   |   [9] V2 <= 6 *
      ## |   |   |   [10] V2 > 6 *
      ## |   |   [11] V4 > 1
      ## |   |   |   [12] V3 <= 57 *
      ## |   |   |   [13] V3 > 57 *
      ## 
      ## 
      ## $data
      ##     Surv(time, status) inst age sex ph.ecog ph.karno pat.karno
      ## 9                  218    1  53   1       1       70        80
      ## 10                 166    7  61   1       2       70        70
      ## 11                 170    6  57   1       1       80        80
      ## 15                 567   12  57   1       1       80        70
      ## 17                 613   22  70   1       1       90       100
      ## 18                 707   16  63   1       2       50        70
      ## 19                  61    1  56   2       2       60        60
      ## 21                 301   11  67   1       1       80        80
      ## 22                  81    6  49   2       0      100        70
      ## 24                 371   15  58   1       0       90       100
      ## 26                 520   12  70   2       1       90        80
      ## 27                 574    4  60   1       0      100       100
      ## 28                 118   13  70   1       3       60        70
      ## 29                 390   13  53   1       1       80        70
      ## 30                  12    1  74   1       2       70        50
      ## 31                 473   12  69   2       1       90        90
      ## 32                  26    1  73   1       2       60        70
      ## 34                 107   16  60   2       2       50        60
      ## 35                  53   12  61   1       2       70       100
      ## 37                 814   22  65   1       2       70        60
      ## 38                965+   15  66   2       1       70        90
      ## 39                  93    1  74   1       2       50        40
      ## 40                 731    1  64   2       1       80       100
      ## 41                 460    5  70   1       1       80        60
      ## 42                 153   11  73   2       2       60        70
      ## 43                 433   10  59   2       0       90        90
      ## 45                 583    7  68   1       1       60        70
      ## 46                  95    7  76   2       2       60        60
      ## 47                 303    1  74   1       0       90        70
      ## 48                 519    3  63   1       1       80        70
      ## 49                 643   13  74   1       0       90        90
      ## 50                 765   22  50   2       1       90       100
      ## 53                  53   21  68   1       0       90       100
      ## 54                 246    1  58   1       0      100        90
      ## 55                 689    6  59   1       1       90        80
      ## 57                   5    5  65   2       0      100        80
      ## 59                 687    3  58   2       1       80        80
      ## 60                 345    1  64   2       1       90        80
      ## 61                 444   22  75   2       2       70        70
      ## 62                 223   12  48   1       1       90        80
      ## 64                  60   11  65   2       1       90        80
      ## 65                 163    3  69   1       1       80        60
      ## 66                  65    3  68   1       2       70        50
      ## 68                821+    5  64   2       0       90        70
      ## 69                 428   22  68   1       0      100        80
      ## 70                 230    6  67   1       1       80       100
      ## 71                840+   13  63   1       0       90        90
      ## 72                 305    3  48   2       1       80        90
      ## 73                  11    5  74   1       2       70       100
      ## 75                 226   21  53   2       1       90        80
      ## 76                 426   12  71   2       1       90        90
      ## 77                 705    1  51   2       0      100        80
      ## 78                 363    6  56   2       1       80        70
      ## 80                 176    1  73   1       0       90        70
      ## 81                 791    4  59   1       0      100        80
      ## 82                  95   13  55   1       1       70        90
      ## 83                196+   11  42   1       1       80        80
      ## 84                 167   21  44   2       1       80        90
      ## 85                806+   16  44   1       1       80        80
      ## 86                 284    6  71   1       1       80        90
      ## 87                 641   22  62   2       1       80        80
      ## 88                 147   21  61   1       0      100        90
      ## 89                740+   13  44   2       1       90        80
      ## 90                 163    1  72   1       2       70        70
      ## 91                 655   11  63   1       0      100        90
      ## 93                  88    5  66   1       1       90        80
      ## 94                 245   10  57   2       1       80        60
      ## 96                  30   12  72   1       2       80        60
      ## 99                 477   11  64   1       1       90       100
      ## 101               559+    1  58   2       0      100       100
      ## 102                450    6  69   2       1       80        90
      ## 106                156   12  66   1       1       80        90
      ## 107               529+   26  54   2       1       80       100
      ## 109                429   21  55   1       1      100        80
      ## 110                351    3  75   2       2       60        50
      ## 111                 15   13  69   1       0       90        70
      ## 112                181    1  44   1       1       80        90
      ## 113                283   10  80   1       1       80       100
      ## 116                 13    1  76   1       2       70        70
      ## 117                212    3  49   1       2       70        60
      ## 118                524    1  68   1       2       60        70
      ## 119                288   16  66   1       2       70        60
      ## 120                363   15  80   1       1       80        90
      ## 122                199   26  60   2       2       70        80
      ## 123                550    3  69   2       1       70        80
      ## 124                 54   11  72   1       2       60        60
      ## 125                558    1  70   1       0       90        90
      ## 126                207   22  66   1       1       80        80
      ## 127                 92    7  50   1       1       80        60
      ## 128                 60   12  64   1       1       80        90
      ## 129               551+   16  77   2       2       80        60
      ## 131                293    4  59   2       1       80        80
      ## 133                353    6  47   1       0      100        90
      ## 135                267    1  67   1       0       90        70
      ## 136               511+   22  74   2       2       60        40
      ## 139                457    1  54   1       1       90        90
      ## 140                337    5  56   1       0      100       100
      ## 141                201   21  73   2       2       70        60
      ## 142               404+    3  74   1       1       80        70
      ## 143                222   26  76   1       2       70        70
      ## 144                 62    1  65   2       1       80        90
      ## 145               458+   11  57   1       1       80       100
      ## 147                353   16  71   1       0      100        80
      ## 148                163   16  54   1       1       90        80
      ## 149                 31   12  82   1       0      100        90
      ## 151                229   13  70   1       1       70        60
      ## 155                156   32  55   1       2       70        30
      ## 158                291    4  62   1       2       70        60
      ## 159                179   12  63   1       1       80        70
      ## 160               376+    1  56   2       1       80        90
      ## 161               384+   32  62   2       0       90        90
      ## 162                268   10  44   2       1       90       100
      ## 163               292+   11  69   1       2       60        70
      ## 164                142    6  63   1       1       90        80
      ## 165               413+    7  64   1       1       80        70
      ## 166               266+   16  57   2       0       90        90
      ## 168                320   21  46   1       0      100       100
      ## 169                181    6  61   1       1       90        90
      ## 170                285   12  65   1       0      100        90
      ## 171               301+   13  61   1       1       90       100
      ## 172                348    2  58   2       0       90        80
      ## 173                197    2  56   1       1       90        60
      ## 174               382+   16  43   2       0      100        90
      ## 175               303+    1  53   1       1       90        80
      ## 176               296+   13  59   2       1       80       100
      ## 177                180    1  56   1       2       60        80
      ## 179                145    1  53   2       1       80        90
      ## 180               269+    7  74   2       0      100       100
      ## 181               300+   13  60   1       0      100       100
      ## 182               284+    1  39   1       0      100        90
      ## 185               292+   12  51   2       0       90        80
      ## 186               332+   12  45   2       0       90       100
      ## 187                285    2  72   2       2       70        90
      ## 188               259+    3  58   1       0       90        80
      ## 189                110   15  64   1       1       80        60
      ## 190                286   22  53   1       0       90        90
      ## 191                270   16  72   1       1       80        90
      ## 194               225+    1  64   1       1       90        80
      ## 195                269   22  71   1       1       90        90
      ## 196               225+   12  70   1       0      100       100
      ## 197               243+   32  63   2       1       80        90
      ## 199               276+    1  52   2       0      100        80
      ## 200                135   32  60   1       1       90        70
      ## 201                 79   15  64   2       1       90        90
      ## 202                 59   22  73   1       1       60        60
      ## 203               240+   32  63   2       0       90       100
      ## 204               202+    3  50   2       0      100       100
      ## 205               235+   26  63   2       0      100        90
      ## 208                239   13  50   2       2       60        60
      ## 211               252+    1  60   2       0      100        90
      ## 212               221+    6  67   1       1       80        70
      ## 213               185+   15  69   1       1       90        70
      ## 216               222+   11  65   1       1       90        70
      ## 218                183   21  76   1       2       80        60
      ## 219               211+   11  70   2       2       70        30
      ## 220               175+    2  57   2       0       80        80
      ## 221               197+   22  67   1       1       80        90
      ## 222               203+   11  71   2       1       80        90
      ## 225               191+   13  39   1       0       90        90
      ## 226               105+   32  75   2       2       60        70
      ## 227               174+    6  66   1       1       90       100
      ## 228               177+   22  58   2       1       80        90
      ##     meal.cal wt.loss
      ## 9        825      16
      ## 10       271      34
      ## 11      1025      27
      ## 15      2600      60
      ## 17      1150      -5
      ## 18      1025      22
      ## 19       238      10
      ## 21      1025      17
      ## 22      1175      -8
      ## 24       975      13
      ## 26       825       6
      ## 27      1025     -13
      ## 28      1075      20
      ## 29       875      -7
      ## 30       305      20
      ## 31      1025      -1
      ## 32       388      20
      ## 34       925     -15
      ## 35      1075      10
      ## 37       513      28
      ## 38       875       4
      ## 39      1225      24
      ## 40      1175      15
      ## 41       975      10
      ## 42      1075      11
      ## 43       363      27
      ## 45      1025       7
      ## 46       625     -24
      ## 47       463      30
      ## 48      1025      10
      ## 49      1425       2
      ## 50      1175       4
      ## 53      1025       0
      ## 54      1175       7
      ## 55      1300      15
      ## 57       338       5
      ## 59      1225      10
      ## 60      1075      -3
      ## 61       438       8
      ## 62      1300      68
      ## 64      1025       0
      ## 65      1125       0
      ## 66       825       8
      ## 68      1025       3
      ## 69      1039       0
      ## 70       488      23
      ## 71      1175      -1
      ## 72       538      29
      ## 73      1175       0
      ## 75       825       3
      ## 76      1075      19
      ## 77      1300       0
      ## 78      1225      -2
      ## 80       169      30
      ## 81       768       5
      ## 82      1500      15
      ## 83      1425       8
      ## 84       588      -1
      ## 85      1025       1
      ## 86      1100      14
      ## 87      1150       1
      ## 88      1175       4
      ## 89       588      39
      ## 90       910       2
      ## 91       975      -1
      ## 93       875       8
      ## 94       280      14
      ## 96       288       7
      ## 99       910       0
      ## 101      710      15
      ## 102     1175       3
      ## 106      875      14
      ## 107      975      -3
      ## 109      975       5
      ## 110      925      11
      ## 111      575      10
      ## 112     1175       5
      ## 113     1030       6
      ## 116      413      20
      ## 117      675      20
      ## 118     1300      30
      ## 119      613      24
      ## 120      346      11
      ## 122      675      10
      ## 123      910       0
      ## 124      768      -3
      ## 125     1025      17
      ## 126      925      20
      ## 127     1075      13
      ## 128      993       0
      ## 129      750      28
      ## 131      925      52
      ## 133     1225       5
      ## 135      313       6
      ## 136       96      37
      ## 139      975      -5
      ## 140     1500      15
      ## 141     1225     -16
      ## 142      413      38
      ## 143     1500       8
      ## 144     1075       0
      ## 145      513      30
      ## 147      775       2
      ## 148     1225      13
      ## 149      413      27
      ## 151     1175      -2
      ## 155     1025      10
      ## 158      475      27
      ## 159      538      -2
      ## 160      825      17
      ## 161      588       8
      ## 162     2450       2
      ## 163     2450      36
      ## 164      875       2
      ## 165      413      16
      ## 166     1075       3
      ## 168      860       4
      ## 169      730       0
      ## 170     1025       0
      ## 171      825       2
      ## 172     1225      10
      ## 173      768      37
      ## 174      338       6
      ## 175     1225      12
      ## 176     1025       0
      ## 177     1225      -2
      ## 179      588      13
      ## 180      588       0
      ## 181      975       5
      ## 182     1225      -5
      ## 185     1225       0
      ## 186      975       5
      ## 187      463      20
      ## 188     1300       8
      ## 189     1025      12
      ## 190     1225       8
      ## 191      488      14
      ## 194      825      33
      ## 195     1300      -2
      ## 196     1175       6
      ## 197      825       0
      ## 199      975       0
      ## 200     1275       0
      ## 201      488      37
      ## 202     2200       5
      ## 203     1025       0
      ## 204      635       1
      ## 205      413       0
      ## 208     1025      -3
      ## 211      488      -2
      ## 212      413      23
      ## 213     1075       0
      ## 216     1025      18
      ## 218      825       7
      ## 219      131       3
      ## 220      725      11
      ## 221     1500       2
      ## 222     1025       0
      ## 225     2350      -5
      ## 226     1025       5
      ## 227     1075       1
      ## 228     1060       0
      ## 
      ## $weights
      ## $weights[[1]]
      ##   [1] 1 1 0 0 0 1 1 0 0 1 0 0 1 1 1 0 1 1 0 1 1 1 1 1 1 1 0 1 1 0 1 1 1
      ##  [34] 1 0 0 1 0 1 1 0 1 1 1 1 0 0 1 0 0 1 0 1 0 0 0 0 1 1 1 0 0 0 1 1 0
      ##  [67] 0 1 1 1 0 0 1 1 1 0 0 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 0 0 0 1 1
      ## [100] 1 1 1 1 1 1 1 0 1 0 1 1 0 1 0 1 0 1 0 1 1 1 1 1 1 1 1 1 0 1 0 1 1
      ## [133] 1 1 1 0 0 1 1 0 1 1 1 1 0 0 1 1 0 0 0 1 0 1 1 1 0 1 1 1 0 0
      ## 
      ## $weights[[2]]
      ##   [1] 1 1 0 0 1 0 0 0 1 1 0 1 0 1 1 1 1 0 1 1 0 0 0 1 0 0 1 1 1 1 1 1 0
      ##  [34] 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 0 1 0 0 1 1 0 0 1 1 0 0 1 1 1
      ##  [67] 1 0 0 1 1 1 1 1 1 0 1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 0 1 0 1 1 1
      ## [100] 1 1 0 1 1 1 0 1 1 1 1 1 0 1 0 1 1 0 1 1 0 1 1 0 1 0 1 0 0 1 1 0 0
      ## [133] 1 0 1 0 0 0 1 1 0 1 1 1 1 1 1 0 1 1 0 1 1 0 0 0 0 1 1 0 1 0
      ## 
      ## $weights[[3]]
      ##   [1] 1 1 1 0 0 0 0 0 1 0 1 1 1 1 1 0 1 0 0 1 0 0 1 1 1 1 0 1 1 0 1 0 1
      ##  [34] 1 1 0 1 1 1 1 0 1 1 1 1 0 0 0 1 1 0 1 1 0 1 1 1 0 0 1 0 1 0 0 0 0
      ##  [67] 0 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 0 0 1 0 0 0 1 1 1 1 0 0 1 1 1 1
      ## [100] 0 1 1 1 0 1 1 1 1 0 0 1 0 0 0 0 1 1 0 0 1 0 1 1 1 1 0 1 0 1 1 1 1
      ## [133] 1 1 0 1 0 1 1 0 0 1 1 1 0 1 0 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1
      ## 
      ## $weights[[4]]
      ##   [1] 1 1 1 1 1 1 0 1 0 0 1 0 1 0 1 0 1 1 0 1 1 0 1 0 1 0 0 0 1 1 0 1 0
      ##  [34] 1 1 0 0 1 0 1 1 1 0 1 1 1 1 1 1 0 1 1 0 0 1 1 0 1 1 1 1 1 0 1 1 1
      ##  [67] 0 1 1 0 1 1 1 1 0 0 1 1 1 0 0 1 0 1 0 1 0 1 1 1 1 0 1 0 1 0 1 1 1
      ## [100] 1 1 0 1 1 0 1 0 1 1 0 0 0 0 1 1 1 1 0 1 0 1 0 0 1 1 0 1 1 1 1 0 1
      ## [133] 1 1 1 0 1 1 0 0 0 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0 1 0 0 1 1
      ## 
      ## $weights[[5]]
      ##   [1] 0 1 1 1 1 1 1 0 1 1 1 0 1 0 1 0 0 1 0 0 1 1 0 0 1 1 1 0 0 1 1 1 0
      ##  [34] 0 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 0 0 0 1 1 0
      ##  [67] 0 0 0 1 0 1 1 0 1 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0 1 0 0 0 0 0 1 1
      ## [100] 1 1 0 0 1 1 1 0 1 1 1 0 1 1 1 1 1 1 0 1 1 0 0 0 1 1 0 1 1 1 1 1 0
      ## [133] 0 1 0 1 1 1 1 1 1 1 0 0 1 1 0 1 0 1 0 1 1 0 0 1 0 0 1 0 1 0
      ## 
      ## $weights[[6]]
      ##   [1] 1 1 1 0 0 0 1 1 1 0 0 1 1 1 0 0 1 0 1 1 0 0 0 0 1 1 1 1 0 1 1 0 1
      ##  [34] 1 1 1 0 0 1 1 1 1 0 1 0 0 1 1 1 1 0 1 0 1 1 1 0 0 0 1 1 1 1 1 1 1
      ##  [67] 1 1 1 0 1 1 1 0 0 0 0 1 1 1 1 1 0 1 1 1 1 0 1 0 1 1 1 1 1 0 1 1 0
      ## [100] 1 1 0 0 0 0 1 1 1 0 0 1 1 0 0 1 1 1 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1
      ## [133] 1 1 1 1 1 0 1 1 0 1 1 1 0 0 1 0 1 0 0 0 1 0 1 1 0 1 1 1 0 0
      ## 
      ## $weights[[7]]
      ##   [1] 0 0 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 0 1 0 1 1 1 1 0 0 1 0 1 0 1 0 1
      ##  [34] 0 0 0 1 0 1 1 1 1 1 0 0 1 1 1 0 1 0 0 0 1 1 0 0 1 1 1 1 1 0 0 1 1
      ##  [67] 1 0 1 1 1 0 0 1 0 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 0 1 0 1 0 1 1 1
      ## [100] 1 1 1 1 1 1 0 1 0 1 0 0 0 0 0 1 0 1 1 1 1 1 1 0 0 0 1 0 1 1 0 1 1
      ## [133] 0 1 1 1 1 1 0 1 1 0 1 1 0 0 1 1 0 0 1 1 0 1 1 0 1 0 1 1 1 1
      ## 
      ## $weights[[8]]
      ##   [1] 0 1 1 1 1 0 1 1 0 1 0 1 1 0 0 1 0 1 0 0 1 1 1 0 1 0 1 1 1 0 0 1 0
      ##  [34] 1 1 0 0 1 0 0 1 0 0 1 0 0 1 1 0 0 1 1 1 0 1 1 1 1 1 0 0 1 0 0 0 1
      ##  [67] 0 0 1 1 1 1 1 1 1 1 0 1 0 1 0 0 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1
      ## [100] 1 0 1 0 0 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0
      ## [133] 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 0 1 1 0 1 1 1 0 0 0 1 1 1 1
      ## 
      ## $weights[[9]]
      ##   [1] 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1
      ##  [34] 0 1 1 1 0 1 0 1 1 0 0 1 1 1 1 0 1 0 0 0 0 1 1 1 1 0 0 0 1 1 1 1 1
      ##  [67] 0 1 0 1 1 0 1 1 1 1 1 1 0 1 0 0 1 0 0 0 0 0 1 1 1 0 1 1 1 1 1 0 1
      ## [100] 1 0 0 1 1 0 0 1 1 0 0 1 0 1 1 1 1 0 0 1 1 0 1 1 0 1 0 0 1 1 0 0 1
      ## [133] 1 0 0 1 1 1 1 0 1 0 0 1 1 1 1 1 1 0 1 1 1 0 0 1 0 0 0 1 0 0
      ## 
      ## $weights[[10]]
      ##   [1] 1 0 0 1 0 0 1 1 1 1 1 0 1 1 0 1 1 1 0 1 0 0 1 0 0 1 1 0 1 1 0 1 1
      ##  [34] 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 0 0 0 0 1 1 1 0 1 0 1 1 1 1 1 1 1
      ##  [67] 0 1 0 0 1 0 0 1 1 1 0 1 0 1 1 1 1 1 1 0 1 0 1 1 1 0 0 0 1 1 1 1 0
      ## [100] 0 1 0 0 1 1 0 1 1 1 0 0 1 1 0 1 0 0 1 1 0 1 0 1 0 1 1 0 1 1 1 1 0
      ## [133] 0 1 1 0 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 0 0 0 0 1
      ## 
      ## $weights[[11]]
      ##   [1] 0 1 0 1 1 0 1 0 1 1 1 0 1 0 1 1 0 0 1 0 1 1 1 1 0 0 1 0 1 0 0 1 1
      ##  [34] 0 0 1 1 0 1 0 0 1 1 1 0 1 1 1 1 0 1 0 1 1 1 1 0 0 0 1 0 1 0 1 0 1
      ##  [67] 1 1 1 0 1 0 1 1 1 1 0 0 1 1 1 1 1 0 1 1 1 0 0 1 1 0 1 0 1 1 1 0 1
      ## [100] 0 0 0 0 1 1 0 0 0 1 0 0 1 1 1 1 1 1 1 0 0 1 0 1 0 1 1 0 1 1 0 1 1
      ## [133] 1 1 0 1 1 1 1 0 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0
      ## 
      ## $weights[[12]]
      ##   [1] 1 1 1 0 1 1 1 0 1 1 1 1 0 0 0 1 1 1 0 1 0 1 0 1 1 1 1 0 1 0 1 1 1
      ##  [34] 0 0 0 1 1 1 0 1 1 0 0 0 1 1 0 1 1 1 0 1 0 0 0 1 1 0 1 0 0 0 1 0 1
      ##  [67] 1 0 1 1 1 1 0 0 1 0 0 0 1 0 1 0 1 1 1 1 0 0 1 0 0 0 0 1 1 1 1 1 1
      ## [100] 1 1 1 1 1 1 0 0 1 1 0 0 1 1 1 1 0 0 1 1 1 1 1 0 0 1 1 1 1 0 0 1 1
      ## [133] 1 0 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1 1 0 0 1 1 1 1 1 0 1 1 0 1
      ## 
      ## $weights[[13]]
      ##   [1] 0 1 0 0 1 1 1 1 1 1 1 0 1 0 1 1 0 0 0 1 1 1 0 1 1 0 1 0 0 1 0 0 1
      ##  [34] 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 0 1 1 0 0 0 1 1 0 1 0 1 0 1
      ##  [67] 1 1 0 1 0 0 0 0 1 0 0 0 1 1 0 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 1 0 1
      ## [100] 0 0 1 0 1 0 0 1 1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1
      ## [133] 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 0 1 1 1 0 0 1 1 1 0 1 1
      ## 
      ## $weights[[14]]
      ##   [1] 0 1 1 1 1 1 0 1 1 0 1 0 0 1 1 0 0 1 1 0 1 0 0 1 1 1 0 0 1 1 0 0 0
      ##  [34] 1 1 0 0 1 0 0 1 0 0 1 1 1 1 0 0 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 1
      ##  [67] 1 1 1 1 0 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 1 0 1 1 1
      ## [100] 0 1 0 1 1 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0 0 1 1 0 0 1 1 0
      ## [133] 1 0 0 1 1 1 0 0 0 1 1 1 1 1 0 1 1 1 0 0 1 0 1 1 0 0 0 0 1 0
      ## 
      ## $weights[[15]]
      ##   [1] 0 1 0 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 1 0
      ##  [34] 1 1 0 0 1 0 0 1 1 0 1 0 1 0 0 0 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1
      ##  [67] 0 1 1 0 1 1 0 0 0 1 1 1 0 1 1 1 1 0 0 1 1 0 1 1 0 1 1 0 1 1 1 0 1
      ## [100] 0 0 1 1 1 1 0 0 1 1 0 1 1 0 0 0 0 0 0 0 1 0 1 0 1 1 1 0 1 0 1 1 0
      ## [133] 1 0 0 1 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 0 1
      ## 
      ## $weights[[16]]
      ##   [1] 1 1 1 1 1 0 1 1 1 0 1 1 0 1 1 1 0 1 1 0 1 1 0 1 1 0 0 1 1 1 1 1 0
      ##  [34] 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 0 1
      ##  [67] 0 1 1 0 1 0 1 0 1 1 1 0 1 0 0 1 0 1 1 0 1 1 1 1 1 0 0 0 1 0 0 1 1
      ## [100] 1 1 1 0 0 1 1 0 1 0 1 1 0 0 1 0 0 1 1 0 1 0 0 0 0 1 0 1 1 1 1 1 0
      ## [133] 0 0 1 0 1 1 1 1 0 0 1 0 1 1 1 1 1 0 0 1 1 1 0 1 0 0 0 1 1 0
      ## 
      ## $weights[[17]]
      ##   [1] 1 1 1 0 1 0 1 0 1 1 0 1 0 1 1 1 1 0 0 0 1 0 0 1 0 1 1 1 1 1 0 1 0
      ##  [34] 1 0 1 1 0 1 0 1 1 0 0 0 0 1 1 1 1 1 0 1 1 1 1 0 0 1 0 1 0 1 1 1 0
      ##  [67] 1 1 1 1 0 0 1 1 1 0 0 1 1 1 0 1 1 1 0 1 1 1 1 0 1 1 1 0 1 1 0 1 1
      ## [100] 1 0 1 0 1 1 1 1 1 1 1 0 1 1 1 0 0 0 0 1 0 0 1 0 0 0 1 0 1 1 1 0 0
      ## [133] 0 1 1 0 1 1 0 1 1 0 0 0 1 1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 1 0
      ## 
      ## $weights[[18]]
      ##   [1] 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 1 0 0 1 0 0 1 1 0 1 0 0 0 1 1 1
      ##  [34] 1 1 0 1 1 1 1 1 0 0 0 1 0 1 0 1 1 1 1 1 0 0 0 0 1 1 1 1 1 0 0 0 1
      ##  [67] 1 0 0 0 0 1 0 0 1 1 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 0 1 0 1 0 1 1 0
      ## [100] 1 1 0 1 1 1 1 1 0 0 1 0 0 1 1 1 1 1 1 1 0 1 1 0 1 0 0 1 1 1 0 0 1
      ## [133] 1 1 1 0 1 0 0 1 0 1 1 0 1 1 1 1 0 1 0 1 0 1 1 0 1 1 1 0 1 0
      ## 
      ## $weights[[19]]
      ##   [1] 1 0 0 1 0 0 0 0 0 1 0 1 1 1 0 1 1 0 1 1 0 1 0 1 0 1 1 1 0 1 1 0 1
      ##  [34] 0 1 1 1 1 0 0 0 1 0 1 1 1 1 1 1 0 1 1 0 0 1 0 1 0 0 1 1 1 1 0 0 1
      ##  [67] 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 0 1 0 1 1 0 1 1 1 0 1 0 1 1
      ## [100] 1 0 1 1 0 1 0 1 1 0 0 0 1 1 1 1 1 0 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1
      ## [133] 0 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 0 1 0 1 1 1 0 0 0 1 1 0 0
      ## 
      ## $weights[[20]]
      ##   [1] 1 1 0 1 1 0 0 0 0 1 0 0 0 1 0 1 1 1 1 0 1 0 1 0 0 0 0 1 1 0 1 1 0
      ##  [34] 0 0 0 0 0 1 1 1 0 1 0 0 0 0 0 0 0 1 1 1 1 0 1 1 0 1 1 0 1 0 1 0 1
      ##  [67] 0 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1
      ## [100] 1 1 1 1 0 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1
      ## [133] 0 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 0 1 1 0 1 0
      ## 
      ## $weights[[21]]
      ##   [1] 0 1 1 1 0 1 0 1 1 1 0 1 0 1 0 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 0 1 0
      ##  [34] 1 0 1 1 0 1 0 1 1 0 0 1 1 0 0 0 1 1 1 1 1 1 0 1 1 1 0 1 1 0 1 1 0
      ##  [67] 0 0 0 1 0 1 1 0 1 1 1 0 0 0 1 1 1 1 0 1 1 0 1 0 1 1 1 1 0 0 1 0 0
      ## [100] 0 1 1 0 1 0 1 0 0 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1
      ## [133] 0 1 0 1 1 0 1 0 0 0 1 0 0 0 1 1 0 1 0 1 1 1 1 0 1 0 1 1 0 1
      ## 
      ## $weights[[22]]
      ##   [1] 1 0 1 0 1 0 1 1 0 1 0 1 0 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 0
      ##  [34] 0 1 1 0 0 0 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 0 1
      ##  [67] 0 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 1 1 0 1 1 0 1 1 0 1 0
      ## [100] 1 0 1 1 1 1 1 0 0 1 1 1 1 1 0 1 0 1 0 0 0 1 1 1 1 0 0 1 0 1 0 0 1
      ## [133] 1 0 1 1 0 0 1 0 0 1 0 0 1 0 1 0 0 0 1 0 0 0 1 0 0 1 1 1 1 0
      ## 
      ## $weights[[23]]
      ##   [1] 1 0 0 1 0 1 1 1 0 1 0 1 0 1 0 0 0 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1
      ##  [34] 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 0 1 1 0 1 0 0 0 0 1 1 1 0 1 0 0 1
      ##  [67] 0 1 0 1 0 1 0 0 1 1 1 1 1 1 1 0 0 0 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1
      ## [100] 0 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 0 0 1 0 1 0 1 0 1 1 0 1 0 0 1 0 1
      ## [133] 0 1 1 1 1 0 1 0 0 1 1 1 1 1 1 1 1 1 0 0 0 1 0 0 1 1 1 1 0 1
      ## 
      ## $weights[[24]]
      ##   [1] 0 0 1 1 1 1 0 1 1 0 1 0 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0 1 1 1
      ##  [34] 1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 0 0 1 1 1 1 1 1 0 0 1
      ##  [67] 1 1 0 0 0 1 1 1 1 0 1 0 0 0 1 1 1 0 1 1 1 1 1 0 1 1 0 1 1 0 1 0 0
      ## [100] 1 1 1 0 0 0 1 1 0 1 1 0 0 1 1 1 1 1 1 0 0 0 1 1 0 1 0 0 1 0 0 1 0
      ## [133] 0 0 1 0 0 0 0 1 1 1 1 0 1 0 1 0 1 1 1 0 1 0 1 1 1 0 1 0 1 1
      ## 
      ## $weights[[25]]
      ##   [1] 1 1 0 1 0 1 0 0 0 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1 0 0 0 0 1 1 1 1 1
      ##  [34] 1 0 0 1 0 1 0 1 0 1 0 0 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 0 1 1 0 0
      ##  [67] 0 0 1 1 1 0 0 0 1 1 1 0 1 1 1 0 1 0 1 1 0 0 1 1 1 1 1 0 1 1 1 0 1
      ## [100] 0 1 1 0 1 0 0 0 1 1 1 0 1 1 1 1 1 0 0 1 1 0 1 0 0 1 1 1 0 1 1 0 1
      ## [133] 0 1 1 1 0 1 1 1 1 0 1 0 1 0 0 1 1 0 1 1 1 0 1 0 1 1 0 1 1 1
      ## 
      ## $weights[[26]]
      ##   [1] 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 0 1 1 0 1 1 1 1 0 0 1 0 1 1 1 0 0 1
      ##  [34] 0 1 1 0 1 1 1 0 0 1 1 1 1 1 0 0 0 0 1 1 0 0 1 1 1 1 0 1 0 0 1 0 1
      ##  [67] 1 1 1 1 0 1 1 1 1 0 1 1 1 1 1 0 1 0 0 1 1 0 1 1 0 1 0 1 0 1 1 0 1
      ## [100] 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 1 0 1 0 1 1 0 1 0 0 0 0 1 0 0 1
      ## [133] 0 1 1 1 0 1 0 1 1 1 1 0 0 1 0 1 0 1 1 1 0 0 1 0 1 0 1 1 0 1
      ## 
      ## $weights[[27]]
      ##   [1] 1 0 0 0 0 1 1 0 1 0 1 1 0 1 1 0 0 0 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1
      ##  [34] 0 0 0 1 1 1 1 0 0 0 1 1 1 1 1 1 0 0 0 1 1 0 0 0 1 0 0 1 0 1 1 1 1
      ##  [67] 0 1 1 1 1 0 0 1 1 1 1 0 1 1 0 1 1 1 0 1 0 1 0 1 1 0 0 0 1 0 1 1 0
      ## [100] 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 0 0 0 1 0 1 1 1 0 0 0
      ## [133] 1 1 1 0 0 1 1 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 0
      ## 
      ## $weights[[28]]
      ##   [1] 1 1 0 1 1 0 1 1 1 1 0 0 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 0 1 1 1 1 0
      ##  [34] 1 0 1 1 1 1 1 0 1 1 0 1 0 0 1 1 1 0 1 1 0 0 1 0 0 0 0 1 0 0 1 1 1
      ##  [67] 0 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1 1 1 0 0 1 0 1 1 0 1 0 1 0 0 1
      ## [100] 1 1 1 1 1 1 1 0 1 1 0 0 1 0 0 1 1 1 1 0 1 1 0 1 0 1 1 0 1 0 1 0 1
      ## [133] 1 1 0 1 1 0 1 1 0 1 0 0 0 1 1 0 0 1 1 1 0 1 0 1 1 0 1 1 0 1
      ## 
      ## $weights[[29]]
      ##   [1] 1 1 0 1 1 0 1 1 1 1 0 0 1 1 1 0 0 1 0 0 0 0 1 0 0 1 0 1 0 1 1 1 0
      ##  [34] 1 1 1 1 0 1 0 1 1 1 0 1 0 1 1 1 1 0 0 1 1 0 1 0 1 1 0 0 1 0 0 0 1
      ##  [67] 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 1 1 1 1 0 1 0 1 0 1 1 1 0 1 1 1 1 0
      ## [100] 0 1 1 0 1 1 0 1 1 1 1 0 0 1 1 0 1 1 0 1 1 0 1 1 1 1 1 0 0 1 0 1 1
      ## [133] 1 0 1 0 1 1 1 1 0 1 0 1 1 1 1 0 1 0 0 1 0 0 1 1 1 1 0 1 1 1
      ## 
      ## $weights[[30]]
      ##   [1] 0 0 1 0 0 0 1 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 0 0
      ##  [34] 1 1 1 1 1 0 0 1 0 0 1 1 1 0 0 0 1 1 1 1 0 1 0 0 1 0 1 1 1 1 0 1 0
      ##  [67] 1 1 1 1 1 1 1 0 1 0 0 1 0 1 0 0 1 1 1 1 1 0 0 1 0 1 1 1 0 0 1 1 1
      ## [100] 0 0 0 0 1 0 0 0 0 0 1 1 1 0 0 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 0 1 1
      ## [133] 1 0 1 0 1 0 1 0 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1 1 0 1 1 0 1 1
      ## 
      ## $weights[[31]]
      ##   [1] 1 0 0 1 0 1 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0
      ##  [34] 0 1 1 0 1 1 0 0 1 0 0 0 1 1 1 0 0 0 1 0 1 0 1 0 1 0 1 1 0 0 0 1 1
      ##  [67] 1 1 0 1 1 1 1 1 1 1 0 1 1 0 0 1 0 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0
      ## [100] 0 0 1 0 1 0 1 0 1 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 1 1 0 1 0 0 0 0
      ## [133] 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 1 0 1 1 1 1 0 0 0 1
      ## 
      ## $weights[[32]]
      ##   [1] 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 0 0 1 1 0 1 1 0 1 1 1 1 0 0 1
      ##  [34] 1 1 0 0 1 1 0 1 0 1 0 0 1 0 0 0 1 1 1 1 1 1 0 1 1 1 0 0 1 0 1 0 1
      ##  [67] 0 1 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 1 1 0 0 1 1 1 0 1 0 1 1 0 1 1 1
      ## [100] 1 0 0 1 1 0 1 1 1 1 1 1 0 1 1 0 1 1 0 1 0 1 1 0 1 0 0 0 1 1 0 0 1
      ## [133] 1 1 1 1 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0 1 1 0 0 1 0 1 1 1 0 1
      ## 
      ## $weights[[33]]
      ##   [1] 1 0 0 0 0 0 0 0 0 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1
      ##  [34] 0 1 0 1 0 1 1 1 0 0 1 1 1 0 0 1 0 1 1 1 0 1 1 0 1 1 1 0 1 0 1 1 1
      ##  [67] 1 0 1 1 1 1 1 1 0 1 0 1 1 0 0 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 0 1 1
      ## [100] 0 0 0 0 1 1 0 1 0 0 1 1 0 1 1 0 1 1 1 0 0 1 0 0 1 1 1 1 0 0 0 1 0
      ## [133] 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1 0 1 0 0 0 1 1 0 0 1 1
      ## 
      ## $weights[[34]]
      ##   [1] 1 0 1 1 1 1 1 1 1 0 1 1 0 1 1 1 0 1 1 0 1 0 0 1 1 0 0 0 1 1 0 1 1
      ##  [34] 1 1 1 1 0 0 1 1 1 0 0 1 1 1 1 0 1 0 0 1 1 0 0 1 1 0 1 0 0 1 1 1 1
      ##  [67] 0 1 1 1 1 1 1 1 0 0 1 0 1 0 1 1 0 0 0 1 1 1 0 0 0 1 1 1 1 1 1 1 1
      ## [100] 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 0 1 1 0 0 1 0 1 1 0 1 1 1 0
      ## [133] 0 0 0 0 1 0 1 0 0 1 0 0 0 1 1 1 1 0 1 0 1 1 1 0 1 1 1 0 1 1
      ## 
      ## $weights[[35]]
      ##   [1] 1 0 0 1 1 0 1 1 1 1 1 0 1 0 1 0 0 1 0 0 1 1 0 1 0 1 1 1 1 1 1 0 0
      ##  [34] 0 1 1 0 1 0 1 1 0 1 1 0 1 1 1 0 1 1 0 1 1 0 0 1 0 1 0 1 1 1 1 0 0
      ##  [67] 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 1 1 0 0 1 1 0 1 1 0 1
      ## [100] 1 1 1 1 1 1 0 1 0 0 0 0 0 1 0 1 0 1 0 1 0 0 1 1 1 0 0 1 1 1 1 0 1
      ## [133] 0 0 1 0 1 1 1 0 1 1 1 1 1 0 1 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1
      ## 
      ## $weights[[36]]
      ##   [1] 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 0 0 0 0
      ##  [34] 1 1 0 1 1 0 1 1 1 0 1 1 1 1 0 0 1 1 1 0 0 1 0 1 1 1 0 1 1 1 0 1 0
      ##  [67] 1 0 1 0 1 1 0 1 0 0 0 1 0 0 1 1 1 1 1 1 1 1 0 1 1 0 0 0 1 0 1 1 1
      ## [100] 0 0 1 1 1 1 0 0 0 0 0 1 0 0 0 1 1 0 1 1 1 1 1 0 0 0 1 1 1 1 0 1 1
      ## [133] 1 0 1 1 1 1 0 1 1 1 0 0 1 0 1 1 0 1 1 0 0 1 1 1 0 1 1 1 0 0
      ## 
      ## $weights[[37]]
      ##   [1] 1 0 1 0 0 0 1 1 0 1 0 0 0 0 1 0 1 1 1 1 0 1 0 1 1 0 0 1 0 1 1 1 1
      ##  [34] 0 1 1 0 1 0 1 1 0 1 0 1 0 1 0 1 1 1 0 0 1 1 0 0 0 0 1 0 1 1 0 0 0
      ##  [67] 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 0 1 0 0 0 1 1 1 1 1 0 0 0 1 1
      ## [100] 0 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 1 1 1
      ## [133] 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 0 1 1 1 0 1 1 1 1 0 0 1 1
      ## 
      ## $weights[[38]]
      ##   [1] 0 1 1 0 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 0 1 1 1 0 0 0 1 0 1 1 1 1 1
      ##  [34] 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 0 1 0 1 0 0 1 1 0 1 0 0 1 1 0 0 1 0
      ##  [67] 1 1 1 0 0 1 1 0 0 1 1 0 1 1 1 0 1 1 0 0 1 0 1 1 1 1 0 1 1 1 1 1 0
      ## [100] 1 0 1 0 0 1 1 0 1 1 0 1 1 1 1 1 0 1 1 0 1 1 1 1 0 0 0 1 1 0 0 1 0
      ## [133] 1 1 0 1 1 0 1 0 0 0 1 1 0 1 1 0 1 0 1 0 1 0 0 0 1 1 1 0 1 0
      ## 
      ## $weights[[39]]
      ##   [1] 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 1 1
      ##  [34] 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 1 0 0 1 1 0 1 1 0 0 1 0 0 0 1 0 1 1
      ##  [67] 1 1 0 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 0 0 1 0 0 1 1 0 1 1 1 0 1 1
      ## [100] 1 1 1 0 0 0 1 1 1 1 0 0 1 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1 0 0 1 1 0
      ## [133] 1 1 1 0 1 1 0 0 0 0 1 1 1 1 1 1 1 1 0 0 1 1 0 0 1 1 1 1 0 0
      ## 
      ## $weights[[40]]
      ##   [1] 1 0 0 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 0 0 1
      ##  [34] 1 0 1 1 0 1 1 1 0 1 1 1 0 1 1 1 0 1 0 0 1 1 0 1 0 0 1 0 0 1 1 1 0
      ##  [67] 0 0 0 0 1 1 1 0 1 0 1 0 1 1 0 0 1 1 0 0 0 1 1 0 0 0 1 1 0 1 0 1 1
      ## [100] 0 1 0 1 1 1 0 1 0 1 1 1 1 0 0 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 0 1 1
      ## [133] 1 1 0 1 0 1 0 0 0 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 1 0
      ## 
      ## $weights[[41]]
      ##   [1] 0 1 0 0 0 0 0 1 1 0 1 0 1 0 0 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0 0 1 1
      ##  [34] 0 1 0 1 0 1 1 1 1 1 1 1 0 1 1 0 0 0 0 1 0 1 1 0 1 0 1 1 0 1 1 1 1
      ##  [67] 0 1 0 1 0 0 0 1 1 0 1 1 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0 0 1 0 1 1 0
      ## [100] 1 1 1 0 1 1 0 1 0 0 0 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 0 0
      ## [133] 1 1 0 1 1 1 1 0 1 0 0 0 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 0 0 1
      ## 
      ## $weights[[42]]
      ##   [1] 0 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 1 1 1
      ##  [34] 0 0 1 1 0 0 1 0 0 0 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 0 0 1 0 1
      ##  [67] 1 0 1 0 1 1 1 1 1 1 1 1 1 0 0 1 1 0 0 1 0 0 0 1 0 1 1 1 0 0 0 0 0
      ## [100] 1 1 1 1 1 1 0 1 0 0 1 1 1 1 1 1 0 1 1 1 0 1 0 0 1 0 1 1 1 0 0 1 0
      ## [133] 1 0 0 0 1 1 1 1 1 1 0 0 1 1 0 0 0 0 1 1 1 1 1 1 0 0 0 1 1 1
      ## 
      ## $weights[[43]]
      ##   [1] 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 0 1 1 1 1 0 1 1 1 0 0 1 0 0 0 0 0
      ##  [34] 1 0 1 0 1 0 0 1 1 1 0 0 1 1 1 0 1 0 1 0 0 0 1 1 1 0 0 0 0 1 0 1 1
      ##  [67] 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 0 0 1 1 1 1
      ## [100] 0 0 1 0 1 1 1 0 0 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 1 1 1 1 1
      ## [133] 0 1 1 0 0 0 0 1 1 1 1 1 0 0 0 1 0 0 1 1 0 1 0 1 1 0 0 1 0 1
      ## 
      ## $weights[[44]]
      ##   [1] 1 1 0 1 1 1 1 1 0 0 1 1 0 0 1 0 1 1 0 1 1 0 1 1 1 0 1 0 0 0 0 1 0
      ##  [34] 1 0 0 1 0 1 1 0 1 0 1 1 1 1 0 0 0 1 0 1 1 0 0 0 1 1 1 0 1 0 1 0 1
      ##  [67] 1 0 1 1 1 1 1 0 0 0 1 0 1 0 0 1 1 1 1 1 1 0 1 1 0 0 1 0 1 0 1 1 1
      ## [100] 0 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 0 0 0 0 1 1 1 1 1 0 0 1 1 1 1
      ## [133] 1 1 1 1 0 1 1 0 1 0 0 0 1 1 1 1 0 1 1 1 1 1 1 0 1 0 0 1 1 1
      ## 
      ## $weights[[45]]
      ##   [1] 1 1 0 1 0 1 0 1 1 0 1 0 0 1 1 1 1 1 1 0 1 1 0 0 1 0 0 1 1 1 0 0 0
      ##  [34] 1 0 0 1 1 1 0 1 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 1 0 1 1 1 0 1 0 1 0
      ##  [67] 1 0 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 1 0 0 1 0 0 1 0 1 0 1 1 0 1 1
      ## [100] 0 1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 1 0 0 0 1 1 1 1 0 0 1 1 0 1 0 1 0
      ## [133] 0 1 0 1 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0 0 1 1 1 0 1 1 0 0
      ## 
      ## $weights[[46]]
      ##   [1] 1 1 0 0 0 1 1 1 0 1 1 1 1 0 1 0 1 0 0 1 0 1 0 1 1 0 1 0 0 0 1 1 0
      ##  [34] 1 1 1 1 1 1 0 0 1 0 1 0 0 1 1 1 0 1 1 1 0 1 1 1 1 0 1 1 1 1 1 0 1
      ##  [67] 1 1 1 0 0 0 1 1 1 0 0 1 0 0 0 1 1 1 1 1 0 1 1 1 0 0 0 1 0 0 0 1 0
      ## [100] 0 0 1 1 1 0 1 0 0 0 1 0 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1
      ## [133] 1 1 1 0 0 0 1 0 1 0 1 1 1 1 1 1 0 0 1 1 0 1 1 0 1 1 0 0 1 1
      ## 
      ## $weights[[47]]
      ##   [1] 0 0 0 0 1 0 1 0 1 1 1 1 1 0 1 1 1 1 0 1 0 0 0 0 1 1 0 1 1 1 1 1 1
      ##  [34] 1 1 1 0 0 0 0 1 0 0 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0 0 0
      ##  [67] 1 1 1 1 1 1 1 0 0 1 0 1 0 1 0 1 1 0 0 1 1 0 0 0 1 1 1 1 1 0 1 1 0
      ## [100] 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 0 0 1 1 1 0 0 1 1 1 1 0 0 1 0 0 0
      ## [133] 0 0 1 0 1 1 1 0 1 0 0 1 0 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 1 1
      ## 
      ## $weights[[48]]
      ##   [1] 0 1 0 1 1 0 1 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1
      ##  [34] 1 1 1 1 1 1 0 0 1 0 1 1 1 0 1 0 0 1 0 1 1 1 1 1 0 1 0 0 1 0 1 1 0
      ##  [67] 0 1 1 1 1 1 0 0 1 1 1 0 0 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0 0 0
      ## [100] 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 0 1 1 0 0 0 1 0 1 1
      ## [133] 1 1 0 1 0 1 1 0 1 1 0 0 1 1 0 1 1 0 0 1 1 0 0 1 0 0 1 0 1 0
      ## 
      ## $weights[[49]]
      ##   [1] 1 1 1 1 0 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1 1 1 0 0 0 0 0 1 0 1 0 1 1
      ##  [34] 1 0 1 1 0 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 0 1 1 0 1 1 1 0 0 0 1 0
      ##  [67] 0 1 0 1 1 1 1 0 0 1 0 0 0 1 1 0 0 0 1 1 1 0 0 0 1 0 1 1 1 1 0 0 0
      ## [100] 0 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 0 0 1 1 1 1 1 1
      ## [133] 1 0 1 1 0 1 0 0 1 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1
      ## 
      ## $weights[[50]]
      ##   [1] 1 0 1 1 0 0 1 1 1 1 0 1 1 1 1 0 1 1 0 0 0 1 0 0 1 0 0 0 0 1 1 1 0
      ##  [34] 0 1 1 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0 1 1 0 1 0 1 1 1 1 1 1 0 1
      ##  [67] 0 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1
      ## [100] 1 0 1 0 1 1 1 1 1 1 0 0 1 0 1 0 1 1 1 0 1 0 1 0 1 1 0 0 0 1 1 0 0
      ## [133] 1 0 1 0 1 1 0 1 1 0 1 1 0 0 1 0 1 1 0 1 1 0 0 1 0 1 0 0 1 1
      ## 
      ## $weights[[51]]
      ##   [1] 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 0 0 0 1 0 1 1 1 1 1 0 1 0 1 0 1
      ##  [34] 1 1 1 1 0 1 0 1 1 1 0 0 1 1 0 1 0 0 1 0 1 0 1 0 0 0 1 1 0 1 1 0 1
      ##  [67] 1 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1 0 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0
      ## [100] 0 1 1 0 0 1 1 1 1 1 0 0 0 1 1 0 1 1 0 0 1 1 0 0 1 1 1 0 1 0 1 0 1
      ## [133] 1 1 0 1 1 0 1 1 0 0 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 0 0 0 1 0
      ## 
      ## $weights[[52]]
      ##   [1] 1 0 1 0 1 1 1 1 0 1 0 1 1 0 1 1 1 1 0 0 1 0 0 1 0 1 1 1 1 0 0 1 0
      ##  [34] 1 0 0 1 1 0 0 0 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 1 0 0 1 1 1 1 1 0
      ##  [67] 1 1 1 1 1 0 1 0 0 1 0 0 0 1 1 1 0 1 1 1 0 1 1 0 0 1 0 1 1 0 0 0 1
      ## [100] 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 0 1 0 1 0 0
      ## [133] 1 1 1 1 0 1 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 0 0 1
      ## 
      ## $weights[[53]]
      ##   [1] 1 1 1 1 1 1 0 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 0 0 0 0
      ##  [34] 1 0 1 0 1 0 1 0 0 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 0 0 0
      ##  [67] 1 1 0 1 1 1 1 1 1 1 1 0 0 0 1 0 1 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0
      ## [100] 1 0 1 0 1 1 1 1 0 1 1 1 0 0 0 1 0 0 1 1 1 1 0 0 1 1 1 1 0 1 0 0 1
      ## [133] 1 0 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0
      ## 
      ## $weights[[54]]
      ##   [1] 0 1 1 1 0 0 0 1 1 1 0 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1
      ##  [34] 0 1 0 1 0 1 1 1 1 1 0 1 1 0 1 0 1 0 1 0 0 1 1 0 0 1 1 1 0 1 1 1 1
      ##  [67] 0 1 1 0 1 0 0 0 0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 1 1 1 0 0 0 0 0 1 0
      ## [100] 1 0 1 1 1 1 1 0 1 1 1 0 1 1 0 1 0 0 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1
      ## [133] 0 1 1 1 1 0 1 1 0 0 1 0 1 0 0 0 1 0 0 0 0 1 1 1 0 1 1 0 1 0
      ## 
      ## $weights[[55]]
      ##   [1] 1 1 1 0 1 0 1 1 1 0 0 1 1 1 1 0 0 1 0 0 1 0 1 1 1 1 1 1 0 1 1 1 1
      ##  [34] 1 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 0 0 1 1 1 1 1 1 0 0 0 1 1 0 1 0 1
      ##  [67] 0 1 1 1 1 0 0 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 1 1 1 1 0 0 1 1
      ## [100] 0 0 1 0 1 0 0 0 0 1 1 1 1 0 0 0 1 1 1 1 0 0 1 0 1 0 0 1 1 1 1 0 1
      ## [133] 1 0 0 0 1 1 0 1 1 0 0 1 0 1 1 1 0 0 1 1 0 1 1 1 1 1 1 1 1 1
      ## 
      ## $weights[[56]]
      ##   [1] 1 1 0 0 1 1 0 1 0 0 0 1 1 1 1 1 1 0 1 1 0 1 0 0 0 0 1 1 1 1 1 0 0
      ##  [34] 0 0 0 1 1 1 0 1 1 0 0 0 0 1 0 1 1 0 1 0 1 0 0 0 1 0 1 1 1 0 1 1 1
      ##  [67] 0 0 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 0 0 1 1 1 0 1 0 1 1 0 1 1 1 0
      ## [100] 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1 0 1 0 0 1 1 1 1 1 0 1 1 0 0 0 0 1 1
      ## [133] 1 1 1 0 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1 1 1 0 0 1 1 1 1 0 0 0
      ## 
      ## $weights[[57]]
      ##   [1] 0 1 1 0 1 0 1 0 1 1 0 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 1 1 1 0 0 0 1
      ##  [34] 0 0 1 0 0 1 0 1 0 0 1 0 1 1 1 0 1 0 0 0 0 1 1 0 0 1 1 1 1 1 0 1 1
      ##  [67] 1 1 1 1 1 1 1 0 1 1 1 0 0 0 1 1 0 1 1 1 1 0 1 0 0 1 0 1 1 1 1 0 1
      ## [100] 1 0 1 0 0 0 1 0 0 1 0 1 0 1 1 1 0 1 1 0 1 1 1 0 1 1 1 1 1 1 1 0 0
      ## [133] 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1
      ## 
      ## $weights[[58]]
      ##   [1] 1 1 1 0 1 1 1 0 1 0 1 1 1 0 0 1 0 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1
      ##  [34] 0 1 1 0 0 1 1 1 0 0 0 1 1 0 0 1 0 1 0 1 1 0 1 1 1 1 0 1 1 1 0 0 0
      ##  [67] 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 1 0 1 1 1 1 0 1 0 1 0 0 0 0 0 0 1 0
      ## [100] 1 1 1 1 1 1 1 0 0 1 1 1 0 1 0 1 0 0 1 0 1 0 1 1 1 0 1 0 1 1 1 1 0
      ## [133] 1 0 1 1 0 1 0 0 0 0 1 0 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 0 0 1
      ## 
      ## $weights[[59]]
      ##   [1] 1 1 0 1 0 1 0 1 0 0 0 1 1 0 1 1 1 1 1 1 0 1 0 1 1 0 0 1 1 0 0 1 0
      ##  [34] 0 1 1 0 1 0 1 1 0 1 1 1 1 1 1 1 0 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 0
      ##  [67] 1 1 0 0 1 0 0 1 1 1 1 1 1 0 1 0 0 1 1 1 0 1 1 0 1 1 1 1 0 0 0 0 1
      ## [100] 1 1 0 1 0 0 0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 0 0 1 1 1 0
      ## [133] 1 1 1 1 0 1 0 1 1 1 1 1 0 1 0 1 0 0 0 1 0 0 1 1 1 1 0 0 0 0
      ## 
      ## $weights[[60]]
      ##   [1] 1 1 0 1 0 1 0 0 0 1 1 1 1 0 0 1 0 1 1 1 0 0 1 0 1 0 1 0 1 1 1 1 1
      ##  [34] 1 1 1 0 1 1 1 1 1 1 0 0 0 1 1 1 0 1 1 1 1 1 0 1 1 0 1 1 1 1 1 0 1
      ##  [67] 0 1 0 1 1 0 0 1 0 1 1 0 1 0 1 1 1 0 1 1 1 1 1 1 1 0 0 1 0 0 1 0 1
      ## [100] 0 1 0 0 0 0 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 0 0 1
      ## [133] 1 1 0 1 1 1 1 1 0 1 0 1 0 1 0 1 0 0 0 0 1 1 1 1 1 1 0 0 0 0
      ## 
      ## $weights[[61]]
      ##   [1] 0 1 0 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 0 0 0 1 0 0 1 0 1 0 0 1 1 1
      ##  [34] 1 1 1 0 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 0 0 1 0 0 0 1 1 1 1 0
      ##  [67] 0 1 0 1 1 0 1 1 0 1 0 1 0 1 0 1 1 1 0 1 0 1 1 0 1 1 0 0 0 0 1 1 0
      ## [100] 0 1 0 1 0 1 1 1 0 1 0 0 1 0 1 1 1 0 1 0 1 1 1 1 1 0 1 1 0 1 1 0 0
      ## [133] 1 1 1 0 0 1 0 0 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1
      ## 
      ## $weights[[62]]
      ##   [1] 0 0 0 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 0 0 1 0 0
      ##  [34] 0 1 0 1 1 0 1 1 0 1 0 1 0 1 1 1 0 1 1 1 1 1 0 0 1 1 0 0 1 1 0 0 1
      ##  [67] 0 0 1 1 1 0 1 0 0 1 0 1 0 1 0 1 1 1 0 1 1 1 1 1 0 1 0 0 0 1 1 0 1
      ## [100] 1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 1 1 1 0 1 0 1 0 0 1 1 0 1 1 1 1 1 1
      ## [133] 1 1 1 1 0 0 0 1 1 1 0 1 1 1 1 1 1 0 0 1 0 1 1 0 1 0 1 0 0 0
      ## 
      ## $weights[[63]]
      ##   [1] 1 1 1 1 1 1 1 0 0 0 1 0 1 1 1 0 0 0 0 1 1 0 0 0 1 1 0 1 0 1 1 1 0
      ##  [34] 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 1 0
      ##  [67] 1 0 1 0 0 1 0 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 0 0 1 1 0 1 0 0 0 1
      ## [100] 1 0 0 1 1 0 0 1 1 0 0 1 1 1 0 0 1 0 1 0 1 1 0 1 1 1 0 0 0 1 1 1 1
      ## [133] 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 0 1 1 1 1 0 1 0 0 0 1 1 0 0
      ## 
      ## $weights[[64]]
      ##   [1] 1 1 0 1 1 1 1 1 0 1 0 1 0 0 1 1 1 0 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1
      ##  [34] 0 0 1 1 0 1 0 0 1 1 1 1 1 0 1 1 1 0 0 1 1 1 0 0 0 1 1 1 1 1 0 1 1
      ##  [67] 1 0 0 0 1 1 0 0 0 1 1 1 0 1 0 0 1 0 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1
      ## [100] 1 0 1 1 1 1 0 0 0 1 1 1 0 1 1 0 1 1 0 1 0 1 1 1 1 0 1 0 0 0 1 0 1
      ## [133] 0 1 0 1 0 0 1 1 1 1 0 0 0 1 1 0 0 1 1 1 1 1 1 0 1 1 1 0 1 0
      ## 
      ## $weights[[65]]
      ##   [1] 0 0 1 1 1 1 1 0 1 0 1 1 1 0 1 1 0 1 1 0 0 0 1 0 0 1 0 0 1 1 1 0 1
      ##  [34] 1 0 1 1 0 1 0 1 0 1 1 0 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 0 0 0 0 0 1
      ##  [67] 1 1 1 1 1 1 0 0 0 0 1 1 0 0 1 1 1 1 0 1 0 0 0 1 1 0 1 0 0 0 1 1 1
      ## [100] 0 1 0 0 1 1 0 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1 0 1 0 1 1 1 1
      ## [133] 1 1 1 0 0 1 1 1 1 1 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 1 1 1 0 1
      ## 
      ## $weights[[66]]
      ##   [1] 0 1 1 1 1 1 1 1 0 0 1 1 1 0 1 1 1 0 1 1 1 1 0 0 0 1 1 1 1 1 1 1 1
      ##  [34] 0 1 0 1 0 0 1 0 1 1 0 1 1 1 0 0 0 1 0 1 1 1 0 1 0 1 1 0 0 1 0 1 1
      ##  [67] 0 1 0 1 1 0 1 0 1 0 1 1 0 1 0 1 1 0 1 1 1 0 0 0 0 1 0 1 0 1 0 0 1
      ## [100] 1 1 0 1 1 1 1 1 0 1 0 0 0 1 0 0 0 0 1 1 1 1 1 1 0 0 1 0 1 1 0 1 1
      ## [133] 1 0 1 1 0 1 0 0 1 1 0 1 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1
      ## 
      ## $weights[[67]]
      ##   [1] 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1 1 1 1 1 0 1 1 1 1 0 0 0 0 0 0 1 0 0
      ##  [34] 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 0 0 1 0 1 0 0 1
      ##  [67] 1 1 1 0 0 0 1 1 1 1 1 1 0 1 0 1 1 0 1 1 1 1 1 1 0 1 1 1 0 1 1 0 0
      ## [100] 1 0 1 1 1 0 1 0 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 0 0 1 1 1 0 0 1 1
      ## [133] 1 1 0 0 0 1 0 1 0 0 1 0 1 0 1 1 0 0 1 1 0 0 0 1 1 0 1 1 0 1
      ## 
      ## $weights[[68]]
      ##   [1] 1 0 1 1 1 1 0 0 0 1 0 0 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 0 0 0 1 0 1
      ##  [34] 1 0 0 1 0 1 0 0 0 0 1 0 1 1 1 1 1 1 0 1 0 1 0 0 1 1 0 1 1 1 1 0 0
      ##  [67] 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 0 1 0 1 1 0 1 1 1 0 1 1 0 1 0 1
      ## [100] 1 1 0 0 0 1 0 1 1 0 1 1 1 1 1 1 0 0 1 1 1 1 1 0 0 1 1 1 0 1 1 0 1
      ## [133] 0 1 1 1 1 1 0 1 0 1 0 1 1 0 0 1 1 1 0 1 1 1 1 0 0 1 0 0 1 1
      ## 
      ## $weights[[69]]
      ##   [1] 0 1 0 1 0 1 1 0 0 1 1 0 0 1 1 0 0 0 0 1 1 0 1 1 0 1 1 0 1 0 0 0 1
      ##  [34] 0 0 0 1 1 1 0 1 1 0 1 0 1 1 0 0 1 1 1 0 1 1 0 0 0 1 1 1 1 1 1 1 1
      ##  [67] 0 0 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 0 0 0 1 1 1 1 1 0 1 1 0 1 1 1 1
      ## [100] 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 1 1 0 1 0 1 1 0 1 0 1
      ## [133] 1 0 1 0 1 1 0 0 1 1 0 0 0 1 0 1 1 0 1 0 1 1 1 1 0 0 1 1 1 1
      ## 
      ## $weights[[70]]
      ##   [1] 0 1 0 0 1 1 1 1 0 0 1 0 1 1 1 1 0 1 1 1 0 1 1 0 1 1 1 0 1 0 0 1 1
      ##  [34] 0 1 0 1 1 0 1 1 1 0 1 1 1 0 1 1 1 1 1 1 0 1 0 0 1 0 1 1 0 1 1 1 0
      ##  [67] 1 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 0 0 0 1 0 1 1 0 1 1 1 1 1 1 1 0 1
      ## [100] 1 1 0 1 1 1 1 1 1 1 1 0 0 0 1 0 1 0 1 0 1 1 1 1 0 0 1 0 1 0 0 1 0
      ## [133] 0 0 1 1 1 1 0 1 1 0 1 0 0 0 0 1 1 1 1 0 1 1 1 1 1 0 1 1 1 0
      ## 
      ## $weights[[71]]
      ##   [1] 1 0 1 1 1 1 0 1 0 1 1 1 1 0 0 0 0 0 1 0 1 0 0 1 1 0 1 1 1 0 1 0 1
      ##  [34] 0 0 0 1 1 1 1 0 0 1 1 1 0 1 0 0 1 1 1 0 1 0 1 1 0 0 0 0 0 1 0 1 0
      ##  [67] 1 1 1 0 1 1 0 1 1 1 0 1 0 0 1 1 0 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 0
      ## [100] 0 0 1 1 1 1 1 0 1 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 1 1 1 1
      ## [133] 0 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 0 1 0 1 1 1 0 1
      ## 
      ## $weights[[72]]
      ##   [1] 0 0 0 0 1 0 0 1 1 1 1 1 0 0 0 1 0 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1
      ##  [34] 0 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1
      ##  [67] 0 1 1 1 1 0 0 1 0 0 1 0 1 0 1 0 1 1 1 1 1 1 0 1 0 0 0 1 1 0 1 1 0
      ## [100] 0 1 1 0 0 0 1 0 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 0 0 1 1 0 0 1 0
      ## [133] 1 0 1 1 0 1 1 1 1 0 0 0 0 1 1 0 1 0 1 0 1 1 1 1 1 0 1 0 1 1
      ## 
      ## $weights[[73]]
      ##   [1] 1 0 1 1 0 1 1 1 0 1 0 0 0 1 1 1 1 1 1 1 0 1 1 0 1 0 1 0 1 1 1 1 1
      ##  [34] 1 0 0 1 0 1 1 0 1 0 0 0 0 0 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 0 1 1 1
      ##  [67] 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 0 0 1 0 0 0 1 0 0 1 1 0 0 1
      ## [100] 0 1 0 1 0 1 1 1 0 1 1 1 0 1 0 1 1 1 1 0 1 0 1 1 0 0 1 0 0 0 1 1 1
      ## [133] 1 1 1 1 1 0 1 0 0 0 1 0 0 1 1 0 1 1 1 1 1 0 0 1 0 1 0 1 0 1
      ## 
      ## $weights[[74]]
      ##   [1] 1 0 1 1 1 1 1 1 0 0 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 1 0 0 1 1 0 1 1
      ##  [34] 0 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1 1 0 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0
      ##  [67] 1 1 0 1 1 1 1 1 1 0 1 1 0 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 0
      ## [100] 1 0 0 1 0 0 1 1 1 1 0 1 0 1 0 1 0 0 1 1 1 1 1 0 1 0 0 0 0 1 1 1 0
      ## [133] 0 0 0 1 0 1 1 0 0 1 1 1 1 0 0 1 1 0 1 1 0 1 0 1 0 1 0 1 1 1
      ## 
      ## $weights[[75]]
      ##   [1] 1 1 1 1 0 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 0 1 0 1 1
      ##  [34] 1 1 0 1 0 1 1 0 0 1 1 0 1 1 1 0 0 0 0 1 1 1 1 1 0 0 0 1 0 1 1 0 0
      ##  [67] 1 1 0 1 0 1 1 0 0 0 0 1 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1
      ## [100] 1 0 1 0 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 0 0 1 1 0 0 1 1 0 1
      ## [133] 0 1 1 0 1 0 0 1 0 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 1 0
      ## 
      ## $weights[[76]]
      ##   [1] 0 1 0 1 1 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 0 1 1
      ##  [34] 0 1 1 1 1 1 0 1 0 1 1 0 1 1 1 1 1 0 0 1 1 0 1 1 1 0 1 1 1 1 1 1 0
      ##  [67] 0 1 0 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 0 0 1 1
      ## [100] 1 1 0 1 0 1 0 0 1 0 1 0 0 0 1 0 1 0 0 0 1 1 0 0 1 0 0 1 1 0 0 0 1
      ## [133] 0 0 0 1 1 1 0 1 1 1 0 1 0 1 1 1 0 1 1 1 0 1 1 0 1 0 0 1 0 1
      ## 
      ## $weights[[77]]
      ##   [1] 1 0 1 1 1 1 0 0 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 0 1
      ##  [34] 0 1 1 1 1 0 1 1 1 0 0 0 1 1 0 0 1 1 1 1 0 1 0 1 1 0 0 0 1 0 1 1 1
      ##  [67] 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 1 0 0 0 0 1 1 1 1 1 0 1 1 1 1 1 0
      ## [100] 1 1 1 1 0 1 1 0 0 0 0 1 0 0 1 0 1 1 1 1 1 1 1 1 0 0 1 0 1 1 0 1 1
      ## [133] 0 0 1 1 0 1 1 0 1 0 1 0 0 0 1 0 1 1 1 0 0 1 0 1 0 1 1 1 1 0
      ## 
      ## $weights[[78]]
      ##   [1] 0 0 1 1 0 0 1 1 0 1 1 1 0 0 0 1 0 1 0 1 0 1 1 1 1 0 1 1 0 0 0 1 0
      ##  [34] 1 0 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 1 1 1 0 1 1 0 0 1
      ##  [67] 0 0 1 1 1 1 0 0 1 1 1 0 1 1 0 1 0 1 1 1 0 1 1 1 1 1 1 1 0 0 0 0 0
      ## [100] 1 0 0 1 0 0 1 1 0 0 1 1 0 0 0 1 1 1 1 1 0 1 1 0 1 1 0 1 1 1 1 1 1
      ## [133] 1 0 1 1 1 1 0 0 0 1 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0
      ## 
      ## $weights[[79]]
      ##   [1] 0 0 0 1 1 1 1 1 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 1 0 1 1 0 1 1 1 0 0
      ##  [34] 1 1 1 1 1 1 0 1 1 0 0 1 0 1 1 0 1 1 0 1 1 1 0 0 1 1 1 1 0 0 1 1 0
      ##  [67] 1 1 1 0 1 1 0 1 1 0 0 0 1 1 0 0 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 1 0
      ## [100] 0 1 0 1 1 1 1 1 1 0 0 0 1 0 0 1 1 1 1 0 0 1 1 0 1 1 1 0 0 1 1 0 1
      ## [133] 1 1 1 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0 1 1 1 0 0 0 1 1 1 1 0 0
      ## 
      ## $weights[[80]]
      ##   [1] 1 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1 0 1 0 0 0 1 1 1 1 0 1 1 1 1 1 1 1
      ##  [34] 0 0 0 1 1 1 0 0 1 1 0 1 0 0 0 1 0 0 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1
      ##  [67] 1 1 0 1 1 1 1 0 1 0 0 0 0 1 1 0 0 1 1 1 1 1 0 1 1 0 1 1 1 1 0 0 0
      ## [100] 0 1 1 0 0 0 1 0 0 0 1 1 1 1 1 1 1 0 0 1 0 0 1 1 1 1 0 0 1 0 0 1 1
      ## [133] 1 1 1 0 0 1 1 0 0 0 1 1 1 1 1 0 1 1 1 0 1 0 1 1 1 1 0 1 1 0
      ## 
      ## $weights[[81]]
      ##   [1] 1 0 1 0 1 1 0 0 0 1 0 0 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0
      ##  [34] 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 0 1 0 0 1 0 1 0 0 1 1
      ##  [67] 0 0 0 1 0 0 1 1 1 1 1 1 1 1 0 0 1 0 1 1 1 1 0 0 0 1 0 1 1 1 1 0 1
      ## [100] 1 1 1 1 1 1 0 1 0 1 0 0 1 1 1 1 0 1 0 0 1 1 0 1 1 1 0 1 1 0 0 1 1
      ## [133] 1 1 0 1 0 0 1 1 0 0 1 1 1 0 1 1 1 0 0 1 0 0 1 0 0 0 1 1 1 1
      ## 
      ## $weights[[82]]
      ##   [1] 1 1 0 1 0 1 1 1 1 1 1 1 0 0 0 0 1 0 1 0 1 1 1 1 1 1 0 0 1 1 1 1 0
      ##  [34] 1 0 1 0 0 0 1 1 0 0 0 1 1 1 0 1 1 1 1 0 0 1 0 0 0 1 1 0 1 0 0 1 1
      ##  [67] 1 0 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 0 1 1 1 0 1 0 1 1 1 1 1 1 1
      ## [100] 1 0 0 1 0 1 0 1 0 1 1 1 0 0 1 1 0 0 1 0 1 0 1 1 1 0 1 0 0 0 1 0 1
      ## [133] 0 1 1 1 0 0 0 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 0 0 1 0
      ## 
      ## $weights[[83]]
      ##   [1] 1 1 0 0 0 1 1 1 1 0 1 0 1 0 1 1 0 1 0 1 0 1 0 0 1 1 0 1 0 1 0 0 0
      ##  [34] 1 0 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 1 0 0 1 1 0 1 0 0 1 1 0
      ##  [67] 1 1 1 0 0 1 1 1 1 0 0 0 1 0 1 1 0 0 1 1 1 1 0 1 0 0 1 1 0 0 0 0 0
      ## [100] 1 0 1 0 1 1 1 1 1 1 0 0 1 1 0 1 1 1 1 0 1 0 1 1 1 1 0 1 1 0 0 1 1
      ## [133] 1 0 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 0
      ## 
      ## $weights[[84]]
      ##   [1] 0 1 1 0 0 1 1 1 1 1 1 1 1 0 0 1 1 0 0 1 0 0 1 1 1 0 1 1 1 0 1 1 0
      ##  [34] 1 1 1 0 1 0 1 1 0 0 1 0 1 1 1 0 0 1 0 1 0 1 1 0 0 1 0 1 0 1 1 1 1
      ##  [67] 0 1 0 1 1 1 0 0 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 0 1 0 1 0 1
      ## [100] 1 0 1 1 1 1 0 1 1 0 0 1 0 0 0 1 0 0 1 1 1 1 1 1 1 1 0 1 1 1 0 1 0
      ## [133] 0 1 1 0 0 1 1 0 0 1 1 1 0 1 1 0 0 0 1 1 1 1 1 1 0 1 1 0 0 1
      ## 
      ## $weights[[85]]
      ##   [1] 0 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 0 1 1 0 0 1 1 0 0 0 1 0 1 0 0 1
      ##  [34] 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 0 1 1 0 0 0 0 1 1 0 1 0 0
      ##  [67] 1 1 1 0 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1 0 1 1 1 1
      ## [100] 0 0 1 1 1 0 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 0 1 0 0 1 1 1 1 1 0 0 1
      ## [133] 1 0 0 0 0 1 1 1 1 0 1 1 1 1 0 0 1 1 1 1 0 0 0 1 1 0 0 1 1 0
      ## 
      ## $weights[[86]]
      ##   [1] 1 1 0 0 0 0 1 1 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 0 1 1 0 1 1 1 1
      ##  [34] 1 0 1 1 1 1 1 1 1 1 0 1 1 1 0 0 0 1 0 1 0 1 0 1 0 1 1 1 1 1 0 1 1
      ##  [67] 1 0 0 1 0 0 1 1 1 1 1 0 1 1 0 0 1 0 1 1 1 1 0 0 1 1 0 1 1 0 0 0 0
      ## [100] 1 0 1 1 1 1 1 1 1 0 0 0 1 1 0 1 0 1 1 1 1 1 1 0 0 1 1 1 1 0 0 0 0
      ## [133] 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 0 1 1 1 0 1 1 1 1 0 1 0 0 0 1
      ## 
      ## $weights[[87]]
      ##   [1] 0 1 0 0 1 0 1 0 0 0 1 0 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1
      ##  [34] 1 1 0 1 1 1 0 0 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 0 0 1 1
      ##  [67] 1 0 1 0 0 1 0 0 1 0 1 1 0 1 1 0 1 0 1 1 0 0 1 0 1 1 0 0 0 1 0 1 0
      ## [100] 1 1 0 1 1 1 1 0 1 0 0 1 0 0 1 1 0 1 1 1 1 1 1 0 0 1 1 0 1 1 1 0 1
      ## [133] 1 1 0 0 1 1 1 1 1 1 0 1 1 0 0 1 0 1 0 1 1 1 1 1 1 0 1 0 1 0
      ## 
      ## $weights[[88]]
      ##   [1] 1 0 1 1 0 1 1 1 1 0 1 1 1 1 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0 0 0 1 1
      ##  [34] 1 0 0 1 0 1 1 1 0 1 0 1 1 1 1 1 1 0 0 0 1 0 0 0 1 1 0 1 0 1 0 1 1
      ##  [67] 0 0 1 1 0 1 0 1 1 1 0 0 0 1 0 0 1 1 0 1 1 0 1 1 1 0 1 1 0 0 1 0 1
      ## [100] 1 1 1 1 0 1 1 0 0 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 0 1 1 0 1 0 0 0 0
      ## [133] 0 0 1 1 0 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0
      ## 
      ## $weights[[89]]
      ##   [1] 0 0 1 1 0 1 0 0 1 0 1 0 1 0 0 0 0 0 1 1 1 1 0 0 1 1 1 1 0 1 0 0 1
      ##  [34] 0 1 1 0 1 1 0 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 0 1 0 1 1 1 0
      ##  [67] 1 0 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0
      ## [100] 1 0 0 1 1 1 1 0 1 0 1 1 1 0 0 0 0 0 0 1 1 1 0 0 1 1 1 0 1 1 1 1 0
      ## [133] 0 1 0 0 1 1 1 0 0 1 1 0 1 1 0 1 1 1 1 1 1 0 0 1 1 1 0 0 1 0
      ## 
      ## $weights[[90]]
      ##   [1] 0 1 1 1 1 0 1 0 1 1 1 1 0 1 0 1 0 1 1 1 1 1 0 0 1 1 0 0 1 1 1 1 1
      ##  [34] 0 1 1 1 1 1 0 0 1 1 0 0 1 0 1 1 0 0 0 1 0 1 1 0 0 0 1 1 0 0 0 0 1
      ##  [67] 1 1 1 0 0 0 1 0 1 0 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 1 1 1 0 1 1 0
      ## [100] 1 1 0 1 0 1 1 1 1 0 0 1 0 1 1 0 1 1 1 1 0 1 0 1 1 0 0 0 1 0 1 0 0
      ## [133] 1 1 0 1 1 0 1 1 0 1 1 1 1 1 0 1 0 1 0 1 1 1 0 1 1 1 0 1 0 1
      ## 
      ## $weights[[91]]
      ##   [1] 1 1 0 0 1 0 1 1 0 1 1 1 0 0 0 0 1 0 1 1 1 1 0 0 1 1 1 1 0 0 0 1 0
      ##  [34] 1 1 0 0 0 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 1
      ##  [67] 1 0 1 1 0 0 1 1 1 1 1 0 1 0 1 1 1 0 0 0 0 1 1 1 0 1 0 1 0 1 1 0 0
      ## [100] 1 0 0 1 0 1 1 1 0 1 1 1 0 0 0 0 1 1 1 0 1 1 0 1 0 1 0 1 0 1 1 1 1
      ## [133] 1 1 1 1 0 1 1 1 1 1 0 0 1 0 0 0 1 1 1 1 0 1 0 0 1 0 1 1 1 1
      ## 
      ## $weights[[92]]
      ##   [1] 1 0 1 1 1 0 1 1 1 1 1 1 0 0 0 1 0 1 1 1 0 1 1 0 1 1 0 1 1 0 1 0 1
      ##  [34] 1 1 1 1 1 0 1 0 1 0 1 0 0 0 1 1 0 0 0 0 1 1 0 0 1 1 0 1 0 1 0 0 0
      ##  [67] 0 0 0 1 1 0 0 1 1 0 1 1 1 1 1 0 0 0 1 1 1 0 0 1 1 1 0 1 0 0 1 0 0
      ## [100] 1 0 0 0 0 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 0 0
      ## [133] 1 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0
      ## 
      ## $weights[[93]]
      ##   [1] 1 0 1 0 0 1 1 1 1 1 1 0 1 1 1 0 1 1 0 1 0 1 1 1 1 1 0 1 0 0 1 1 1
      ##  [34] 1 1 0 1 1 0 0 1 1 1 0 0 1 0 1 0 1 1 1 1 1 1 1 0 0 1 1 0 1 0 1 0 1
      ##  [67] 1 0 1 0 0 0 0 1 1 0 0 1 1 1 0 1 1 0 1 0 1 1 0 1 1 0 0 1 1 0 0 1 0
      ## [100] 0 0 0 1 1 1 1 0 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 1 1 0 1 1 1 0 1 1 1
      ## [133] 0 1 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1 1 1 1 0 1 1 0 1 1 1 1 0
      ## 
      ## $weights[[94]]
      ##   [1] 1 1 1 1 1 0 1 0 0 1 1 1 1 1 1 1 0 0 0 1 1 0 1 0 1 1 0 1 1 1 0 1 0
      ##  [34] 1 0 0 1 0 0 1 0 1 0 1 1 1 0 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 1 1 0
      ##  [67] 0 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 0 0 1 0 1 0 1 1 1 1 1 1 1 0 0 1 1
      ## [100] 1 0 1 1 1 1 1 0 1 0 1 1 1 0 1 1 0 1 1 0 1 0 1 1 1 1 1 0 0 0 1 1 1
      ## [133] 1 1 0 0 0 0 0 0 1 0 1 1 0 1 1 0 0 0 1 0 0 0 1 1 0 0 1 0 0 0
      ## 
      ## $weights[[95]]
      ##   [1] 1 0 0 1 0 0 1 0 1 1 1 1 0 1 1 0 1 1 1 1 0 0 1 0 0 1 1 1 1 0 1 1 0
      ##  [34] 0 0 1 1 1 1 0 0 1 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 0 1 0 1 0 1 1 1 1
      ##  [67] 1 1 0 0 1 1 1 1 0 0 0 1 1 0 1 1 1 0 0 1 1 0 1 1 0 1 0 0 1 1 1 0 1
      ## [100] 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1 1
      ## [133] 0 1 1 1 1 1 1 0 1 0 0 1 1 0 1 1 1 1 1 1 1 1 0 0 1 1 0 0 1 1
      ## 
      ## $weights[[96]]
      ##   [1] 1 1 0 1 1 1 1 1 0 1 1 1 1 0 0 1 0 1 1 0 1 1 1 0 1 1 1 0 1 0 1 1 1
      ##  [34] 0 0 1 0 0 1 1 1 1 0 1 1 0 1 0 0 0 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 1
      ##  [67] 1 1 0 1 0 1 1 0 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 0 0 0 0 0 1 1
      ## [100] 1 1 0 1 0 0 1 0 0 0 1 0 0 0 1 1 0 1 1 0 1 1 1 0 0 0 1 1 1 1 0 1 0
      ## [133] 1 1 1 0 1 1 1 1 1 0 0 1 1 1 1 1 0 1 0 1 1 0 1 1 0 1 1 0 1 1
      ## 
      ## $weights[[97]]
      ##   [1] 1 1 1 0 0 0 0 1 1 0 1 0 0 0 1 1 1 1 1 1 1 0 1 1 0 1 1 1 0 0 1 0 0
      ##  [34] 1 0 1 1 1 0 0 1 1 0 1 1 1 1 0 0 0 1 1 1 0 1 1 1 1 0 0 1 0 1 1 0 1
      ##  [67] 1 0 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 0 0 1 0 1 1 1 1 1 0 0
      ## [100] 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 0 1 1 0 1 0 1 1 1 0 1 1 0 1 1 1 0 1
      ## [133] 1 1 1 0 0 1 0 1 1 0 1 0 1 1 0 0 0 0 1 0 1 0 1 1 1 1 1 0 1 1
      ## 
      ## $weights[[98]]
      ##   [1] 0 1 1 1 1 1 0 0 1 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1
      ##  [34] 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 0 1 0 1 0 1 0 0 1 0 1 1 1 1 0 1 0 1
      ##  [67] 1 0 0 0 1 1 1 1 1 0 1 0 0 0 1 0 0 1 1 1 0 0 1 1 0 0 1 0 1 0 1 1 0
      ## [100] 0 0 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 1 0 1 1 0 1 1 0 1 1 1 1 1 1 0 0
      ## [133] 0 0 0 1 0 1 1 1 0 0 1 1 1 0 1 0 1 1 1 1 1 1 0 0 0 1 0 0 1 0
      ## 
      ## $weights[[99]]
      ##   [1] 1 1 1 1 0 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0
      ##  [34] 1 0 0 0 1 0 1 1 0 0 1 0 1 1 1 1 0 1 0 0 0 1 1 1 1 1 1 1 0 0 0 1 1
      ##  [67] 0 0 0 1 1 1 1 1 1 0 1 0 0 1 1 1 0 1 1 1 0 1 1 0 0 1 0 0 1 0 0 0 0
      ## [100] 1 1 1 0 1 1 1 0 0 1 0 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 0 0 1 1
      ## [133] 0 1 1 0 1 0 1 1 1 1 0 0 0 1 0 1 1 0 0 0 1 0 1 1 1 1 1 0 1 1
      ## 
      ## $weights[[100]]
      ##   [1] 0 1 1 0 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 0 0 1 1 1 1 1 1 0 1 1 1 0 0
      ##  [34] 0 1 0 0 1 1 0 1 0 1 0 1 1 1 1 1 0 0 1 1 0 1 1 0 1 0 1 1 0 1 1 0 1
      ##  [67] 1 1 1 0 1 1 0 1 1 0 0 1 0 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1
      ## [100] 0 1 1 1 0 0 0 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 0 1 1 1
      ## [133] 0 1 0 0 0 0 0 0 1 0 1 0 0 1 0 1 1 0 0 1 1 1 0 0 0 0 1 1 1 0
      ## 
      ## $weights[[101]]
      ##   [1] 1 1 1 1 0 1 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 1 0 1 1 1 0 0 0
      ##  [34] 1 1 1 0 0 0 1 0 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 0 0 0 1 0
      ##  [67] 1 1 1 1 1 0 0 0 1 1 1 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1
      ## [100] 1 0 1 0 1 1 0 1 1 1 0 1 1 0 1 0 1 0 0 1 1 1 0 0 1 0 1 0 1 0 1 0 0
      ## [133] 0 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1 0 1 1 1 0 0 0 0
      ## 
      ## $weights[[102]]
      ##   [1] 0 1 1 1 0 1 1 0 1 1 0 1 0 1 1 1 1 0 1 1 0 1 1 0 0 0 0 1 1 1 1 1 0
      ##  [34] 1 0 0 0 0 1 1 1 0 1 0 1 1 0 0 1 1 1 0 0 0 1 1 1 1 1 1 0 1 1 0 0 1
      ##  [67] 0 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1 1 0 1 1
      ## [100] 1 1 1 1 1 0 1 0 1 1 0 1 0 1 0 0 1 0 0 1 0 0 1 1 1 1 0 0 1 1 0 1 0
      ## [133] 0 1 1 1 1 1 1 1 1 0 0 1 0 0 1 1 1 1 0 1 0 1 1 1 0 0 0 1 0 1
      ## 
      ## $weights[[103]]
      ##   [1] 1 1 0 0 0 1 1 0 1 1 0 1 1 1 1 1 1 1 0 0 1 0 0 0 0 1 1 0 1 1 0 1 1
      ##  [34] 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 1 0 1 0 1 1 1 0 1 1 1 1 0 0 1 0 0
      ##  [67] 1 1 0 1 1 1 0 1 1 0 0 1 0 0 0 0 0 1 1 0 0 0 1 1 1 0 1 1 0 0 0 1 1
      ## [100] 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 1
      ## [133] 1 1 1 1 0 0 1 1 0 0 0 0 0 0 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 0
      ## 
      ## $weights[[104]]
      ##   [1] 1 0 0 1 1 1 0 1 0 0 0 1 0 1 0 1 1 0 1 0 1 1 0 0 0 1 0 0 1 0 1 1 1
      ##  [34] 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 0 1 1 1 0 1 1 1 0
      ##  [67] 1 1 0 1 0 1 0 1 0 0 1 1 1 0 1 0 0 1 1 0 1 1 1 1 1 0 1 1 0 1 1 1 0
      ## [100] 0 1 1 1 1 1 1 0 0 1 0 0 1 1 1 0 1 0 0 1 0 0 1 1 1 0 1 1 0 1 1 1 1
      ## [133] 1 0 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 0 0 0 0 1 1 1
      ## 
      ## $weights[[105]]
      ##   [1] 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 0 1 0 0 1 0 1 1 0 1
      ##  [34] 1 0 0 1 1 0 1 1 1 1 1 0 1 1 1 0 0 0 1 0 1 1 1 1 1 1 0 0 1 0 1 0 1
      ##  [67] 1 1 1 1 0 1 0 0 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 0 1 1
      ## [100] 0 0 0 1 0 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 0 0 0 1 0 0 0 0 0 1 1 1 0
      ## [133] 1 1 0 1 1 1 0 0 1 0 1 1 1 0 1 0 1 1 0 1 0 1 1 1 0 1 0 0 1 0
      ## 
      ## $weights[[106]]
      ##   [1] 1 0 0 1 1 0 1 1 1 1 1 1 0 0 0 1 1 0 0 1 1 1 0 1 0 0 1 1 0 1 1 1 0
      ##  [34] 1 1 0 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 0 0 1 1 1
      ##  [67] 1 0 0 1 1 1 1 1 1 0 0 0 1 0 1 0 1 0 1 1 0 0 0 0 1 1 1 1 0 0 1 1 1
      ## [100] 1 1 0 1 1 0 1 1 1 1 1 0 0 1 1 1 0 1 0 0 1 1 1 1 1 1 1 1 0 1 0 1 1
      ## [133] 1 0 1 0 0 0 1 1 0 1 0 0 0 1 0 0 1 1 1 1 0 0 1 0 1 0 0 1 0 1
      ## 
      ## $weights[[107]]
      ##   [1] 1 1 0 0 0 0 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 0 0 1 1 1 1 0 1 0 0 1
      ##  [34] 0 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 0 0 0 0 0 0 0
      ##  [67] 0 1 1 0 1 0 0 0 1 0 1 0 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 0 1 1 1 1
      ## [100] 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 0 1 0 0 1 1 0 1 1 0 0 1 1 1 1
      ## [133] 0 1 1 0 1 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 0 0 1 1 1 0 1 1 1 0
      ## 
      ## $weights[[108]]
      ##   [1] 0 0 0 0 0 0 1 1 1 1 0 1 1 0 0 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0 0 0 1
      ##  [34] 1 0 1 1 1 0 1 1 1 0 1 0 1 0 1 0 1 1 1 1 0 1 1 0 0 1 0 1 1 1 1 1 1
      ##  [67] 1 0 1 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 1 0 1 1 0 1 1 1 0
      ## [100] 1 0 1 1 1 1 0 0 0 0 1 1 0 0 0 0 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 0
      ## [133] 0 1 1 1 0 1 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 0 0 1
      ## 
      ## $weights[[109]]
      ##   [1] 1 0 0 0 1 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 0 0 1 0 1 1 1 1 1 1 0 0 1
      ##  [34] 1 0 1 1 1 0 1 0 1 0 0 1 1 1 1 1 1 0 0 0 1 1 0 0 1 1 1 1 1 1 1 1 1
      ##  [67] 1 1 0 1 1 0 0 0 1 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 0 1 0 1 1 0 1 1 0
      ## [100] 0 0 0 1 1 1 0 1 1 1 0 0 0 1 1 1 0 1 1 0 1 0 1 1 1 0 1 1 1 1 0 1 1
      ## [133] 0 1 1 0 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 0 1 1 0 1 0 1 0 1 1
      ## 
      ## $weights[[110]]
      ##   [1] 1 1 1 1 0 1 1 1 1 0 1 1 0 1 0 0 0 0 1 0 1 0 1 1 1 0 0 1 1 0 0 0 1
      ##  [34] 1 1 1 1 1 1 1 0 0 0 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 0 1 0 1
      ##  [67] 0 1 0 0 0 1 1 0 1 0 1 1 1 1 1 1 0 0 1 1 0 0 0 0 0 1 1 0 1 1 1 0 0
      ## [100] 1 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 0 1 0 0 0 1 0 1 1 0 1 0 1 1 0
      ## [133] 0 1 0 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 0 1 0 1
      ## 
      ## $weights[[111]]
      ##   [1] 1 1 1 1 1 0 1 0 0 1 1 1 0 1 0 0 1 1 1 0 1 1 1 1 0 1 1 1 0 0 1 1 1
      ##  [34] 0 1 1 1 1 0 0 1 0 1 1 1 0 0 1 1 1 0 1 1 0 1 0 1 0 0 1 1 1 0 0 1 1
      ##  [67] 0 1 0 1 1 1 0 1 0 1 1 0 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0 0 0 0 1 1 0
      ## [100] 0 0 0 1 1 0 0 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1
      ## [133] 1 0 0 0 0 1 1 1 0 1 1 0 0 0 1 1 1 0 1 1 1 0 1 1 1 1 0 1 1 0
      ## 
      ## $weights[[112]]
      ##   [1] 1 1 0 1 1 0 0 0 0 0 1 0 1 1 1 1 1 1 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1
      ##  [34] 1 1 1 1 1 1 0 1 0 0 0 1 0 1 1 0 1 1 1 1 0 1 1 1 1 0 1 0 0 1 1 1 1
      ##  [67] 1 1 0 0 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 0 0 1
      ## [100] 1 1 1 1 0 1 1 0 1 0 1 1 0 0 1 1 0 0 1 1 0 1 1 1 0 0 1 1 1 1 0 1 1
      ## [133] 1 1 1 1 0 0 1 1 0 1 1 0 1 1 1 0 0 0 1 1 0 0 1 1 0 0 0 1 1 0
      ## 
      ## $weights[[113]]
      ##   [1] 1 0 1 1 1 0 0 1 1 0 1 1 1 0 1 1 1 1 0 0 1 0 0 1 1 1 1 1 1 1 1 1 0
      ##  [34] 1 1 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 1 0 1 0 1 1 0 1 0 1 0 0 1 1 1
      ##  [67] 1 1 0 0 0 1 0 0 1 1 0 1 0 1 0 1 1 0 1 0 1 0 1 0 0 1 1 1 1 1 1 1 1
      ## [100] 1 1 1 1 0 1 1 1 0 0 1 0 0 0 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 0 1 0
      ## [133] 1 1 1 0 0 1 1 0 1 0 0 0 0 0 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 0
      ## 
      ## $weights[[114]]
      ##   [1] 1 1 1 1 1 0 0 1 1 0 1 0 1 1 1 1 0 1 1 1 0 1 1 1 0 1 0 0 1 0 1 0 0
      ##  [34] 1 1 1 0 0 1 0 1 0 1 1 1 0 1 0 0 0 1 1 1 1 1 0 0 1 1 1 1 0 0 0 0 1
      ##  [67] 0 0 0 0 0 1 0 1 1 1 0 0 1 0 0 1 1 1 0 1 1 1 1 0 1 0 0 1 0 1 1 1 1
      ## [100] 0 1 1 0 1 1 0 1 1 0 1 1 0 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0
      ## [133] 1 0 0 1 0 1 1 1 0 1 0 1 1 1 0 1 0 1 0 0 1 1 0 1 0 1 1 1 1 1
      ## 
      ## $weights[[115]]
      ##   [1] 1 1 1 1 1 1 1 0 0 0 0 1 0 1 1 0 0 1 0 0 0 0 1 0 0 1 1 1 1 1 0 1 1
      ##  [34] 1 0 1 0 0 1 1 1 1 1 1 0 1 0 0 1 0 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1
      ##  [67] 0 1 1 1 0 0 1 1 0 1 1 1 0 1 1 1 1 1 0 0 1 1 0 0 0 0 1 1 1 1 0 1 1
      ## [100] 1 0 1 1 1 1 1 0 1 1 0 1 1 1 1 0 0 0 0 0 1 1 1 0 1 0 1 1 0 1 1 1 0
      ## [133] 1 1 1 1 0 1 1 1 0 1 1 1 0 1 1 0 0 0 1 0 1 1 1 0 0 0 0 0 0 1
      ## 
      ## $weights[[116]]
      ##   [1] 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 0 0 1 0 1 1 1
      ##  [34] 1 1 0 1 1 0 1 0 1 0 0 1 0 0 1 1 1 1 0 0 1 1 1 1 0 0 0 1 1 1 1 0 0
      ##  [67] 1 1 1 0 1 1 0 0 1 0 1 0 0 1 1 1 1 0 0 1 0 1 1 1 1 1 1 1 1 0 1 0 0
      ## [100] 1 1 0 0 0 1 0 1 0 0 1 1 1 1 1 0 1 0 1 0 1 1 1 1 0 0 1 1 1 1 1 0 1
      ## [133] 1 1 0 1 0 1 0 1 0 0 1 1 0 0 0 1 1 0 1 1 0 0 0 1 1 1 1 1 0 1
      ## 
      ## $weights[[117]]
      ##   [1] 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 0 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1
      ##  [34] 1 1 0 0 1 1 0 0 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      ##  [67] 1 1 0 1 0 0 0 0 1 0 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 0 0 0 1 1 0 1 0
      ## [100] 1 0 1 0 1 1 0 0 0 0 1 1 1 1 0 0 0 1 0 1 0 1 0 1 0 1 1 1 1 0 1 1 1
      ## [133] 1 0 0 1 0 1 1 0 0 1 1 1 0 0 0 0 1 1 1 1 0 1 1 1 0 1 0 0 0 1
      ## 
      ## $weights[[118]]
      ##   [1] 0 0 1 1 1 1 0 1 0 1 1 0 1 1 1 0 0 0 1 1 0 0 1 0 0 0 1 1 1 1 1 0 1
      ##  [34] 0 0 1 0 1 1 0 1 1 1 0 1 1 0 0 1 0 1 1 1 1 0 1 1 1 1 0 0 1 1 0 0 0
      ##  [67] 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 0 0 0 1 0 0 0 1 1 1 0 1 1 1 0 1 1
      ## [100] 0 0 1 0 0 0 0 0 1 1 1 1 0 1 0 0 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1
      ## [133] 1 0 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1
      ## 
      ## $weights[[119]]
      ##   [1] 1 0 0 1 0 1 0 0 0 1 1 0 0 0 1 0 0 1 0 1 1 1 1 0 1 0 1 0 1 1 1 1 1
      ##  [34] 1 0 1 0 0 1 0 1 1 1 0 1 1 0 0 1 0 1 1 1 1 1 0 0 0 1 0 0 0 1 1 1 1
      ##  [67] 1 1 1 1 1 0 1 1 0 0 1 0 1 1 0 0 1 0 1 0 1 1 0 1 1 0 1 0 1 1 1 1 1
      ## [100] 1 1 1 0 1 0 1 0 1 1 1 0 1 0 1 1 1 1 0 1 1 1 1 0 1 0 1 1 0 1 1 1 1
      ## [133] 1 0 0 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 1 0 1 0 0 1 1 0 1 0 1
      ## 
      ## $weights[[120]]
      ##   [1] 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 0 0 0 1 1 1 1 0 1 0 1 0 0 1 1 1
      ##  [34] 1 1 1 0 1 1 1 0 1 0 1 0 0 1 0 0 0 0 1 0 0 1 1 1 1 0 0 0 0 1 1 1 0
      ##  [67] 0 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 0 1 0 1 0 0 0 1 0
      ## [100] 1 1 0 1 1 1 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 0 0 1 1 0 0 1 0 0 1
      ## [133] 0 1 0 1 1 1 1 1 1 0 0 1 1 1 1 0 1 0 0 1 1 0 1 1 1 1 1 1 0 1
      ## 
      ## $weights[[121]]
      ##   [1] 0 0 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0
      ##  [34] 0 0 1 1 1 1 0 1 1 1 0 1 1 0 0 1 0 1 1 0 0 1 0 0 0 0 1 0 1 1 0 0 1
      ##  [67] 0 1 0 1 0 0 1 0 1 1 1 0 0 1 0 1 1 0 1 1 1 0 0 1 0 1 1 0 1 1 1 0 0
      ## [100] 1 0 1 0 1 1 0 1 0 1 1 1 0 0 0 1 1 0 1 1 1 0 1 1 0 0 1 1 1 1 1 0 1
      ## [133] 1 0 1 0 1 1 1 1 1 1 0 1 0 1 0 0 0 0 0 1 1 1 1 1 0 1 1 1 1 1
      ## 
      ## $weights[[122]]
      ##   [1] 1 1 0 0 1 0 0 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 0 1 0 0 0 1 0 0
      ##  [34] 1 1 0 1 0 1 1 1 0 0 0 0 1 0 1 0 0 1 1 1 1 0 1 1 0 1 1 0 0 1 1 1 0
      ##  [67] 1 1 0 1 0 0 1 1 0 0 1 1 1 0 1 0 1 1 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0
      ## [100] 1 1 0 1 0 0 1 0 0 1 0 1 0 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0
      ## [133] 0 1 0 0 1 1 1 1 1 0 0 1 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 0 1 1
      ## 
      ## $weights[[123]]
      ##   [1] 1 0 0 1 0 1 1 0 1 0 1 1 1 1 0 0 0 0 1 0 0 1 1 1 0 0 0 1 0 1 0 1 1
      ##  [34] 1 1 0 1 0 1 1 1 1 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 0 0 0 1 1 0 1 1 0
      ##  [67] 1 0 1 1 0 0 0 1 0 0 0 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 0
      ## [100] 1 0 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 0 1 0 1 1 0 0 1 0 0 0 1 0
      ## [133] 1 0 1 1 1 0 1 1 1 1 0 1 0 0 1 1 1 0 1 1 1 0 1 0 1 1 1 1 1 1
      ## 
      ## $weights[[124]]
      ##   [1] 1 1 1 0 0 1 1 1 0 1 0 1 1 1 0 1 1 1 1 0 1 0 1 0 1 1 0 1 1 1 0 1 0
      ##  [34] 0 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 1 1 1 1 1 0 1 0 0 1 0 0 1 0 1 0
      ##  [67] 1 0 0 1 1 1 0 0 1 0 0 1 1 0 1 1 0 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 0
      ## [100] 1 1 0 0 0 1 0 1 1 1 1 0 1 1 0 0 0 1 0 1 1 0 1 0 1 1 1 1 1 1 1 1 0
      ## [133] 1 0 1 0 0 0 1 1 0 0 1 1 1 1 0 0 1 1 0 1 0 1 1 1 1 1 0 1 0 0
      ## 
      ## $weights[[125]]
      ##   [1] 1 1 0 1 1 0 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 0 0
      ##  [34] 1 0 1 1 0 1 1 0 0 1 0 1 1 0 0 0 0 1 0 1 1 1 0 1 1 1 0 1 0 1 1 1 1
      ##  [67] 0 0 1 1 0 1 1 0 1 0 0 0 1 1 0 1 1 1 0 0 1 1 1 0 1 1 0 1 1 1 0 1 0
      ## [100] 1 1 1 1 1 1 1 0 0 0 1 1 0 1 1 1 0 1 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1
      ## [133] 0 0 1 0 1 1 0 1 0 0 0 0 1 1 1 1 0 0 0 1 1 1 0 1 0 0 1 0 1 0
      ## 
      ## $weights[[126]]
      ##   [1] 1 0 1 0 0 0 0 1 1 0 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 0 1 0
      ##  [34] 0 1 1 0 1 1 0 0 1 0 0 1 0 1 0 0 0 1 1 1 1 1 0 1 1 0 1 1 1 0 0 0 1
      ##  [67] 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 0 1 0 0 1 1 1 1 1 0
      ## [100] 1 1 0 1 1 1 0 1 1 1 0 1 1 1 0 1 1 1 0 0 1 0 1 1 1 1 0 0 1 1 1 0 0
      ## [133] 1 0 0 1 1 1 1 0 0 0 1 0 0 1 1 0 0 1 0 1 1 0 1 0 1 1 1 0 0 1
      ## 
      ## $weights[[127]]
      ##   [1] 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 1 1 0 0 0 0 1 1 1 1 1
      ##  [34] 1 1 1 0 1 1 0 0 0 1 1 1 0 0 1 1 0 1 1 0 1 0 0 0 0 0 1 1 0 1 1 1 1
      ##  [67] 1 1 1 1 0 1 1 1 0 0 1 0 1 1 0 0 1 1 0 1 1 1 1 0 0 1 1 0 0 1 1 1 0
      ## [100] 1 0 1 1 0 1 1 1 1 1 0 0 1 0 1 1 0 1 1 0 1 1 1 1 1 0 0 1 1 0 0 1 0
      ## [133] 0 0 1 1 1 1 0 1 1 1 1 0 0 0 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0
      ## 
      ## $weights[[128]]
      ##   [1] 1 0 1 1 0 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 0 0
      ##  [34] 1 0 0 0 1 1 0 1 0 0 0 1 0 1 0 1 1 0 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1
      ##  [67] 0 1 1 0 1 1 1 0 0 0 1 0 1 1 0 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 0
      ## [100] 1 1 0 1 0 0 1 0 1 1 1 1 1 1 0 0 1 0 1 1 1 0 1 1 1 1 1 0 1 0 0 0 1
      ## [133] 1 1 1 0 1 0 0 0 0 1 0 1 1 1 0 0 1 1 1 1 0 0 1 0 1 0 1 1 1 1
      ## 
      ## $weights[[129]]
      ##   [1] 1 1 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 0 0 1 0 0 1 0 1
      ##  [34] 1 1 0 0 0 0 1 0 0 1 1 1 1 1 1 0 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 0 1
      ##  [67] 0 1 0 1 1 1 0 0 1 0 1 1 0 1 1 0 1 0 1 0 0 1 1 1 1 0 1 0 1 0 1 0 1
      ## [100] 0 1 1 1 0 0 0 1 1 1 1 1 0 0 1 1 0 0 0 1 1 0 1 1 1 0 1 0 1 0 0 1 1
      ## [133] 1 1 0 0 1 1 0 1 0 1 1 1 0 0 1 1 1 0 0 1 1 0 1 1 1 1 1 1 0 0
      ## 
      ## $weights[[130]]
      ##   [1] 0 1 1 1 1 1 1 0 1 1 0 0 1 1 0 0 0 1 0 0 1 0 1 1 1 1 1 1 1 0 1 0 1
      ##  [34] 1 1 0 1 1 1 1 1 1 1 0 0 0 0 1 0 1 1 1 0 0 0 1 1 0 1 1 0 1 1 0 1 1
      ##  [67] 1 0 0 1 0 1 1 0 0 1 0 1 0 0 0 1 1 1 1 1 0 1 1 0 1 1 1 1 0 0 1 1 0
      ## [100] 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 0 1 0 1 1 0 1 1 0 0
      ## [133] 1 1 1 1 0 0 1 1 1 1 1 0 0 0 1 1 0 1 1 1 0 1 0 0 1 0 1 1 0 0
      ## 
      ## $weights[[131]]
      ##   [1] 1 1 1 0 0 1 0 1 0 1 1 0 1 1 1 0 0 0 1 1 1 1 1 0 1 0 1 1 1 0 1 1 0
      ##  [34] 1 1 1 1 0 0 1 1 0 0 0 1 1 1 1 1 0 1 1 0 1 0 1 1 0 0 0 0 1 0 0 1 0
      ##  [67] 0 1 1 1 1 0 1 0 1 1 0 1 0 1 0 0 1 0 1 1 1 0 1 1 1 0 1 0 0 1 0 0 1
      ## [100] 1 0 0 0 1 1 1 1 1 0 0 1 0 1 0 1 1 0 0 1 1 1 1 0 1 1 1 1 1 0 1 1 1
      ## [133] 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 1 0 1 0 0 1 0 0 1 1
      ## 
      ## $weights[[132]]
      ##   [1] 1 0 1 0 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0 1 1 0 1 1 1 1 0 1 0 1 1 1 1
      ##  [34] 1 1 0 1 1 0 0 0 1 0 1 1 1 0 0 1 0 1 1 0 0 1 1 1 1 1 1 0 1 0 0 0 1
      ##  [67] 1 0 1 1 1 1 0 1 1 1 0 1 1 0 0 1 0 0 1 0 0 0 0 1 1 1 1 1 1 0 0 1 1
      ## [100] 0 0 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 0 0 1 0 0 1 1 1 1 1
      ## [133] 0 0 1 1 1 1 1 0 1 1 1 0 0 1 1 0 0 1 1 1 0 1 1 1 0 0 1 1 0 1
      ## 
      ## $weights[[133]]
      ##   [1] 1 0 0 1 0 1 1 0 1 1 0 0 1 0 1 1 1 1 1 0 1 0 1 0 0 0 0 1 0 1 1 1 1
      ##  [34] 1 1 1 1 1 1 1 0 1 0 1 0 0 1 1 0 1 1 0 1 1 1 1 1 0 0 1 0 0 1 0 0 1
      ##  [67] 1 1 0 1 1 0 0 0 1 1 0 1 0 0 1 0 1 1 1 0 1 1 1 1 1 1 1 1 0 1 0 0 1
      ## [100] 1 1 1 0 1 1 1 0 0 1 0 0 1 0 0 1 1 0 1 1 1 1 0 0 0 1 1 1 1 0 1 1 0
      ## [133] 1 1 1 1 1 0 1 1 1 0 0 1 0 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 0 0
      ## 
      ## $weights[[134]]
      ##   [1] 0 1 1 1 0 1 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 0 1 0
      ##  [34] 0 0 0 0 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 0 1 1 1 0 0 1 1 1
      ##  [67] 1 1 1 0 1 1 1 1 1 0 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 0 1 0 1 1 1 1 0
      ## [100] 0 0 1 0 1 1 0 1 0 0 0 1 1 1 1 0 1 0 0 1 1 1 1 1 0 0 1 0 1 0 0 1 1
      ## [133] 0 0 1 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 0
      ## 
      ## $weights[[135]]
      ##   [1] 1 0 1 0 1 1 1 0 0 0 0 1 1 1 1 1 1 0 1 1 1 1 0 0 1 0 1 1 1 1 1 1 1
      ##  [34] 0 0 0 0 1 0 1 1 1 1 0 1 0 0 0 1 0 0 1 0 0 1 1 0 1 1 1 0 1 1 1 1 1
      ##  [67] 0 1 0 0 1 1 1 1 1 0 1 0 1 0 1 0 0 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1
      ## [100] 1 0 1 0 1 0 0 1 0 1 0 0 0 1 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0 1 1 1 1
      ## [133] 1 1 0 1 0 1 1 1 1 1 0 0 1 0 1 1 0 1 1 0 1 1 1 0 0 1 1 1 0 0
      ## 
      ## $weights[[136]]
      ##   [1] 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 0 1 1 1 1 1
      ##  [34] 0 1 0 1 1 1 0 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 0
      ##  [67] 0 1 0 0 1 1 0 0 0 1 1 0 1 1 0 1 1 0 1 1 0 0 1 1 1 1 0 1 0 1 1 1 1
      ## [100] 0 1 1 1 1 0 1 1 0 0 1 0 0 0 1 1 0 0 1 1 0 0 1 0 1 0 1 0 1 0 1 1 0
      ## [133] 1 0 0 1 1 1 1 1 0 1 1 1 1 0 0 1 1 1 0 0 1 0 1 1 1 1 0 0 1 0
      ## 
      ## $weights[[137]]
      ##   [1] 1 1 1 1 0 0 1 0 1 0 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 0 1 1 1 0 0 1 1
      ##  [34] 0 0 1 1 1 1 1 0 1 0 1 0 0 0 0 1 1 1 0 0 0 0 1 1 0 1 1 0 0 0 1 1 1
      ##  [67] 1 0 1 1 1 1 0 0 1 1 1 1 0 1 1 1 1 1 0 0 1 1 0 0 1 1 1 0 1 0 1 1 0
      ## [100] 1 1 0 1 0 1 1 0 1 1 1 1 0 1 1 1 1 1 0 1 1 0 0 1 1 1 1 0 0 1 0 1 1
      ## [133] 0 0 1 1 0 0 1 1 0 1 1 1 1 1 1 0 1 1 0 1 0 0 0 1 1 1 0 0 0 0
      ## 
      ## $weights[[138]]
      ##   [1] 0 1 0 1 1 1 0 1 0 0 1 1 1 1 1 0 0 0 1 0 1 1 1 0 0 1 0 1 1 1 0 0 1
      ##  [34] 0 0 1 1 0 1 0 0 0 1 1 0 0 1 1 0 0 0 1 1 1 1 1 1 0 1 1 0 0 0 1 1 1
      ##  [67] 1 0 0 0 1 1 1 1 0 1 1 1 1 1 1 0 1 1 0 1 0 1 1 0 0 0 1 1 1 1 0 1 1
      ## [100] 0 1 1 1 1 1 0 0 1 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 1 1
      ## [133] 0 0 1 1 0 0 0 1 1 1 1 1 0 0 1 1 1 1 0 0 0 0 0 1 1 1 1 1 0 1
      ## 
      ## $weights[[139]]
      ##   [1] 1 1 1 1 0 0 0 1 1 0 1 0 0 1 1 1 1 1 1 0 0 0 0 1 0 1 1 1 1 1 0 1 0
      ##  [34] 1 1 0 0 1 0 1 0 1 1 1 0 0 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 1 0 0 1 0
      ##  [67] 1 0 1 1 0 0 1 0 1 1 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 1 0 0 1 1 1 0 1
      ## [100] 0 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 1 1 1 0
      ## [133] 1 1 1 0 1 1 1 0 0 0 1 0 1 1 0 0 1 1 1 0 0 1 1 0 1 1 1 1 0 0
      ## 
      ## $weights[[140]]
      ##   [1] 1 0 1 1 0 1 0 1 1 1 1 1 1 1 0 0 1 0 0 1 1 0 1 1 0 0 1 1 0 1 1 1 0
      ##  [34] 1 0 0 1 1 1 0 1 1 1 0 1 0 1 1 0 0 0 0 1 1 0 1 1 0 1 1 0 1 1 1 1 0
      ##  [67] 0 1 0 1 1 1 1 0 1 0 0 1 1 1 0 1 1 1 1 0 1 0 1 0 0 1 1 1 1 0 1 0 1
      ## [100] 1 0 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 1 0 1 1 1 0 0 0 1 1 1 1 1 1 0 1
      ## [133] 0 1 0 0 0 1 1 0 0 1 1 0 1 0 1 0 0 1 1 0 1 1 1 1 1 0 1 0 1 0
      ## 
      ## $weights[[141]]
      ##   [1] 1 1 1 0 1 1 0 1 0 0 1 1 0 0 1 1 1 1 1 0 0 1 1 0 0 1 1 1 0 0 0 1 1
      ##  [34] 0 1 1 0 1 0 1 0 1 0 0 1 0 1 1 1 1 0 1 0 1 1 1 1 0 0 1 0 0 1 0 0 1
      ##  [67] 0 1 1 0 1 1 0 1 1 1 0 0 1 0 0 1 0 0 0 1 1 1 1 1 0 1 1 1 1 1 0 1 0
      ## [100] 0 1 1 0 1 1 0 1 1 1 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1
      ## [133] 0 1 1 1 1 0 0 0 1 0 1 0 1 0 1 1 1 1 1 0 1 0 0 1 0 1 1 1 1 1
      ## 
      ## $weights[[142]]
      ##   [1] 1 1 0 1 0 1 0 0 0 1 1 1 0 0 0 1 1 1 1 0 0 1 1 1 0 0 1 1 0 1 0 0 1
      ##  [34] 0 0 1 1 1 1 0 1 1 0 0 1 0 1 1 1 1 1 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1
      ##  [67] 1 1 0 0 1 1 1 0 1 0 1 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 0 0 0 1 1
      ## [100] 1 0 1 0 0 1 1 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1 1 0 0 0 0 1 1 1 0 1 1
      ## [133] 0 1 1 0 1 0 1 1 1 0 1 1 1 1 1 0 0 1 0 1 0 1 1 1 0 1 1 1 1 0
      ## 
      ## $weights[[143]]
      ##   [1] 1 0 1 1 1 0 1 1 0 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 0 0 0 0 1 0 1 1
      ##  [34] 0 1 0 1 0 1 1 1 1 0 1 0 0 1 1 0 1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1 0
      ##  [67] 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 0 0 0 1 1 1 1 0 0 0 1 0 0 0 0 1 0 1
      ## [100] 1 1 0 0 1 1 0 0 1 0 1 1 1 1 0 1 1 1 0 0 1 0 1 1 0 1 1 1 1 1 1 0 1
      ## [133] 1 1 1 0 1 1 0 1 1 1 1 0 0 1 1 1 0 0 1 1 1 1 1 1 0 0 1 1 1 1
      ## 
      ## $weights[[144]]
      ##   [1] 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 0 0 0 0 1 1 1 0 0 1 0 0 1
      ##  [34] 1 0 1 1 0 1 0 1 1 1 0 0 1 1 1 1 0 1 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1
      ##  [67] 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 1 1 1 0 0 0 1 1 1 1
      ## [100] 1 1 0 1 1 1 0 0 1 0 0 0 1 1 0 1 0 0 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0
      ## [133] 0 0 0 0 1 0 1 1 1 0 0 1 1 0 0 0 1 0 1 1 1 1 0 1 1 1 1 0 1 1
      ## 
      ## $weights[[145]]
      ##   [1] 0 1 1 1 0 1 0 1 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 0 1 1 1 0 1 0 0 1 0
      ##  [34] 1 1 1 0 1 0 1 1 0 1 1 1 1 1 1 0 1 1 1 0 0 1 0 0 1 0 1 0 0 1 1 0 0
      ##  [67] 1 0 0 0 1 1 1 0 1 1 0 0 0 0 1 0 1 1 0 1 0 0 0 0 1 0 1 0 1 1 1 1 1
      ## [100] 1 1 0 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 0 0 1 0
      ## [133] 1 1 1 0 1 1 0 1 1 1 0 1 1 1 1 0 1 0 1 0 1 0 1 1 1 1 1 1 1 1
      ## 
      ## $weights[[146]]
      ##   [1] 0 1 1 1 0 1 0 1 1 1 1 0 1 0 0 1 0 1 1 1 0 1 1 1 1 0 0 1 0 0 1 1 1
      ##  [34] 0 0 0 1 1 1 1 1 0 1 1 1 0 1 1 0 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 0
      ##  [67] 0 1 0 1 0 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 0 1 0 1 1
      ## [100] 1 0 1 1 0 1 1 0 1 1 1 1 1 0 0 1 0 0 1 0 0 1 1 1 0 1 0 1 1 0 0 1 1
      ## [133] 0 1 0 1 1 0 0 0 1 0 0 0 0 1 1 1 1 0 1 0 1 1 0 0 1 1 1 0 0 1
      ## 
      ## $weights[[147]]
      ##   [1] 1 1 1 1 1 1 0 0 1 0 1 0 1 0 0 0 0 1 0 1 1 1 0 0 1 0 1 1 0 1 0 1 0
      ##  [34] 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 0 0 1 1 0
      ##  [67] 1 0 1 0 1 1 1 0 0 1 1 0 1 0 1 1 0 0 1 0 1 1 1 1 0 0 0 1 1 1 0 0 1
      ## [100] 0 1 1 1 1 1 0 0 1 1 0 1 1 1 1 1 0 0 1 1 0 0 1 0 0 0 0 0 1 1 1 1 1
      ## [133] 1 1 1 0 1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 0 1 1 0 1
      ## 
      ## $weights[[148]]
      ##   [1] 0 1 1 0 1 1 1 1 1 1 1 1 0 0 1 1 0 1 1 0 1 1 0 0 0 1 1 1 1 1 0 1 1
      ##  [34] 1 1 1 0 1 0 0 0 0 1 0 0 1 0 1 1 1 0 0 0 0 1 0 1 0 1 1 1 1 1 1 1 0
      ##  [67] 0 1 1 1 0 0 1 0 1 0 1 0 1 1 0 0 1 1 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1
      ## [100] 0 1 0 1 0 0 0 1 1 0 1 1 1 1 0 0 1 1 1 1 0 0 0 0 1 1 1 1 1 0 1 1 1
      ## [133] 0 1 1 0 0 1 1 1 1 0 1 1 1 1 0 0 1 0 1 1 0 0 1 1 1 1 1 0 1 1
      ## 
      ## $weights[[149]]
      ##   [1] 1 1 1 1 1 0 0 1 0 1 1 1 1 0 0 0 1 1 0 0 1 0 1 0 1 0 1 1 0 1 1 0 1
      ##  [34] 1 0 0 1 0 1 0 0 0 1 0 1 1 0 1 1 1 0 1 1 0 1 1 1 0 0 1 1 0 0 1 0 1
      ##  [67] 1 1 1 1 0 0 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 0 0 1 0 1 1 0 1 0 1 0 1
      ## [100] 1 1 0 1 1 0 0 1 0 1 1 1 1 1 1 1 0 1 1 0 1 1 1 0 0 0 1 1 0 1 1 0 1
      ## [133] 1 1 0 1 1 0 1 1 1 1 0 1 0 1 0 0 0 1 1 1 1 1 1 1 0 1 0 1 0 0
      ## 
      ## $weights[[150]]
      ##   [1] 0 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 1
      ##  [34] 0 0 1 0 1 0 0 1 0 1 0 0 1 1 0 0 1 1 1 0 1 0 0 1 1 0 1 1 0 1 1 0 1
      ##  [67] 0 1 0 1 0 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 0 1 1 0 1 0 1 0 0 1 0 1 1
      ## [100] 1 1 1 1 1 1 1 1 1 0 0 1 1 0 1 1 0 1 1 0 0 0 1 0 1 1 1 1 1 1 1 1 1
      ## [133] 0 0 1 0 0 0 0 0 0 1 1 0 0 0 0 1 0 1 1 1 0 1 1 1 0 1 0 1 0 1
      ## 
      ## $weights[[151]]
      ##   [1] 1 0 1 1 0 0 0 0 1 1 1 1 1 0 1 0 1 0 1 1 1 0 0 1 1 0 0 1 0 1 1 0 0
      ##  [34] 1 0 1 1 1 0 1 0 1 0 1 1 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1 0 0 1 1 0 1
      ##  [67] 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0 1 0 0 1 1 1 0 1 1 1 0 1 0 1 1 1 0 0
      ## [100] 0 1 0 1 0 0 0 0 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 0 1 1 0 1 1 0
      ## [133] 1 0 1 1 0 1 1 1 1 1 1 0 1 0 0 1 1 1 1 1 1 0 1 0 1 0 1 1 0 1
      ## 
      ## $weights[[152]]
      ##   [1] 1 0 0 1 1 1 1 1 0 0 1 1 0 0 0 1 0 1 1 1 0 1 0 1 0 0 1 0 0 0 0 0 0
      ##  [34] 1 1 0 1 1 1 0 1 0 1 0 0 1 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 0 1 0 1
      ##  [67] 0 1 1 1 1 0 0 0 0 1 1 0 0 1 0 1 1 0 1 1 1 1 0 1 1 1 1 0 1 1 1 1 0
      ## [100] 1 1 1 0 1 0 1 1 0 0 1 1 0 0 0 1 0 1 0 1 1 1 1 1 0 1 1 1 1 0 0 1 0
      ## [133] 1 1 1 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0 0
      ## 
      ## $weights[[153]]
      ##   [1] 1 1 1 0 1 1 0 0 0 1 1 1 0 1 0 1 0 0 1 1 0 0 1 1 1 0 1 1 1 1 1 1 1
      ##  [34] 1 1 1 1 1 0 0 0 1 1 0 1 0 0 1 0 0 0 1 0 1 1 0 0 0 0 1 0 1 0 1 1 1
      ##  [67] 0 1 0 1 1 0 1 0 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0 0 0 1 1 0 1 1 1 1
      ## [100] 1 0 1 0 0 1 1 0 1 0 1 1 0 0 0 1 1 1 1 1 1 0 1 0 0 1 0 0 0 0 1 1 1
      ## [133] 1 0 1 1 1 1 1 1 0 1 0 1 0 1 1 1 0 1 0 1 1 0 1 1 1 1 0 1 1 1
      ## 
      ## $weights[[154]]
      ##   [1] 0 1 1 1 0 1 1 0 0 0 1 0 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1
      ##  [34] 1 0 0 1 1 0 1 1 0 1 0 1 0 0 1 1 0 0 0 1 1 1 0 1 1 1 1 1 0 1 0 1 1
      ##  [67] 1 0 0 0 1 1 1 1 0 0 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1 0 0 1
      ## [100] 1 1 0 1 1 1 1 1 0 0 1 1 0 1 1 1 0 1 0 1 0 0 1 1 1 0 1 0 0 1 1 0 0
      ## [133] 0 1 1 1 1 0 0 0 1 1 1 0 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 1 0 1
      ## 
      ## $weights[[155]]
      ##   [1] 0 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 0 0 1 1 0 0
      ##  [34] 0 0 1 0 0 1 1 1 1 1 1 1 0 0 0 1 1 0 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1
      ##  [67] 0 1 1 0 1 1 1 1 0 1 0 1 1 1 0 0 0 0 0 1 0 0 1 1 1 0 0 1 1 1 1 1 0
      ## [100] 0 1 0 0 1 1 0 0 0 1 0 0 1 0 1 1 0 1 1 1 1 0 1 0 0 1 1 1 1 1 1 0 0
      ## [133] 0 1 0 0 1 1 1 1 1 0 0 0 1 1 1 0 1 1 1 1 0 1 0 1 1 1 1 0 1 1
      ## 
      ## $weights[[156]]
      ##   [1] 1 0 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1 0 0 1 0 1 1 0 1 0 0 1 0 0 1 0 1
      ##  [34] 0 1 1 0 1 1 1 0 1 1 0 1 0 1 1 1 1 0 1 1 1 0 1 1 0 0 0 0 0 1 1 0 1
      ##  [67] 1 0 0 1 1 1 1 1 0 1 1 0 1 1 0 1 0 1 0 0 1 1 1 1 1 0 1 1 0 1 0 0 0
      ## [100] 1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 1 0 1 1 1 0 0 1 0 1 0 1 0 0 1 1 1 1
      ## [133] 1 1 1 1 0 0 1 1 0 1 1 1 0 1 1 0 1 0 0 1 0 1 1 1 1 1 0 1 0 1
      ## 
      ## $weights[[157]]
      ##   [1] 0 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 0 0 1 0 1 1 0 1 0 1 1 1 0 1
      ##  [34] 1 1 1 1 1 1 1 0 1 1 0 0 1 0 0 1 0 1 1 0 0 0 1 1 0 0 0 0 1 1 0 1 1
      ##  [67] 1 1 0 1 0 1 1 0 1 0 1 1 1 0 1 0 0 1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 1
      ## [100] 1 1 0 0 0 0 1 1 1 1 0 0 1 1 1 1 0 0 0 1 0 1 0 1 0 1 1 1 0 1 1 0 0
      ## [133] 0 1 1 1 1 1 1 0 1 0 1 1 1 0 1 1 1 0 0 1 1 0 1 0 1 1 1 0 1 0
      ## 
      ## $weights[[158]]
      ##   [1] 1 0 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 1 1 1
      ##  [34] 0 1 1 1 0 1 1 0 1 0 1 0 0 0 0 1 0 0 0 0 0 1 1 0 1 1 0 0 1 0 0 0 0
      ##  [67] 0 1 1 1 1 0 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 1 0
      ## [100] 1 0 1 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 1 0 1 1 1 0 1 1 1
      ## [133] 0 0 0 0 0 1 0 1 1 0 0 1 1 1 0 0 0 0 1 0 1 1 1 1 0 1 1 1 1 1
      ## 
      ## $weights[[159]]
      ##   [1] 1 1 1 0 0 0 1 1 0 0 0 1 0 1 1 1 0 1 1 1 1 1 1 1 0 1 0 1 1 0 1 1 1
      ##  [34] 0 1 1 1 0 0 0 1 0 1 1 0 0 0 0 1 1 1 0 1 1 0 0 1 1 1 1 1 1 1 0 0 1
      ##  [67] 0 0 1 0 0 0 1 1 1 1 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1
      ## [100] 0 1 1 1 0 1 0 0 0 1 0 0 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0 0 1 0 1 0 1
      ## [133] 0 1 1 0 1 1 0 0 0 1 1 0 1 0 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1
      ## 
      ## $weights[[160]]
      ##   [1] 1 1 1 0 1 0 0 1 0 1 1 1 1 0 1 1 0 1 1 0 1 0 1 0 1 0 1 1 0 1 0 0 0
      ##  [34] 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 0 0 0 1 1 1 0 1 0 1 1 1 1 0 1
      ##  [67] 1 0 1 0 0 0 0 1 0 1 0 1 1 0 1 1 1 1 1 1 1 1 0 1 0 0 0 0 1 1 1 1 0
      ## [100] 1 1 1 0 1 1 1 1 1 0 0 0 0 0 0 1 0 0 0 0 1 1 0 0 1 1 1 0 0 0 1 1 1
      ## [133] 0 0 1 1 1 1 0 1 1 1 1 1 0 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 1
      ## 
      ## $weights[[161]]
      ##   [1] 1 0 0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 0 0 1 0 1 1 1 1 0 1 0 0 1 1 1 0
      ##  [34] 1 1 0 1 1 1 1 1 1 1 1 1 0 0 1 1 0 1 0 0 1 1 1 0 0 1 1 0 0 0 0 1 1
      ##  [67] 0 0 0 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 0 1
      ## [100] 1 0 1 1 0 0 1 1 1 1 0 0 1 1 1 0 1 0 1 0 0 1 0 0 1 1 0 1 0 1 1 1 1
      ## [133] 0 0 1 1 1 1 1 0 1 0 1 0 1 0 1 0 0 0 0 0 1 0 1 1 1 1 1 0 1 0
      ## 
      ## $weights[[162]]
      ##   [1] 1 1 1 0 1 1 1 0 1 0 0 0 0 1 0 1 1 1 1 1 0 1 0 1 1 0 0 1 1 1 1 0 1
      ##  [34] 1 1 1 1 0 0 0 0 1 1 1 1 0 1 1 0 1 0 1 1 0 1 0 0 0 1 0 1 1 0 1 1 0
      ##  [67] 1 0 0 0 0 1 1 0 0 1 1 0 1 1 1 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 0 1 1
      ## [100] 1 1 1 0 1 1 0 1 1 0 1 0 0 1 0 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 1 0 1
      ## [133] 1 0 1 0 1 0 1 1 1 0 1 1 1 0 0 0 0 1 1 0 1 1 1 1 1 1 1 1 1 1
      ## 
      ## $weights[[163]]
      ##   [1] 1 0 1 1 0 0 1 1 0 0 1 1 1 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1 0 0 1 1 0
      ##  [34] 1 0 1 1 1 1 1 1 0 1 1 1 0 0 0 0 1 0 1 1 0 1 1 1 1 1 1 1 0 1 0 1 1
      ##  [67] 0 1 1 1 0 1 0 1 0 0 1 1 0 1 0 1 1 1 0 1 0 1 1 1 0 0 1 1 1 0 0 1 1
      ## [100] 1 1 1 1 1 1 0 1 1 0 0 0 0 1 0 0 0 1 0 1 1 0 0 1 1 0 0 1 0 1 1 0 1
      ## [133] 1 0 1 1 1 1 1 0 1 1 1 0 1 1 0 1 1 0 0 1 1 1 1 0 0 1 1 1 1 0
      ## 
      ## $weights[[164]]
      ##   [1] 1 0 1 1 0 1 1 1 1 0 1 0 0 1 1 1 0 1 1 1 0 1 0 1 0 0 0 1 1 1 1 0 1
      ##  [34] 1 0 0 0 1 0 0 0 1 0 1 1 0 0 1 1 1 0 1 0 1 1 1 1 1 1 0 1 1 1 1 0 1
      ##  [67] 1 1 1 1 1 1 1 0 1 0 0 0 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 1 0 1 1 1 1
      ## [100] 0 1 1 1 0 0 1 1 0 1 1 1 0 1 1 1 0 0 1 0 0 1 1 1 1 1 0 0 1 1 1 1 1
      ## [133] 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 0 0 1 1 0 1 1 1 0 0 1 0 0 0 1
      ## 
      ## $weights[[165]]
      ##   [1] 0 1 1 0 1 0 0 1 0 1 0 1 1 0 1 1 0 1 1 1 1 0 1 0 1 1 0 0 0 1 1 1 1
      ##  [34] 0 1 1 0 1 0 1 1 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 0 0 0 1 0 0 0 1 1 1
      ##  [67] 0 1 1 1 0 1 0 1 0 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 0 1 0 1 1
      ## [100] 0 1 1 1 0 1 0 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 0 0 1 0 1 1 0 1 1 1
      ## [133] 1 1 1 1 1 1 0 0 1 1 1 1 0 1 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1
      ## 
      ## $weights[[166]]
      ##   [1] 0 1 1 1 1 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 0 1 1 1 1 1 1 1 1 0 1
      ##  [34] 1 1 0 1 0 1 1 1 1 1 1 1 0 1 0 0 1 0 0 1 1 1 0 1 1 1 0 1 0 1 1 0 0
      ##  [67] 1 1 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1 0 0 1 0 0 0 0 1 0 1 0 1 0 1 0 0
      ## [100] 0 1 0 1 1 0 1 1 1 1 1 1 1 0 0 1 0 0 1 1 0 1 1 0 1 0 0 1 1 1 0 1 1
      ## [133] 1 1 0 1 1 0 1 0 0 1 0 0 1 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1
      ## 
      ## $weights[[167]]
      ##   [1] 1 1 1 1 0 1 1 1 1 0 0 0 1 1 0 0 1 1 1 0 1 1 0 1 0 1 1 1 1 0 0 0 0
      ##  [34] 1 0 1 1 1 1 1 0 0 1 0 1 0 1 1 0 0 1 0 1 1 1 1 0 1 1 1 0 1 1 0 1 1
      ##  [67] 1 0 0 1 1 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 0 1 1 0 1 1 0 1 1 0 0 1 1
      ## [100] 1 0 1 1 1 0 1 0 0 1 0 0 0 0 0 1 1 1 0 1 0 1 1 0 1 0 0 1 1 1 0 0 1
      ## [133] 0 1 1 1 1 1 1 0 1 0 1 0 0 0 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1
      ## 
      ## $weights[[168]]
      ##   [1] 1 1 1 0 1 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 0 0 0 0 1 1 1 0 1 0 0 0
      ##  [34] 1 0 1 1 1 0 1 0 0 0 0 0 1 0 1 1 1 0 1 0 0 0 1 0 1 1 1 0 0 1 0 0 0
      ##  [67] 1 1 0 0 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 1 1 0 1 0 0 0
      ## [100] 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 0 0 0 1 0 1 1 0 1 1 0 1 1
      ## [133] 1 1 0 1 1 0 1 1 1 0 0 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 1
      ## 
      ## $weights[[169]]
      ##   [1] 0 1 0 1 0 1 0 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 1 0 0 1 1 0 1 1 1 1 1
      ##  [34] 1 0 1 0 0 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 0 1 0 0 1 0 0 1 1 1 0 1
      ##  [67] 1 0 1 0 0 1 0 1 0 1 0 1 0 0 1 1 0 1 1 0 0 1 1 1 0 0 1 1 1 1 1 1 1
      ## [100] 0 1 1 1 0 0 0 1 0 0 0 1 0 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 1 1 1 0 1
      ## [133] 0 1 0 0 1 0 0 1 0 1 1 1 0 1 1 0 1 0 1 0 1 1 0 0 1 1 1 1 1 1
      ## 
      ## $weights[[170]]
      ##   [1] 1 1 1 0 0 1 1 1 1 0 1 0 0 0 1 1 0 1 0 0 1 1 1 1 1 0 1 0 1 1 0 0 0
      ##  [34] 1 0 0 1 1 1 1 1 0 1 0 1 1 0 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1
      ##  [67] 0 1 1 0 1 1 1 1 1 1 1 1 0 1 0 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1 1
      ## [100] 0 1 1 0 0 0 1 1 1 0 1 0 0 0 1 1 1 1 0 0 1 1 1 0 0 1 0 1 0 0 1 0 1
      ## [133] 1 1 1 0 0 0 0 1 0 1 0 0 0 1 1 1 0 1 1 1 1 1 1 0 0 1 1 0 1 0
      ## 
      ## $weights[[171]]
      ##   [1] 0 1 1 1 1 1 0 1 0 0 0 1 1 0 1 1 0 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 0
      ##  [34] 1 1 1 1 1 0 0 0 0 1 1 0 1 0 0 1 1 1 1 1 1 0 1 0 1 1 1 1 0 1 0 1 0
      ##  [67] 0 0 1 1 0 0 0 0 1 1 1 0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0
      ## [100] 0 0 1 0 0 1 1 1 0 0 1 0 1 0 0 1 1 0 0 1 0 0 1 0 1 1 0 1 1 1 1 1 1
      ## [133] 0 1 1 0 1 0 1 0 0 1 0 1 0 1 1 1 1 1 1 0 1 1 0 1 1 1 0 0 1 1
      ## 
      ## $weights[[172]]
      ##   [1] 1 0 1 0 1 1 1 1 1 1 1 1 0 1 0 0 1 1 1 0 0 1 1 1 1 0 1 1 1 0 1 1 1
      ##  [34] 0 1 1 1 0 1 1 0 0 0 0 1 1 1 1 1 0 0 1 1 1 0 1 0 0 1 1 0 1 1 0 1 1
      ##  [67] 1 0 1 1 1 1 1 0 0 0 0 1 1 0 1 0 0 0 0 1 1 1 0 1 1 0 1 0 1 1 0 1 1
      ## [100] 1 1 1 1 1 1 0 0 1 1 0 1 0 0 1 0 0 1 0 0 1 0 0 0 0 1 1 1 0 0 0 0 0
      ## [133] 1 1 1 1 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0
      ## 
      ## $weights[[173]]
      ##   [1] 0 0 1 0 1 1 1 0 0 1 0 1 1 0 1 0 1 1 0 1 1 0 1 1 1 1 1 0 1 0 1 0 1
      ##  [34] 1 1 1 0 0 1 1 0 1 1 0 1 0 1 1 0 1 0 1 1 1 1 1 0 1 0 0 0 1 1 1 1 0
      ##  [67] 1 0 0 1 1 0 1 1 1 0 0 1 1 0 1 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 1
      ## [100] 1 1 0 1 1 1 0 1 1 1 0 1 0 1 1 1 0 1 0 0 0 0 0 1 0 0 1 1 1 1 1 1 0
      ## [133] 0 1 0 1 0 0 1 1 0 0 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 0 0 1 0 0
      ## 
      ## $weights[[174]]
      ##   [1] 0 0 0 0 1 1 1 1 1 0 1 1 1 0 0 1 0 1 1 0 1 1 1 1 0 1 1 1 0 1 1 1 0
      ##  [34] 1 0 0 1 0 1 0 0 1 1 1 0 1 0 1 0 1 1 1 0 0 1 1 1 1 0 0 1 1 1 0 1 1
      ##  [67] 1 1 0 0 1 0 1 0 1 1 1 1 0 0 1 1 0 0 0 1 1 0 1 1 1 1 0 1 0 0 1 1 1
      ## [100] 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 1 1 0 0 0 1 0 1 0 0 1 1 1 1 1
      ## [133] 1 1 0 0 0 1 1 1 0 1 1 0 0 1 0 0 1 0 1 1 1 0 1 0 1 1 0 1 1 1
      ## 
      ## $weights[[175]]
      ##   [1] 0 0 1 1 0 0 0 1 1 1 1 0 0 1 0 0 1 0 1 1 1 1 1 1 0 1 1 1 0 1 0 1 1
      ##  [34] 1 1 0 1 1 1 1 1 0 1 0 1 0 1 0 0 0 1 0 1 0 1 0 0 1 0 1 0 0 1 1 1 1
      ##  [67] 1 1 0 1 0 1 1 0 1 1 1 1 1 0 1 1 0 0 0 1 0 1 0 1 1 1 0 0 1 1 1 1 1
      ## [100] 1 1 0 1 0 1 1 0 1 1 0 1 0 0 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 0 1 1 1
      ## [133] 1 0 1 1 1 0 0 1 1 1 1 1 0 0 0 0 1 1 0 0 1 0 1 0 0 1 1 1 1 1
      ## 
      ## $weights[[176]]
      ##   [1] 1 1 0 0 0 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 1 1
      ##  [34] 1 0 1 0 1 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 0 0 0 1 1 1 0 0 1 1 1 0
      ##  [67] 1 1 0 1 1 0 1 1 0 0 1 1 0 1 0 1 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 1 0
      ## [100] 1 0 0 1 0 1 1 0 1 1 0 1 1 0 1 1 0 0 0 0 0 1 0 1 1 1 0 1 1 1 1 1 0
      ## [133] 1 1 1 0 1 1 1 1 0 1 0 1 0 1 1 1 1 1 0 1 1 0 0 0 1 0 0 1 1 1
      ## 
      ## $weights[[177]]
      ##   [1] 1 0 1 1 1 1 1 0 1 0 1 1 0 0 1 0 1 1 0 1 0 1 0 0 0 1 0 1 1 0 0 0 0
      ##  [34] 1 0 1 1 1 1 0 1 0 1 0 1 0 0 0 1 1 0 1 1 1 0 0 1 1 1 1 1 0 1 1 0 1
      ##  [67] 0 0 0 1 1 1 1 1 0 1 1 0 0 1 1 1 1 0 1 1 0 1 1 1 1 0 1 0 1 0 1 0 1
      ## [100] 1 0 0 1 0 1 1 1 0 0 1 1 1 0 1 0 1 0 1 1 1 1 1 0 1 1 1 0 1 0 1 1 0
      ## [133] 1 1 1 1 0 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1 0 1 1 1 1 0 0 0 1
      ## 
      ## $weights[[178]]
      ##   [1] 1 1 1 1 0 0 1 1 1 0 0 0 1 0 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 1 1 0 1
      ##  [34] 1 1 0 0 1 0 0 1 1 0 1 1 0 1 0 0 0 1 1 0 1 0 0 1 1 0 1 1 1 1 1 1 1
      ##  [67] 1 1 0 1 1 1 1 0 1 1 1 0 0 0 1 0 1 0 0 0 0 1 0 1 1 1 1 1 0 1 1 1 1
      ## [100] 1 1 0 1 1 1 0 1 1 0 0 0 1 1 1 0 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1
      ## [133] 0 0 1 1 1 1 1 1 0 0 0 0 1 0 1 0 1 1 1 1 1 0 0 0 0 0 1 1 0 1
      ## 
      ## $weights[[179]]
      ##   [1] 0 1 0 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1 1 0 1 0 0 0 1 1 1 0 0 0 1 1 0
      ##  [34] 0 1 1 0 1 1 1 0 0 0 1 0 0 1 1 0 0 0 0 1 1 1 1 0 1 1 1 0 0 1 1 1 1
      ##  [67] 1 1 1 0 1 0 0 0 1 0 0 1 1 1 1 1 0 0 0 0 0 0 1 1 1 0 1 1 1 1 1 0 1
      ## [100] 0 1 1 0 1 1 0 0 1 1 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1
      ## [133] 0 1 1 0 1 1 1 0 0 0 1 1 0 0 1 0 1 1 1 1 1 1 1 0 0 1 0 1 1 0
      ## 
      ## $weights[[180]]
      ##   [1] 0 1 1 1 1 0 0 0 0 0 1 0 1 0 0 1 0 1 0 1 1 1 1 0 1 1 1 0 0 1 1 0 0
      ##  [34] 0 0 1 0 1 0 1 0 1 0 1 0 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 0 0 1 1 1
      ##  [67] 1 1 0 1 1 1 0 1 1 1 1 0 1 1 1 1 0 1 0 1 1 0 0 1 0 1 0 0 0 1 0 0 1
      ## [100] 1 0 1 0 1 1 0 1 1 1 1 0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 1
      ## [133] 1 1 1 1 1 1 1 1 1 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 0
      ## 
      ## $weights[[181]]
      ##   [1] 0 1 1 1 1 1 1 0 1 0 1 1 0 0 1 1 0 1 0 1 1 1 0 1 0 0 1 1 1 0 0 1 0
      ##  [34] 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 1 1 1 0 0 0 1
      ##  [67] 1 0 0 1 0 0 0 1 1 1 0 0 1 1 0 0 0 1 1 1 0 1 1 1 0 1 1 0 1 1 0 1 0
      ## [100] 0 0 0 1 0 1 1 0 0 1 1 1 0 0 0 1 1 1 0 0 1 0 1 0 1 1 1 1 1 0 1 1 1
      ## [133] 0 1 0 1 1 0 1 1 1 0 1 1 0 1 0 1 1 0 0 1 0 1 1 1 0 1 1 1 1 1
      ## 
      ## $weights[[182]]
      ##   [1] 1 1 0 1 0 1 1 0 1 1 0 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 0 1 1 0 1
      ##  [34] 0 1 0 1 0 0 0 1 0 1 1 1 1 1 1 0 1 1 1 0 1 0 1 1 1 0 0 1 0 0 1 1 1
      ##  [67] 1 1 1 1 0 0 1 0 1 1 0 1 1 1 1 0 0 1 0 1 1 1 1 0 1 1 1 0 0 0 1 0 0
      ## [100] 1 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 0 1 1 0 0 1 1 0 1
      ## [133] 1 1 0 1 1 1 0 1 0 0 0 0 1 1 1 0 1 0 1 1 1 0 0 1 0 0 1 0 0 1
      ## 
      ## $weights[[183]]
      ##   [1] 1 0 1 0 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 0 0 0 0 1 1 1 1 1 1
      ##  [34] 1 0 1 0 1 1 0 1 0 1 1 0 1 1 1 0 1 0 1 0 1 1 1 1 1 1 1 1 0 1 0 0 0
      ##  [67] 1 1 1 1 0 1 1 0 1 1 1 1 1 1 0 1 0 1 0 0 1 0 0 1 1 1 1 0 1 0 1 1 1
      ## [100] 1 1 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 1 1 1 1 1 0 0 1 0 0 1 1 1 1 1 1
      ## [133] 0 1 1 0 1 1 0 1 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 0 1 1 0 0 1
      ## 
      ## $weights[[184]]
      ##   [1] 1 1 1 1 0 1 0 0 1 0 0 1 1 0 1 1 1 1 1 1 0 1 0 0 0 1 1 1 1 1 1 0 0
      ##  [34] 1 0 1 1 1 1 1 0 1 1 1 0 0 0 1 1 1 1 0 0 0 1 0 0 0 0 0 0 1 1 1 1 0
      ##  [67] 1 1 0 0 1 1 0 1 1 0 1 1 1 1 0 1 1 1 1 0 0 0 1 1 0 0 0 1 1 0 0 1 1
      ## [100] 0 0 1 0 0 1 1 0 1 0 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1
      ## [133] 0 1 0 0 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 0 1
      ## 
      ## $weights[[185]]
      ##   [1] 1 1 0 1 1 1 1 1 0 0 1 1 1 0 1 1 1 1 0 1 1 0 1 1 1 1 0 0 0 1 0 1 1
      ##  [34] 0 0 0 1 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 0 1 0 1 1 1 0 1 1 0 1 1 0
      ##  [67] 1 1 1 1 0 1 1 0 1 0 1 1 1 1 0 0 0 1 0 0 0 0 0 0 1 1 0 1 0 1 1 1 1
      ## [100] 1 0 1 1 1 0 0 1 1 0 1 1 1 1 0 1 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1
      ## [133] 0 1 1 1 1 0 0 1 0 0 0 0 1 1 0 1 1 0 1 0 1 0 1 0 1 1 0 1 0 1
      ## 
      ## $weights[[186]]
      ##   [1] 1 0 1 0 1 0 1 0 1 0 1 1 1 1 0 0 0 0 0 1 1 0 0 0 1 1 1 1 1 0 1 0 1
      ##  [34] 1 1 1 1 0 1 1 0 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 0 0 0 1 1 1
      ##  [67] 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 0 1 0 1 1 0 1 0 1 1 0 0 1 1 1 1 0 1
      ## [100] 1 0 1 0 1 1 1 1 1 1 0 1 0 1 1 0 1 1 0 1 1 1 0 1 1 1 1 0 1 0 0 1 1
      ## [133] 0 0 0 1 1 1 1 0 0 1 0 1 1 0 0 1 0 0 0 1 1 0 0 0 1 1 1 1 0 1
      ## 
      ## $weights[[187]]
      ##   [1] 0 1 0 1 1 1 0 0 1 1 0 1 0 1 1 0 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1
      ##  [34] 0 0 0 0 1 1 1 1 0 1 0 1 1 0 0 0 1 0 1 1 0 1 1 1 0 0 0 0 0 1 1 1 1
      ##  [67] 0 0 0 1 1 1 1 1 1 1 0 1 1 0 0 0 1 0 0 1 1 1 0 1 1 1 1 0 1 0 0 0 1
      ## [100] 1 1 1 1 1 1 0 1 0 1 1 0 0 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 0 1 1 0
      ## [133] 1 1 1 1 1 1 0 0 1 0 1 1 1 1 1 1 1 0 1 1 0 1 0 1 0 1 1 0 1 1
      ## 
      ## $weights[[188]]
      ##   [1] 1 0 0 0 1 0 0 1 1 0 0 1 1 0 0 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0
      ##  [34] 0 0 0 1 1 0 0 1 1 0 0 1 0 1 0 1 1 0 1 0 0 0 1 1 1 0 0 0 1 0 0 1 1
      ##  [67] 1 1 1 0 1 0 1 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0 0
      ## [100] 1 1 1 0 1 0 0 0 1 1 1 1 1 1 1 1 0 1 1 0 0 0 1 1 1 1 0 0 1 1 0 1 1
      ## [133] 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1 0 0 1 1 1 0 0 1 1 1 0 1 0 0 0
      ## 
      ## $weights[[189]]
      ##   [1] 0 1 1 0 1 1 0 1 0 1 0 1 0 0 1 0 1 1 0 1 1 1 0 0 0 1 1 1 1 1 0 1 1
      ##  [34] 0 0 1 0 1 1 1 1 1 1 0 1 1 0 1 0 0 1 1 1 1 0 1 0 1 1 1 1 1 0 0 1 1
      ##  [67] 0 1 0 1 1 0 1 1 1 0 1 1 1 1 0 1 0 1 1 0 1 1 1 1 1 1 0 0 0 1 0 1 1
      ## [100] 1 1 1 1 1 1 0 1 1 0 1 0 1 0 1 1 1 0 1 1 0 0 1 0 1 1 1 1 1 1 1 1 1
      ## [133] 0 1 0 1 1 0 1 0 1 0 0 0 0 1 1 0 0 1 0 1 0 1 0 1 0 0 0 0 1 1
      ## 
      ## $weights[[190]]
      ##   [1] 1 0 0 0 1 1 1 0 0 0 1 0 1 1 1 1 1 0 1 1 1 1 1 0 1 0 1 0 0 1 1 1 0
      ##  [34] 1 1 0 0 1 1 0 1 0 0 1 1 1 1 0 0 1 1 1 0 1 1 1 0 0 1 1 0 1 0 0 1 0
      ##  [67] 0 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 0 1 0 0 0 0 1 1 0 1 1 0 1 1
      ## [100] 1 0 0 1 1 1 1 1 1 1 1 1 0 0 1 1 0 1 1 1 0 0 0 1 1 0 1 0 0 0 1 1 0
      ## [133] 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 0 0 1 0 1 0 1
      ## 
      ## $weights[[191]]
      ##   [1] 1 0 1 0 1 0 1 0 1 1 0 1 0 0 1 1 0 1 1 1 0 1 0 1 1 1 0 0 0 1 1 0 1
      ##  [34] 0 0 0 1 1 0 1 1 0 1 1 0 0 0 1 1 1 1 1 1 1 1 1 0 1 1 0 0 0 1 1 1 0
      ##  [67] 1 1 1 0 1 0 1 0 0 0 1 1 1 1 1 1 0 0 1 1 0 1 0 0 1 1 0 1 1 1 1 0 1
      ## [100] 1 1 1 0 1 0 1 0 0 1 1 1 1 1 0 1 0 1 0 1 1 1 1 0 1 1 1 0 0 1 0 0 1
      ## [133] 1 1 0 1 0 0 1 1 1 1 0 1 1 1 1 1 1 0 1 0 1 1 1 1 0 1 1 0 0 1
      ## 
      ## $weights[[192]]
      ##   [1] 1 0 1 0 0 1 1 0 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 0 1 1 0 0
      ##  [34] 1 1 0 1 1 0 0 1 1 0 1 0 1 0 1 1 1 1 1 0 1 1 1 1 1 1 0 0 0 1 0 0 0
      ##  [67] 1 1 0 0 1 1 1 1 0 1 1 0 1 0 1 0 1 1 1 0 0 1 0 0 1 1 1 1 0 1 1 0 0
      ## [100] 0 1 1 0 0 1 0 0 1 1 1 0 0 1 0 0 0 1 0 1 1 1 1 0 1 1 1 1 1 1 1 0 0
      ## [133] 0 1 1 1 1 1 0 1 1 1 1 1 0 1 1 0 0 1 1 1 0 0 1 1 0 1 1 1 1 0
      ## 
      ## $weights[[193]]
      ##   [1] 1 0 0 1 1 1 1 1 0 0 0 1 0 0 1 1 0 0 1 0 0 1 0 0 0 0 1 1 1 1 1 0 0
      ##  [34] 1 1 1 0 1 1 1 0 1 0 0 0 1 1 0 1 0 1 0 1 0 1 1 0 0 0 1 1 1 0 1 1 1
      ##  [67] 1 1 1 1 1 1 1 0 0 1 1 0 1 0 1 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
      ## [100] 0 1 0 0 0 1 1 1 1 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 1 0 1
      ## [133] 1 0 1 1 1 0 1 1 0 1 0 1 1 0 0 0 0 0 0 1 1 1 0 0 0 1 0 0 1 1
      ## 
      ## $weights[[194]]
      ##   [1] 0 1 1 1 0 0 0 1 1 1 1 1 1 0 0 1 0 0 0 1 1 1 1 0 1 1 0 1 1 0 1 0 0
      ##  [34] 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1
      ##  [67] 1 0 1 1 1 1 1 0 1 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 0 0 0 0 1 0 0 1
      ## [100] 0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 1 0 0 0 1 1 1 1 0 0 1 0 1 1 1 1 1 1
      ## [133] 1 0 0 1 1 0 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1 0 1
      ## 
      ## $weights[[195]]
      ##   [1] 1 0 1 1 0 0 1 1 0 1 1 0 0 1 1 1 1 1 1 1 1 0 0 1 0 1 1 0 1 1 0 1 0
      ##  [34] 0 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0 0 1 1 1 1 1 1
      ##  [67] 1 1 0 0 1 1 1 0 1 1 0 1 1 1 0 0 1 1 0 1 0 0 1 0 0 1 0 1 1 0 1 0 1
      ## [100] 1 1 1 0 1 0 1 1 0 1 0 1 0 1 1 1 0 0 1 0 0 0 0 1 0 1 1 1 1 0 0 0 0
      ## [133] 0 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 0 1 1 0 1 0 0 1 0 1 1 1 0
      ## 
      ## $weights[[196]]
      ##   [1] 1 1 1 1 0 1 1 1 1 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 0 1 1 0 0 0 1 1 0
      ##  [34] 1 1 1 1 1 0 1 1 0 1 1 0 0 1 0 1 1 0 1 0 1 0 1 0 1 1 0 1 1 0 0 0 1
      ##  [67] 0 1 1 0 0 1 0 1 1 1 0 1 1 0 1 1 1 1 1 0 1 0 0 1 0 1 1 0 1 1 1 1 1
      ## [100] 1 0 0 1 0 0 1 0 1 1 0 1 1 0 0 1 1 1 1 0 1 0 1 1 0 0 0 0 1 1 1 0 1
      ## [133] 1 0 0 1 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 0 1 0 0 1 1
      ## 
      ## $weights[[197]]
      ##   [1] 0 1 1 1 1 0 1 1 1 1 1 0 0 1 1 1 1 1 0 1 1 1 1 0 0 1 0 1 1 0 1 1 0
      ##  [34] 1 1 1 0 1 1 1 1 1 1 1 0 0 1 1 0 1 1 0 0 1 1 1 1 0 0 0 1 1 1 0 0 0
      ##  [67] 1 1 0 0 1 0 0 0 1 0 0 0 0 0 1 1 0 1 0 0 1 1 1 1 0 1 0 1 1 1 0 1 1
      ## [100] 0 1 1 1 1 1 1 0 1 1 1 0 1 0 1 0 1 1 1 0 0 0 1 1 1 1 0 1 1 1 0 1 0
      ## [133] 1 0 1 1 1 1 1 1 0 0 1 0 1 1 1 0 0 1 1 1 1 1 0 1 0 1 1 0 0 0
      ## 
      ## $weights[[198]]
      ##   [1] 1 1 1 1 0 1 1 1 0 1 0 1 1 1 1 1 1 0 1 0 1 0 0 1 0 0 0 1 0 1 1 0 1
      ##  [34] 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 0 0 1 0 1 1 0 1
      ##  [67] 0 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 1 1 1 1 1 1 0 1 0 1 0 1 1 0 0 0 1
      ## [100] 1 0 0 1 1 1 0 1 1 0 0 0 1 1 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 1 1 1 0
      ## [133] 1 0 1 0 0 1 1 1 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 1 0 0 1 1 0 0
      ## 
      ## $weights[[199]]
      ##   [1] 1 0 0 1 1 1 0 1 1 1 0 0 0 0 1 0 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 0
      ##  [34] 1 1 0 0 0 1 1 1 1 1 1 1 0 1 1 0 1 1 0 1 0 1 0 0 1 1 1 1 1 1 1 0 1
      ##  [67] 0 0 0 1 1 0 1 0 1 1 1 1 0 1 0 1 0 1 1 1 0 0 0 1 0 1 0 0 1 1 1 1 0
      ## [100] 1 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 0 1 0 1 1 0 0
      ## [133] 1 1 0 0 1 0 0 1 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0
      ## 
      ## $weights[[200]]
      ##   [1] 1 1 1 0 1 0 1 0 1 1 1 0 0 0 0 1 1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 1
      ##  [34] 1 1 1 0 0 1 1 1 1 1 0 1 0 0 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 1 0 1
      ##  [67] 1 0 1 0 0 0 1 0 1 0 1 0 1 0 1 0 0 1 0 1 1 0 0 1 1 1 1 1 1 1 1 1 0
      ## [100] 1 0 1 1 0 0 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 0 0 0 1 1 0 0 0 0 1 0
      ## [133] 1 0 1 0 1 0 1 0 0 1 0 1 1 1 1 1 1 0 0 1 1 1 0 0 1 1 0 1 1 1
      ## 
      ## 
      ## $fitted
      ##     idx (response)
      ## 1     1        218
      ## 2     2        166
      ## 3     3        170
      ## 4     4        567
      ## 5     5        613
      ## 6     6        707
      ## 7     7         61
      ## 8     8        301
      ## 9     9         81
      ## 10   10        371
      ## 11   11        520
      ## 12   12        574
      ## 13   13        118
      ## 14   14        390
      ## 15   15         12
      ## 16   16        473
      ## 17   17         26
      ## 18   18        107
      ## 19   19         53
      ## 20   20        814
      ## 21   21       965+
      ## 22   22         93
      ## 23   23        731
      ## 24   24        460
      ## 25   25        153
      ## 26   26        433
      ## 27   27        583
      ## 28   28         95
      ## 29   29        303
      ## 30   30        519
      ## 31   31        643
      ## 32   32        765
      ## 33   33         53
      ## 34   34        246
      ## 35   35        689
      ## 36   36          5
      ## 37   37        687
      ## 38   38        345
      ## 39   39        444
      ## 40   40        223
      ## 41   41         60
      ## 42   42        163
      ## 43   43         65
      ## 44   44       821+
      ## 45   45        428
      ## 46   46        230
      ## 47   47       840+
      ## 48   48        305
      ## 49   49         11
      ## 50   50        226
      ## 51   51        426
      ## 52   52        705
      ## 53   53        363
      ## 54   54        176
      ## 55   55        791
      ## 56   56         95
      ## 57   57       196+
      ## 58   58        167
      ## 59   59       806+
      ## 60   60        284
      ## 61   61        641
      ## 62   62        147
      ## 63   63       740+
      ## 64   64        163
      ## 65   65        655
      ## 66   66         88
      ## 67   67        245
      ## 68   68         30
      ## 69   69        477
      ## 70   70       559+
      ## 71   71        450
      ## 72   72        156
      ## 73   73       529+
      ## 74   74        429
      ## 75   75        351
      ## 76   76         15
      ## 77   77        181
      ## 78   78        283
      ## 79   79         13
      ## 80   80        212
      ## 81   81        524
      ## 82   82        288
      ## 83   83        363
      ## 84   84        199
      ## 85   85        550
      ## 86   86         54
      ## 87   87        558
      ## 88   88        207
      ## 89   89         92
      ## 90   90         60
      ## 91   91       551+
      ## 92   92        293
      ## 93   93        353
      ## 94   94        267
      ## 95   95       511+
      ## 96   96        457
      ## 97   97        337
      ## 98   98        201
      ## 99   99       404+
      ## 100 100        222
      ## 101 101         62
      ## 102 102       458+
      ## 103 103        353
      ## 104 104        163
      ## 105 105         31
      ## 106 106        229
      ## 107 107        156
      ## 108 108        291
      ## 109 109        179
      ## 110 110       376+
      ## 111 111       384+
      ## 112 112        268
      ## 113 113       292+
      ## 114 114        142
      ## 115 115       413+
      ## 116 116       266+
      ## 117 117        320
      ## 118 118        181
      ## 119 119        285
      ## 120 120       301+
      ## 121 121        348
      ## 122 122        197
      ## 123 123       382+
      ## 124 124       303+
      ## 125 125       296+
      ## 126 126        180
      ## 127 127        145
      ## 128 128       269+
      ## 129 129       300+
      ## 130 130       284+
      ## 131 131       292+
      ## 132 132       332+
      ## 133 133        285
      ## 134 134       259+
      ## 135 135        110
      ## 136 136        286
      ## 137 137        270
      ## 138 138       225+
      ## 139 139        269
      ## 140 140       225+
      ## 141 141       243+
      ## 142 142       276+
      ## 143 143        135
      ## 144 144         79
      ## 145 145         59
      ## 146 146       240+
      ## 147 147       202+
      ## 148 148       235+
      ## 149 149        239
      ## 150 150       252+
      ## 151 151       221+
      ## 152 152       185+
      ## 153 153       222+
      ## 154 154        183
      ## 155 155       211+
      ## 156 156       175+
      ## 157 157       197+
      ## 158 158       203+
      ## 159 159       191+
      ## 160 160       105+
      ## 161 161       174+
      ## 162 162       177+
      ## 
      ## $terms
      ## Surv(time, status) ~ inst + age + sex + ph.ecog + ph.karno + 
      ##     pat.karno + meal.cal + wt.loss
      ## attr(,"variables")
      ## list(Surv(time, status), inst, age, sex, ph.ecog, ph.karno, pat.karno, 
      ##     meal.cal, wt.loss)
      ## attr(,"factors")
      ##                    inst age sex ph.ecog ph.karno pat.karno meal.cal
      ## Surv(time, status)    0   0   0       0        0         0        0
      ## inst                  1   0   0       0        0         0        0
      ## age                   0   1   0       0        0         0        0
      ## sex                   0   0   1       0        0         0        0
      ## ph.ecog               0   0   0       1        0         0        0
      ## ph.karno              0   0   0       0        1         0        0
      ## pat.karno             0   0   0       0        0         1        0
      ## meal.cal              0   0   0       0        0         0        1
      ## wt.loss               0   0   0       0        0         0        0
      ##                    wt.loss
      ## Surv(time, status)       0
      ## inst                     0
      ## age                      0
      ## sex                      0
      ## ph.ecog                  0
      ## ph.karno                 0
      ## pat.karno                0
      ## meal.cal                 0
      ## wt.loss                  1
      ## attr(,"term.labels")
      ## [1] "inst"      "age"       "sex"       "ph.ecog"   "ph.karno" 
      ## [6] "pat.karno" "meal.cal"  "wt.loss"  
      ## attr(,"order")
      ## [1] 1 1 1 1 1 1 1 1
      ## attr(,"intercept")
      ## [1] 1
      ## attr(,"response")
      ## [1] 1
      ## attr(,".Environment")
      ## <environment: 0x56038c9e2aa0>
      ## attr(,"Formula_with_dot")
      ## Surv(time, status) ~ .
      ## <environment: 0x56038c9e2aa0>
      ## attr(,"Formula_without_dot")
      ## Surv(time, status) ~ inst + age + sex + ph.ecog + ph.karno + 
      ##     pat.karno + meal.cal + wt.loss
      ## <environment: 0x56038c9e2aa0>
      ## attr(,"dot")
      ## [1] "sequential"
      ## 
      ## $info
      ## $info$call
      ## partykit::cforest(formula = formula, data = data, weights = weights, 
      ##     control = partykit::ctree_control(minsplit = 20L, maxdepth = Inf, 
      ##         teststat = "quadratic", testtype = "Univariate", mincriterion = 0, 
      ##         saveinfo = FALSE), ntree = 200, mtry = 3)
      ## 
      ## $info$control
      ## $info$control$criterion
      ## [1] "p.value"
      ## 
      ## $info$control$logmincriterion
      ## [1] -Inf
      ## 
      ## $info$control$minsplit
      ## [1] 20
      ## 
      ## $info$control$minbucket
      ## [1] 7
      ## 
      ## $info$control$minprob
      ## [1] 0.01
      ## 
      ## $info$control$maxvar
      ## [1] Inf
      ## 
      ## $info$control$stump
      ## [1] FALSE
      ## 
      ## $info$control$nmax
      ##  yx   z 
      ## Inf Inf 
      ## 
      ## $info$control$lookahead
      ## [1] FALSE
      ## 
      ## $info$control$mtry
      ## [1] 3
      ## 
      ## $info$control$maxdepth
      ## [1] Inf
      ## 
      ## $info$control$multiway
      ## [1] FALSE
      ## 
      ## $info$control$splittry
      ## [1] 2
      ## 
      ## $info$control$maxsurrogate
      ## [1] 0
      ## 
      ## $info$control$numsurrogate
      ## [1] FALSE
      ## 
      ## $info$control$majority
      ## [1] FALSE
      ## 
      ## $info$control$caseweights
      ## [1] TRUE
      ## 
      ## $info$control$applyfun
      ## function (X, FUN, ...) 
      ## {
      ##     FUN <- match.fun(FUN)
      ##     if (!is.vector(X) || is.object(X)) 
      ##         X <- as.list(X)
      ##     .Internal(lapply(X, FUN))
      ## }
      ## <bytecode: 0x56038163dda0>
      ## <environment: namespace:base>
      ## 
      ## $info$control$saveinfo
      ## [1] FALSE
      ## 
      ## $info$control$bonferroni
      ## [1] FALSE
      ## 
      ## $info$control$update
      ## [1] FALSE
      ## 
      ## $info$control$selectfun
      ## function (model, trafo, data, subset, weights, whichvar, ctrl) 
      ## {
      ##     args <- list(...)
      ##     ctrl[names(args)] <- args
      ##     .select(model, trafo, data, subset, weights, whichvar, ctrl, 
      ##         FUN = .ctree_test)
      ## }
      ## <bytecode: 0x56038a257690>
      ## <environment: 0x56038c994fd8>
      ## 
      ## $info$control$splitfun
      ## function (model, trafo, data, subset, weights, whichvar, ctrl) 
      ## {
      ##     args <- list(...)
      ##     ctrl[names(args)] <- args
      ##     .split(model, trafo, data, subset, weights, whichvar, ctrl, 
      ##         FUN = .ctree_test)
      ## }
      ## <bytecode: 0x56038a255888>
      ## <environment: 0x56038c9950b8>
      ## 
      ## $info$control$svselectfun
      ## function (model, trafo, data, subset, weights, whichvar, ctrl) 
      ## {
      ##     args <- list(...)
      ##     ctrl[names(args)] <- args
      ##     .select(model, trafo, data, subset, weights, whichvar, ctrl, 
      ##         FUN = .ctree_test)
      ## }
      ## <bytecode: 0x56038a257690>
      ## <environment: 0x56038c995198>
      ## 
      ## $info$control$svsplitfun
      ## function (model, trafo, data, subset, weights, whichvar, ctrl) 
      ## {
      ##     args <- list(...)
      ##     ctrl[names(args)] <- args
      ##     .split(model, trafo, data, subset, weights, whichvar, ctrl, 
      ##         FUN = .ctree_test)
      ## }
      ## <bytecode: 0x56038a255888>
      ## <environment: 0x56038c9952e8>
      ## 
      ## $info$control$teststat
      ## [1] "quadratic"
      ## 
      ## $info$control$splitstat
      ## [1] "quadratic"
      ## 
      ## $info$control$splittest
      ## [1] FALSE
      ## 
      ## $info$control$pargs
      ## $maxpts
      ## [1] 25000
      ## 
      ## $abseps
      ## [1] 0.001
      ## 
      ## $releps
      ## [1] 0
      ## 
      ## attr(,"class")
      ## [1] "GenzBretz"
      ## 
      ## $info$control$testtype
      ## [1] "Univariate"
      ## 
      ## $info$control$nresample
      ## [1] 9999
      ## 
      ## $info$control$tol
      ## [1] 1.490116e-08
      ## 
      ## $info$control$intersplit
      ## [1] FALSE
      ## 
      ## $info$control$MIA
      ## [1] FALSE
      ## 
      ## 
      ## 
      ## $trafo
      ## function (subset, weights, info, estfun, object, ...) 
      ## list(estfun = Y, unweighted = TRUE)
      ## <bytecode: 0x56039271ccf0>
      ## <environment: 0x56038c993128>
      ## 
      ## $predictf
      ## ~inst + age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + 
      ##     wt.loss
      ## attr(,"variables")
      ## list(inst, age, sex, ph.ecog, ph.karno, pat.karno, meal.cal, 
      ##     wt.loss)
      ## attr(,"factors")
      ##           inst age sex ph.ecog ph.karno pat.karno meal.cal wt.loss
      ## inst         1   0   0       0        0         0        0       0
      ## age          0   1   0       0        0         0        0       0
      ## sex          0   0   1       0        0         0        0       0
      ## ph.ecog      0   0   0       1        0         0        0       0
      ## ph.karno     0   0   0       0        1         0        0       0
      ## pat.karno    0   0   0       0        0         1        0       0
      ## meal.cal     0   0   0       0        0         0        1       0
      ## wt.loss      0   0   0       0        0         0        0       1
      ## attr(,"term.labels")
      ## [1] "inst"      "age"       "sex"       "ph.ecog"   "ph.karno" 
      ## [6] "pat.karno" "meal.cal"  "wt.loss"  
      ## attr(,"order")
      ## [1] 1 1 1 1 1 1 1 1
      ## attr(,"intercept")
      ## [1] 1
      ## attr(,"response")
      ## [1] 0
      ## attr(,".Environment")
      ## <environment: 0x56038c9e2aa0>
      ## attr(,"Formula_with_dot")
      ## Surv(time, status) ~ .
      ## <environment: 0x56038c9e2aa0>
      ## attr(,"Formula_without_dot")
      ## Surv(time, status) ~ inst + age + sex + ph.ecog + ph.karno + 
      ##     pat.karno + meal.cal + wt.loss
      ## <environment: 0x56038c9e2aa0>
      ## attr(,"dot")
      ## [1] "sequential"
      ## 
      ## attr(,"class")
      ## [1] "cforest"      "constparties" "parties"

The holdout data can be predicted for survival probability at different
time points as well as event time.

``` r

  predict(
    rf_fit, 
    lung_test, 
    type = "survival", 
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.886 
      ## 2        500         0.303 
      ## 3       1000         0.0443

``` r

  predict(rf_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1        337
      ## 2        267
      ## 3        230
      ## 4        201
      ## 5        226

With the `"aorsf"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  rf_spec <- 
    rand_forest(trees = 200) |>
    set_engine("aorsf") |> 
    set_mode("censored regression") 
  rf_spec
```

      ## Random Forest Model Specification (censored regression)
      ## 
      ## Main Arguments:
      ##   trees = 200
      ## 
      ## Computational engine: aorsf

Now we create the model fit object:

``` r

  set.seed(1)
  
  rf_fit <- rf_spec |> fit(Surv(time, status) ~ ., data = lung_train)
  rf_fit
```

      ## parsnip model object
      ## 
      ## ---------- Oblique random survival forest
      ## 
      ##      Linear combinations: Accelerated Cox regression
      ##           N observations: 162
      ##                 N events: 278
      ##                  N trees: 200
      ##       N predictors total: 8
      ##    N predictors per node: 3
      ##  Average leaves per tree: 17.175
      ## Min observations in leaf: 5
      ##       Min events in leaf: 1
      ##           OOB stat value: 0.62
      ##            OOB stat type: Harrell's C-index
      ##      Variable importance: anova
      ## 
      ## -----------------------------------------

The holdout data can be predicted for survival probability at different
time points as well as event time.

``` r

  predict(
    rf_fit, 
    lung_test, 
    type = "survival", 
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.917 
      ## 2        500         0.397 
      ## 3       1000         0.0538

``` r

  predict(rf_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1       397.
      ## 2       277.
      ## 3       240.
      ## 4       228.
      ## 5       226.

## `survival_reg()` models

With the `"survival"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  sr_spec <- 
    survival_reg(dist = "weibull") |>
    set_engine("survival") |> 
    set_mode("censored regression") 
  sr_spec
```

      ## Parametric Survival Regression Model Specification (censored regression)
      ## 
      ## Main Arguments:
      ##   dist = weibull
      ## 
      ## Computational engine: survival

Now we create the model fit object:

``` r

  set.seed(1)
  sr_fit <- sr_spec |> fit(Surv(time, status) ~ ., data = lung_train)
  sr_fit
```

      ## parsnip model object
      ## 
      ## Call:
      ## survival::survreg(formula = Surv(time, status) ~ ., data = data, 
      ##     dist = ~"weibull", model = TRUE)
      ## 
      ## Coefficients:
      ##   (Intercept)          inst           age           sex       ph.ecog 
      ##  6.2802499155  0.0191302849 -0.0085917372  0.4249655608 -0.5022975982 
      ##      ph.karno     pat.karno      meal.cal       wt.loss 
      ## -0.0085852225  0.0058753359  0.0001003211  0.0127001420 
      ## 
      ## Scale= 0.6902035 
      ## 
      ## Loglik(model)= -795.2   Loglik(intercept only)= -811.4
      ##    Chisq= 32.41 on 8 degrees of freedom, p= 7.85e-05 
      ## n= 162

The holdout data can be predicted for survival probability at different
time points as well as event time, linear predictor, quantile, and
hazard.

``` r

  predict(
    sr_fit, 
    lung_test, 
    type = "survival",
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.912 
      ## 2        500         0.386 
      ## 3       1000         0.0742

``` r

  predict(sr_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1       517.
      ## 2       283.
      ## 3       361.
      ## 4       268.
      ## 5       313.

``` r

  predict(sr_fit, lung_test, type = "linear_pred")
```

      ## # A tibble: 5 × 1
      ##   .pred_linear_pred
      ##               <dbl>
      ## 1              6.25
      ## 2              5.64
      ## 3              5.89
      ## 4              5.59
      ## 5              5.75

``` r

  predict(sr_fit, lung_test, type = "quantile")
```

      ## # A tibble: 5 × 1
      ##   .pred_quantile
      ##        <qtls(9)>
      ## 1          [401]
      ## 2          [219]
      ## 3          [280]
      ## 4          [208]
      ## 5          [243]

``` r

  predict(sr_fit, lung_test, type = "hazard", eval_time = c(100, 500, 1000)) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_hazard
      ##        <dbl>        <dbl>
      ## 1        100      0.00134
      ## 2        500      0.00276
      ## 3       1000      0.00377

With the `"flexsurv"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model with specific parameters:

``` r

  sr_spec <- 
    survival_reg(dist = "weibull") |>
    set_engine("flexsurv") |> 
    set_mode("censored regression") 
  sr_spec
```

      ## Parametric Survival Regression Model Specification (censored regression)
      ## 
      ## Main Arguments:
      ##   dist = weibull
      ## 
      ## Computational engine: flexsurv

Now we create the model fit object:

``` r

  set.seed(1)
  sr_fit <- sr_spec |> 
    fit(Surv(time, status) ~ age + sex + ph.ecog, data = lung_train)
  sr_fit
```

      ## parsnip model object
      ## 
      ## Call:
      ## flexsurv::flexsurvreg(formula = Surv(time, status) ~ age + sex + 
      ##     ph.ecog, data = data, dist = ~"weibull")
      ## 
      ## Estimates: 
      ##          data mean  est        L95%       U95%       se       
      ## shape           NA   1.39e+00   1.21e+00   1.61e+00   1.02e-01
      ## scale           NA   5.74e+02   1.99e+02   1.65e+03   3.10e+02
      ## age       6.24e+01  -9.02e-03  -2.50e-02   6.95e-03   8.15e-03
      ## sex       1.38e+00   4.02e-01   1.17e-01   6.87e-01   1.45e-01
      ## ph.ecog   9.51e-01  -3.17e-01  -5.13e-01  -1.21e-01   1.00e-01
      ##          exp(est)   L95%       U95%     
      ## shape           NA         NA         NA
      ## scale           NA         NA         NA
      ## age       9.91e-01   9.75e-01   1.01e+00
      ## sex       1.50e+00   1.12e+00   1.99e+00
      ## ph.ecog   7.28e-01   5.99e-01   8.86e-01
      ## 
      ## N = 162,  Events: 116,  Censored: 46
      ## Total time at risk: 49401
      ## Log-likelihood = -800.356, df = 5
      ## AIC = 1610.712

The holdout data can be predicted for survival probability at different
time points as well as event time, linear predictor, quantile, and
hazard.

``` r

  predict(
    sr_fit, 
    lung_test, 
    type = "survival",
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.889 
      ## 2        500         0.330 
      ## 3       1000         0.0543

``` r

  predict(sr_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1       424.
      ## 2       341.
      ## 3       292.
      ## 4       336.
      ## 5       327.

``` r

  predict(sr_fit, lung_test, type = "linear_pred")
```

      ## # A tibble: 5 × 1
      ##   .pred_linear_pred
      ##               <dbl>
      ## 1              6.14
      ## 2              5.92
      ## 3              5.77
      ## 4              5.91
      ## 5              5.88

``` r

  predict(sr_fit, lung_test, type = "quantile")
```

      ## # A tibble: 5 × 1
      ##   .pred_quantile
      ##        <qtls(9)>
      ## 1          [357]
      ## 2          [287]
      ## 3          [246]
      ## 4          [283]
      ## 5          [276]

``` r

  predict(sr_fit, lung_test, type = "hazard", eval_time = c(100, 500, 1000)) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_hazard
      ##        <dbl>        <dbl>
      ## 1        100      0.00164
      ## 2        500      0.00309
      ## 3       1000      0.00406

With the `"flexsurvspline"` engine

We’ll model the survival of lung cancer patients.

``` r

  library(tidymodels)
  library(censored)
  tidymodels_prefer()
  
  data(cancer)
  
  lung <- lung %>% drop_na()
  lung_train <- lung[-c(1:5), ]
  lung_test <- lung[1:5, ]
```

We can define the model:

``` r

  sr_spec <- 
    survival_reg() |>
    set_engine("flexsurvspline") |> 
    set_mode("censored regression") 
  sr_spec
```

      ## Parametric Survival Regression Model Specification (censored regression)
      ## 
      ## Computational engine: flexsurvspline

Now we create the model fit object:

``` r

  set.seed(1)
  sr_fit <- sr_spec |> 
    fit(Surv(time, status) ~ age + sex + ph.ecog, data = lung_train)
  sr_fit
```

      ## parsnip model object
      ## 
      ## Call:
      ## flexsurv::flexsurvspline(formula = Surv(time, status) ~ age + 
      ##     sex + ph.ecog, data = data)
      ## 
      ## Estimates: 
      ##          data mean  est        L95%       U95%       se       
      ## gamma0          NA   -8.85681  -10.78595   -6.92767    0.98427
      ## gamma1          NA    1.39431    1.19358    1.59504    0.10241
      ## age       62.41358    0.01258   -0.00968    0.03484    0.01136
      ## sex        1.38272   -0.56080   -0.95517   -0.16643    0.20121
      ## ph.ecog    0.95062    0.44213    0.17197    0.71230    0.13784
      ##          exp(est)   L95%       U95%     
      ## gamma0          NA         NA         NA
      ## gamma1          NA         NA         NA
      ## age        1.01266    0.99037    1.03545
      ## sex        0.57075    0.38475    0.84668
      ## ph.ecog    1.55602    1.18764    2.03867
      ## 
      ## N = 162,  Events: 116,  Censored: 46
      ## Total time at risk: 49401
      ## Log-likelihood = -800.356, df = 5
      ## AIC = 1610.712

The holdout data can be predicted for survival probability at different
time points as well as event time, linear predictor, quantile, and
hazard.

``` r

  predict(
    sr_fit, 
    lung_test, 
    type = "survival",
    eval_time = c(100, 500, 1000)
  ) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_survival
      ##        <dbl>          <dbl>
      ## 1        100         0.889 
      ## 2        500         0.330 
      ## 3       1000         0.0543

``` r

  predict(sr_fit, lung_test, type = "time")
```

      ## # A tibble: 5 × 1
      ##   .pred_time
      ##        <dbl>
      ## 1       424.
      ## 2       341.
      ## 3       292.
      ## 4       336.
      ## 5       327.

``` r

  predict(sr_fit, lung_test, type = "linear_pred")
```

      ## # A tibble: 5 × 1
      ##   .pred_linear_pred
      ##               <dbl>
      ## 1             -8.56
      ## 2             -8.26
      ## 3             -8.04
      ## 4             -8.24
      ## 5             -8.20

``` r

  predict(sr_fit, lung_test, type = "quantile")
```

      ## # A tibble: 5 × 1
      ##   .pred_quantile
      ##        <qtls(9)>
      ## 1          [357]
      ## 2          [287]
      ## 3          [246]
      ## 4          [283]
      ## 5          [276]

``` r

  predict(sr_fit, lung_test, type = "hazard", eval_time = c(100, 500, 1000)) |> 
    slice(1) |> 
    tidyr::unnest(col = .pred)
```

      ## # A tibble: 3 × 2
      ##   .eval_time .pred_hazard
      ##        <dbl>        <dbl>
      ## 1        100      0.00164
      ## 2        500      0.00309
      ## 3       1000      0.00406
