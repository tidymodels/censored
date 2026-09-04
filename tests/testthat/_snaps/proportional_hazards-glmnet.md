# print coxnet model

    Code
      f_fit
    Output
      parsnip model object
      
      
      Call:  glmnet::glmnet(x = data_obj$x, y = data_obj$y, family = "cox",      weights = weights, alpha = alpha, lambda = lambda, cox.ties = ..1) 
      
         Df %Dev   Lambda
      1   0 0.00 0.225800
      2   1 0.20 0.205800
      3   1 0.38 0.187500
      4   1 0.52 0.170800
      5   1 0.64 0.155600
      6   1 0.74 0.141800
      7   1 0.82 0.129200
      8   1 0.88 0.117700
      9   1 0.94 0.107300
      10  1 0.99 0.097750
      11  1 1.02 0.089070
      12  2 1.05 0.081150
      13  2 1.08 0.073940
      14  2 1.10 0.067370
      15  2 1.13 0.061390
      16  2 1.16 0.055940
      17  2 1.18 0.050970
      18  2 1.20 0.046440
      19  2 1.21 0.042310
      20  2 1.23 0.038550
      21  2 1.24 0.035130
      22  2 1.25 0.032010
      23  2 1.26 0.029160
      24  2 1.27 0.026570
      25  2 1.27 0.024210
      26  2 1.28 0.022060
      27  2 1.28 0.020100
      28  2 1.28 0.018320
      29  2 1.29 0.016690
      30  2 1.29 0.015210
      31  2 1.29 0.013860
      32  2 1.30 0.012620
      33  2 1.30 0.011500
      34  2 1.30 0.010480
      35  2 1.30 0.009550
      36  2 1.30 0.008702
      37  2 1.30 0.007929
      38  2 1.30 0.007224
      39  2 1.30 0.006583
      40  2 1.30 0.005998
      41  2 1.31 0.005465
      42  2 1.31 0.004979
      43  2 1.31 0.004537
      44  2 1.31 0.004134
      45  2 1.31 0.003767
      46  2 1.31 0.003432
      47  2 1.31 0.003127
      48  2 1.31 0.002849
      49  2 1.31 0.002596
      50  2 1.31 0.002366
      The training data has been saved for prediction.

# stratification is specified in a single term

    Code
      fit(spec, Surv(time, status) ~ age + ph.ecog + strata(sex) + strata(inst),
      data = lung)
    Condition
      Error:
      ! There can only be a single strata term specified using the `strata()` function.
      i It can contain multiple strata columns, e.g., `~ x + strata(s1, s2)`.

# formula modifications to remove strata

    Code
      check_strata_remaining(rlang::expr(x * (y + strata(s)) + z))
    Condition
      Error:
      ! Stratification must be nested under a chain of `+` calls.
      i # Good: `~ x1 + x2 + strata(s)`
      i # Bad: `~ x1 + (x2 + strata(s))`

---

    Code
      fit(spec, Surv(time, status) ~ strata(sex), data = lung)
    Condition
      Error:
      ! The Cox model does not contain an intercept, please add a predictor.

---

    Code
      fit(spec, Surv(time, status) ~ age + (ph.ecog + strata(sex)), data = lung)
    Condition
      Error:
      ! Stratification must be nested under a chain of `+` calls.
      i # Good: `~ x1 + x2 + strata(s)`
      i # Bad: `~ x1 + (x2 + strata(s))`

# protect certain glmnet engine args

    Code
      fit(set_engine(proportional_hazards(penalty = 0.1), "glmnet", family = "gaussian"),
      Surv(time, status) ~ age + sex, data = lung)
    Condition
      Error:
      ! This argument cannot be used to create the model: `family`.

# predictions with strata and dot in formula

    Code
      f_fit <- fit(cox_spec, Surv(time, status) ~ . - sex + strata(sex), data = lung2)

---

    Code
      f_fit_2 <- fit(cox_spec, Surv(time, status) ~ ph.ecog + age + strata(sex),
      data = lung2)

---

    Code
      predict(f_fit, lung2, type = "linear_pred")
    Output
      # A tibble: 227 x 1
         .pred_linear_pred
                     <dbl>
       1            -1.07 
       2            -0.562
       3            -0.462
       4            -0.929
       5            -0.495
       6            -1.07 
       7            -1.48 
       8            -1.50 
       9            -0.896
      10            -1.42 
      # i 217 more rows
    Code
      predict(f_fit, lung2, type = "survival", eval_time = c(100, 300))
    Condition
      Warning in `terms.formula()`:
      'varlist' has changed (from nvar=5) to new 6 after EncodeVars() -- should no longer happen!
    Output
      # A tibble: 227 x 1
         .pred           
         <list>          
       1 <tibble [2 x 2]>
       2 <tibble [2 x 2]>
       3 <tibble [2 x 2]>
       4 <tibble [2 x 2]>
       5 <tibble [2 x 2]>
       6 <tibble [2 x 2]>
       7 <tibble [2 x 2]>
       8 <tibble [2 x 2]>
       9 <tibble [2 x 2]>
      10 <tibble [2 x 2]>
      # i 217 more rows

---

    Code
      f_pred <- predict(f_fit, lung2, type = "survival", eval_time = c(100, 300))
    Condition
      Warning in `terms.formula()`:
      'varlist' has changed (from nvar=5) to new 6 after EncodeVars() -- should no longer happen!
    Code
      f_pred_2 <- predict(f_fit_2, lung2, type = "survival", eval_time = c(100, 300))

# `fit_xy()` errors with stratification

    Code
      fit_xy(spec, x = lung_x, y = lung_y_s)
    Condition
      Error in `fit_xy()`:
      ! For stratification, please use the formula interface via `fit()`.

