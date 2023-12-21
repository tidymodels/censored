# print coxnet model

    Code
      f_fit
    Output
      parsnip model object
      
      Fit time:  NA 
      
      Call:  glmnet::glmnet(x = data_obj$x, y = data_obj$y, family = "cox",      weights = weights, alpha = alpha, lambda = lambda) 
      
         Df %Dev   Lambda
      1   0 0.00 0.225300
      2   1 0.21 0.205300
      3   1 0.39 0.187100
      4   1 0.53 0.170500
      5   1 0.65 0.155300
      6   1 0.75 0.141500
      7   1 0.84 0.128900
      8   1 0.90 0.117500
      9   1 0.96 0.107000
      10  1 1.01 0.097540
      11  1 1.05 0.088870
      12  2 1.08 0.080980
      13  2 1.13 0.073790
      14  2 1.16 0.067230
      15  2 1.19 0.061260
      16  2 1.22 0.055820
      17  2 1.24 0.050860
      18  2 1.26 0.046340
      19  2 1.27 0.042220
      20  2 1.28 0.038470
      21  2 1.29 0.035050
      22  2 1.30 0.031940
      23  2 1.31 0.029100
      24  2 1.31 0.026520
      25  2 1.32 0.024160
      26  2 1.32 0.022010
      27  2 1.33 0.020060
      28  2 1.33 0.018280
      29  2 1.33 0.016650
      30  2 1.33 0.015170
      31  2 1.33 0.013830
      32  2 1.33 0.012600
      33  2 1.34 0.011480
      34  2 1.34 0.010460
      35  2 1.34 0.009530
      36  2 1.34 0.008683
      37  2 1.34 0.007912
      38  2 1.34 0.007209
      39  2 1.34 0.006568
      40  2 1.34 0.005985
      41  2 1.34 0.005453
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
      proportional_hazards(penalty = 0.1) %>% set_engine("glmnet", family = "gaussian") %>%
        fit(Surv(time, status) ~ age + sex, data = lung)
    Condition
      Error:
      ! This argument cannot be used to create the model: `family`.

# predictions with strata and dot in formula

    Code
      f_fit <- fit(cox_spec, Surv(time, status) ~ . - sex + strata(sex), data = lung2)
    Condition
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge

---

    Code
      f_fit_2 <- fit(cox_spec, Surv(time, status) ~ ph.ecog + age + strata(sex),
      data = lung2)
    Condition
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge
      Warning:
      cox.fit: algorithm did not converge

---

    Code
      predict(f_fit, lung2, type = "linear_pred")
    Output
      # A tibble: 227 x 1
         .pred_linear_pred
                     <dbl>
       1            -1.09 
       2            -0.579
       3            -0.477
       4            -0.940
       5            -0.511
       6            -1.09 
       7            -1.49 
       8            -1.51 
       9            -0.906
      10            -1.43 
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

