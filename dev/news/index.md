# Changelog

## censored (development version)

- [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  with the `"mboost"` engine no longer accepts case weights because
  mboost cannot predict from a weighted fit
  ([\#363](https://github.com/tidymodels/censored/issues/363)).

- [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  now supports the `"censored regression"` mode with a new `"ranger"`
  engine, fitting a survival random forest via
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  ([\#131](https://github.com/tidymodels/censored/issues/131)).

- [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  now supports the `"randomForestSRC"` engine for censored regression,
  fitting a survival random forest via
  [`randomForestSRC::rfsrc()`](https://www.randomforestsrc.org//reference/rfsrc.html)
  ([\#130](https://github.com/tidymodels/censored/issues/130)).

- [`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.html)
  is now available for
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  with the `"mboost"` engine over the `trees` submodel parameter
  ([\#290](https://github.com/tidymodels/censored/issues/290)).

- [`decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
  with the `"rpart"` engine now correctly returns the median survival
  time of the leaf’s Kaplan-Meier curve for `type = "time"` predictions,
  instead of rpart’s relative event rate
  ([\#331](https://github.com/tidymodels/censored/issues/331)).

- [`null_model()`](https://parsnip.tidymodels.org/reference/null_model.html)
  now supports the `"censored regression"` mode with a new `"survival"`
  engine, fitting a Kaplan-Meier curve via
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
  ([\#353](https://github.com/tidymodels/censored/issues/353)).

- Prediction for
  [`proportional_hazards()`](https://parsnip.tidymodels.org/reference/proportional_hazards.html)
  with the `"glmnet"` engine no longer fails on data with factors when
  fitted through
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
  ([\#365](https://github.com/tidymodels/censored/issues/365)).

- The `survival_prob_*()` and `hazard_*()` helpers now validate their
  inputs and return more informative error messages when given an
  unusable `object`, `new_data`, or `eval_time`
  ([\#271](https://github.com/tidymodels/censored/issues/271)).

- [`survival_reg()`](https://parsnip.tidymodels.org/reference/survival_reg.html)
  with the `"survival"` engine and
  [`strata()`](https://rdrr.io/pkg/survival/man/strata.html) terms now
  returns `NA` survival and hazard predictions for new data rows with a
  missing value in a stratification variable, instead of silently using
  another stratum’s scale
  ([\#383](https://github.com/tidymodels/censored/issues/383)).

## censored 0.3.4

CRAN release: 2026-04-04

- Adapted tests for hardhat 1.4.3
  ([\#358](https://github.com/tidymodels/censored/issues/358)).

## censored 0.3.3

CRAN release: 2025-02-14

### Breaking change

- The format of quantile predictions now follows the new requirements in
  parsnip ([\#339](https://github.com/tidymodels/censored/issues/339),
  tidymodels/parsnip/#1209).

## censored 0.3.2

CRAN release: 2024-06-11

- censored now depends on survival \>= 3.7-0 which allows us to use it
  also for predictions of survival probabilities at infinite evaluation
  time points. This means that: Survival probabilities at
  `eval_time = Inf` are now not always set to 0 and confidence intervals
  at infinite evaluation times are now not always set to `NA`. This
  applies to
  [`proportional_hazards()`](https://parsnip.tidymodels.org/reference/proportional_hazards.html)and
  [`bag_tree()`](https://parsnip.tidymodels.org/reference/bag_tree.html)
  models as well as models with the `partykit` engine,
  [`decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
  and
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  ([\#320](https://github.com/tidymodels/censored/issues/320)).

## censored 0.3.1

CRAN release: 2024-04-19

- Internal changes to the
  [`predict()`](https://rdrr.io/r/stats/predict.html) methods for
  flexsurv models, in preparation for the upcoming flexsurv release
  ([\#317](https://github.com/tidymodels/censored/issues/317)).

## censored 0.3.0

CRAN release: 2024-01-31

### New features

- [`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.html)
  is now available for all prediction types for
  [`proportional_hazards()`](https://parsnip.tidymodels.org/reference/proportional_hazards.html)
  models with the `"glmnet"` engine, so newly also for `type = "time"`
  and `type = "raw"`
  ([\#277](https://github.com/tidymodels/censored/issues/277),
  [\#282](https://github.com/tidymodels/censored/issues/282)).

- Random forests with the `"aorsf"` engine can now predict survival
  time, i.e., `predict(type = "time")` is now available
  ([\#308](https://github.com/tidymodels/censored/issues/308)).

### Breaking change

- The `survival_prob_*()`, `survival_time_*()`, and `hazard_*()` helper
  functions now all take a parsnip `model_fit` object as the main input,
  instead of an engine fit as was the case for some of them previously
  ([\#302](https://github.com/tidymodels/censored/issues/302)).

### Bug fixes

- [`extract_fit_engine()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  now works properly for proportional hazards models fitted with the
  `"glmnet"` engine
  ([\#266](https://github.com/tidymodels/censored/issues/266)).

- `multi_predict(type = "survival")` for
  `proportional_hazards(engine = "glmnet")` models: when used with a
  single `penalty` value, this value is now included in the results. It
  was previously omitted
  ([\#267](https://github.com/tidymodels/censored/issues/267),
  [\#282](https://github.com/tidymodels/censored/issues/282)).

- `proportional_hazards(engine = "glmnet")` models now don’t pretend to
  be able to deal with sparse matrices when they are not
  ([\#291](https://github.com/tidymodels/censored/issues/291)).

- Fixed a bug for `proportional_hazards(engine = "glmnet")` where
  prediction didn’t work for a `workflow()` with a formula as the
  preprocessor
  ([\#264](https://github.com/tidymodels/censored/issues/264)).

### Other

- The helper functions
  [`survival_time_coxnet()`](https://censored.tidymodels.org/dev/reference/survival_time_coxnet.md)
  and
  [`survival_prob_coxnet()`](https://censored.tidymodels.org/dev/reference/survival_prob_coxnet.md)
  gain a `multi` argument to allow multiple values for `penalty`
  ([\#278](https://github.com/tidymodels/censored/issues/278),
  [\#279](https://github.com/tidymodels/censored/issues/279)).

## censored 0.2.0

CRAN release: 2023-04-13

### Cross-package changes with parsnip

- The new `eval_time` argument replaces the `time` argument for the time
  points at which to predict survival probability and hazard. The `time`
  argument has been deprecated
  ([\#244](https://github.com/tidymodels/censored/issues/244)).

- The matrix interface for fitting,
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html), now
  works for censored regression models
  ([\#225](https://github.com/tidymodels/censored/issues/225),
  [\#234](https://github.com/tidymodels/censored/issues/234),
  [\#247](https://github.com/tidymodels/censored/issues/247),
  [\#251](https://github.com/tidymodels/censored/issues/251)).

- Improved error messages throughout the package
  ([\#248](https://github.com/tidymodels/censored/issues/248)).

### New engines

- Added the new `"aorsf"` engine for
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  for accelerated oblique random survival forests with the aorsf package
  ([@bcjaeger](https://github.com/bcjaeger),
  [\#211](https://github.com/tidymodels/censored/issues/211)).

- Added the new `flexsurvspline` engine for
  [`survival_reg()`](https://parsnip.tidymodels.org/reference/survival_reg.html)
  ([@mattwarkentin](https://github.com/mattwarkentin),
  [\#213](https://github.com/tidymodels/censored/issues/213)).

### Bug fixes

- Predictions of type `"linear_pred"` for
  `survival_reg(engine = "flexsurv")` are now on the correct scale for
  distributions where the natural scale and the unrestricted scale of
  the location parameter are identical, e.g. `dist = "lnorm"`
  ([\#229](https://github.com/tidymodels/censored/issues/229)).

- Predictions of type `"linear_pred"` for
  `proportional_hazards(engine = "glmnet")` via
  [`multi_predict()`](https://parsnip.tidymodels.org/reference/multi_predict.html)
  now have the same sign as those via
  [`predict()`](https://rdrr.io/r/stats/predict.html)
  ([\#242](https://github.com/tidymodels/censored/issues/242)).

- Predictions of survival probability for
  `survival_reg(engine = "flexsurv")` for a single time point are now
  nested correctly
  ([\#254](https://github.com/tidymodels/censored/issues/254)).

- Predictions of survival probability for
  `decision_tree(engine = "rpart")` for a single observation now work
  ([\#256](https://github.com/tidymodels/censored/issues/256)).

- Predictions of type `"quantile"` for
  `survival_reg(engine = "survival")` for a single observation now work
  ([\#257](https://github.com/tidymodels/censored/issues/257)).

- Fixed a bug for printing `coxnet` models, i.e.,
  [`proportional_hazards()`](https://parsnip.tidymodels.org/reference/proportional_hazards.html)
  models fitted with the `"glmnet"` engine
  ([\#249](https://github.com/tidymodels/censored/issues/249)).

### Internal changes

- Predictions of survival probabilities are now calculated via
  `summary.survfit()` for
  [`proportional_hazards()`](https://parsnip.tidymodels.org/reference/proportional_hazards.html)
  models with the `"survival"` and `"glmnet"` engines,
  [`bag_tree()`](https://parsnip.tidymodels.org/reference/bag_tree.html)
  models with the `"rpart"` engine,
  [`decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
  models with the `"partykit"` engines, as well as
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  models with the `"partykit"` engine
  ([\#221](https://github.com/tidymodels/censored/issues/221),
  [\#224](https://github.com/tidymodels/censored/issues/224)).

- Added internal `survfit_summary_*()` helper functions
  ([\#216](https://github.com/tidymodels/censored/issues/216)).

## censored 0.1.1

CRAN release: 2022-09-30

- For boosted trees with the `"mboost"` engine, survival probabilities
  can now be predicted for `time = -Inf`. This is always 1. For
  `time = Inf` this now predicts a survival probability of 0
  ([\#215](https://github.com/tidymodels/censored/issues/215)).

- Updated tests on model arguments and
  [`update()`](https://rdrr.io/r/stats/update.html) methods
  ([\#208](https://github.com/tidymodels/censored/issues/208)).

- Internal re-organisation of code
  ([\#206](https://github.com/tidymodels/censored/issues/206), 209).

- Added a `NEWS.md` file to track changes to the package.
