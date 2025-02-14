# censored 0.3.3

## Breaking change

* The format of quantile predictions now follows the new requirements in parsnip (#339, tidymodels/parsnip/#1209).


# censored 0.3.2

* censored now depends on survival >= 3.7-0 which allows us to use it also for predictions of survival probabilities at infinite evaluation time points. This means that: Survival probabilities at `eval_time = Inf` are now not always set to 0 and confidence intervals at infinite evaluation times are now not always set to `NA`. This applies to `proportional_hazards()`and `bag_tree()` models as well as models with the `partykit` engine, `decision_tree()` and `rand_forest()` (#320).


# censored 0.3.1

* Internal changes to the `predict()` methods for flexsurv models, in preparation for the upcoming flexsurv release (#317).


# censored 0.3.0

## New features

* `multi_predict()` is now available for all prediction types for `proportional_hazards()` models with the `"glmnet"` engine, so newly also for `type = "time"` and `type = "raw"` (#277, #282).

* Random forests with the `"aorsf"` engine can now predict survival time, i.e., `predict(type = "time")` is now available (#308).

## Breaking change

* The `survival_prob_*()`, `survival_time_*()`, and `hazard_*()` helper functions now all take a parsnip `model_fit` object as the main input, instead of an engine fit as was the case for some of them previously (#302).

## Bug fixes

* `extract_fit_engine()` now works properly for proportional hazards models fitted with the `"glmnet"` engine (#266).

* `multi_predict(type = "survival")` for `proportional_hazards(engine = "glmnet")` models: when used with a single `penalty` value, this value is now included in the results. It was previously omitted (#267, #282).

* `proportional_hazards(engine = "glmnet")` models now don't pretend to be able to deal with sparse matrices when they are not (#291).

* Fixed a bug for `proportional_hazards(engine = "glmnet")` where prediction didn't work for a `workflow()` with a formula as the preprocessor (#264).

## Other

* The helper functions `survival_time_coxnet()` and `survival_prob_coxnet()` gain a `multi` argument to allow multiple values for `penalty` (#278, #279).


# censored 0.2.0

## Cross-package changes with parsnip

* The new `eval_time` argument replaces the `time` argument for the time points at which to predict survival probability and hazard. The `time` argument has been deprecated (#244).

* The matrix interface for fitting, `fit_xy()`, now works for censored regression models (#225, #234, #247, #251).

* Improved error messages throughout the package (#248).

## New engines

* Added the new `"aorsf"` engine for `rand_forest()` for accelerated oblique random survival forests with the aorsf package (@bcjaeger, #211).

* Added the new `flexsurvspline` engine for `survival_reg()` (@mattwarkentin, #213).

## Bug fixes

* Predictions of type `"linear_pred"` for `survival_reg(engine = "flexsurv")` are now on the correct scale for distributions where the natural scale and the unrestricted scale of the location parameter are identical, e.g. `dist = "lnorm"` (#229).

* Predictions of type `"linear_pred"` for `proportional_hazards(engine = "glmnet")` via `multi_predict()` now have the same sign as those via `predict()` (#242).

* Predictions of survival probability for `survival_reg(engine = "flexsurv")` for a single time point are now nested correctly (#254).

* Predictions of survival probability for `decision_tree(engine = "rpart")` for a single observation now work (#256).

* Predictions of type `"quantile"` for `survival_reg(engine = "survival")` for a single observation now work (#257).

* Fixed a bug for printing `coxnet` models, i.e., `proportional_hazards()` models fitted with the `"glmnet"` engine (#249).

## Internal changes

* Predictions of survival probabilities are now calculated via `summary.survfit()` for `proportional_hazards()` models with the `"survival"` and `"glmnet"` engines, `bag_tree()` models with the `"rpart"` engine, `decision_tree()` models with the `"partykit"` engines, as well as `rand_forest()` models with the `"partykit"` engine (#221, #224). 

* Added internal `survfit_summary_*()` helper functions (#216).


# censored 0.1.1

* For boosted trees with the `"mboost"` engine, survival probabilities can now be predicted for `time = -Inf`. This is always 1. For `time = Inf` this now predicts a survival probability of 0 (#215).

* Updated tests on model arguments and `update()` methods (#208).

* Internal re-organisation of code (#206, 209).

* Added a `NEWS.md` file to track changes to the package.
