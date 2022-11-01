# censored (development version)

* Added the new `"aorsf"` engine for `rand_forest()` for accelerated oblique random survival forests with the aorsf package (@bcjaeger, #211).

* The matrix interface for fitting `fit_xy()` now works for censored regression models, with the exception of `decision_tree()` with the `"rpart"` engine and `survival_reg()` with the `"flexsurv"` engine (#225).

* Predictions of survival probabilities are now calculated via `summary.survfit()` for `proportional_hazards()` models with the `"survival"` and `"glmnet"` engines, `bag_tree()` models with the `"rpart"` engine, `decision_tree()` models with the `"partykit"` engines, as well as `rand_forest()` models with the `"partykit"` engine (#221, #224). 

* Added internal `survfit_summary_*()` helper functions (#216).


# censored 0.1.1

* For boosted trees with the `"mboost"` engine, survival probabilities can now be predicted for `time = -Inf`. This is always 1. For `time = Inf` this now predicts a survival probability of 0 (#215).

* Updated tests on model arguments and `update()` methods (#208).

* Internal re-organisation of code (#206, 209).

* Added a `NEWS.md` file to track changes to the package.
