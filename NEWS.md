# censored (development version)

* The matrix interface for fitting `fit_xy()` now works for censored regression models.

* Predictions of survival probabilities for proportional hazards models with the `"survival"` and `"glmnet"` engines are now calculated by `summary.survfit()` (#221).

* Added internal `survfit_summary_*()` helper functions (#216).


# censored 0.1.1

* For boosted trees with the `"mboost"` engine, survival probabilities can now be predicted for `time = -Inf`. This is always 1. For `time = Inf` this now predicts a survival probability of 0 (#215).

* Updated tests on model arguments and `update()` methods (#208).

* Internal re-organisation of code (#206, 209).

* Added a `NEWS.md` file to track changes to the package.
