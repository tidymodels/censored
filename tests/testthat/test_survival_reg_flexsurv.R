library(testthat)
library(parsnip)
library(rlang)
library(survival)
library(tibble)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------

context("Survival Regresion - flexsurv")

# ------------------------------------------------------------------------------

source(test_path("helper-objects.R"))

basic_form <- Surv(time, status) ~ age
complete_form <- Surv(time) ~ age

surv_basic <- survival_reg(dist = "lognormal") %>% set_engine("flexsurv")
surv_weib <- survival_reg(dist = "weibull") %>% set_engine("flexsurv")

# ------------------------------------------------------------------------------

test_that("flexsurv execution", {
  skip_if_not_installed("flexsurv")

  expect_error(
    res <- fit(
      surv_basic,
      Surv(time, status) ~ age,
      data = lung,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      surv_basic,
      Surv(time) ~ age,
      data = lung,
      control = ctrl
    ),
    regexp = NA
  )
  expect_false(has_multi_predict(res))
  expect_equal(multi_predict_args(res), NA_character_)

  expect_error(
    res <- fit_xy(
      surv_basic,
      x = lung[, "age", drop = FALSE],
      y = lung$time,
      control = ctrl
    )
  )
})

test_that("flexsurv time prediction", {
  skip_if_not_installed("flexsurv")

  res <- fit(
    surv_basic,
    Surv(time, status) ~ age,
    data = lung,
    control = ctrl
  )
  exp_pred <- summary(res$fit, head(lung), type = "mean")
  exp_pred <- do.call("rbind", unclass(exp_pred))
  exp_pred <- tibble(.pred = exp_pred$est)
  expect_equal(exp_pred$.pred, predict(res, head(lung))$.pred_time)
})


test_that("survival probability prediction", {
  skip_on_travis()

  res <- fit(
    surv_weib,
    Surv(time, status) ~ age + sex,
    data = lung,
    control = ctrl
  )
  expect_error(
    predict(res, head(lung), type = "survival"),
    "a numeric vector 'time'"
  )

  exp_pred <- predict(res, head(lung), type = "survival", time = prob_times)
  exp_pred_vert <- exp_pred %>% mutate(.patient = row_number()) %>% unnest(cols = .pred)
  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(names(exp_pred_vert), c(".time", ".pred_survival", ".patient"))

  # using rms for expected results
  expect_equal(exp_pred$.pred[[1]]$.pred_survival, rms_surv, tol = 0.001)
})


test_that("survival hazard prediction", {
  skip_on_travis()

  res <- fit(
    surv_weib,
    Surv(time, status) ~ age + sex,
    data = lung,
    control = ctrl
  )
  expect_error(
    predict(res, head(lung), type = "hazard"),
    "a numeric vector 'time'"
  )

  exp_pred <- predict(res, head(lung), type = "hazard", time = prob_times)
  exp_pred_vert <- exp_pred %>% mutate(.patient = row_number()) %>% unnest(cols = .pred)
  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(names(exp_pred_vert), c(".time", ".pred_hazard", ".patient"))

  # using rms for expected results
  expect_equal(exp_pred$.pred[[1]]$.pred_hazard, rms_haz, tol = 0.001)
})

test_that("quantile predictions", {
  set.seed(1)
  fit_s <- survival_reg() %>%
    set_engine("flexsurv") %>%
    set_mode("censored regression") %>%
    fit(Surv(stop, event) ~ rx + size + enum, data = bladder)
  pred <- predict(fit_s, new_data = bladder[1:3,], type = "quantile")

  set.seed(1)
  exp_fit <- flexsurv::flexsurvreg(
    Surv(stop, event) ~ rx + size + enum,
    data = bladder,
    dist = "weibull"
  )
  exp_pred <- summary(
    exp_fit,
    newdata = bladder[1:3, ],
    type = "quantile",
    quantiles = (1:9)/10
    )

  expect_s3_class(pred, "tbl_df")
  expect_equal(names(pred), ".pred")
  expect_equal(nrow(pred), 3)
  expect_true(
    all(purrr::map_lgl(pred$.pred,
                       ~ all(dim(.x) == c(9, 2))))
  )
  expect_true(
    all(purrr::map_lgl(pred$.pred,
                       ~ all(names(.x) == c(".quantile",
                                            ".pred_quantile"))))
  )
  expect_equal(
    tidyr::unnest(pred, cols = .pred)$.pred_quantile,
    do.call(rbind, exp_pred)$est
  )

  # add confidence interval
  pred <- predict(fit_s, new_data = bladder[1:3,], type = "quantile",
                  interval = "confidence", level = 0.7)
  expect_true(
    all(purrr::map_lgl(pred$.pred,
                       ~ all(names(.x) == c(".quantile",
                                            ".pred_quantile",
                                            ".pred_lower",
                                            ".pred_upper"))))
  )

})

test_that("linear predictor", {
  f_fit <- survival_reg() %>%
    set_engine("flexsurv") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung[1:5,], type = "linear_pred")

  exp_fit <- flexsurv::flexsurvreg(
    Surv(time, status) ~ age + sex,
    data = lung,
    dist = "weibull"
    )
  exp_pred <- predict(exp_fit, lung[1:5,], type = "linear")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, log(exp_pred$.pred))
  expect_equal(nrow(f_pred), 5)
})
