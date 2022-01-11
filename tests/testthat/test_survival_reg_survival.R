library(testthat)
library(parsnip)
library(survival)
library(tidyr)
library(tibble)
library(dplyr)
library(flexsurv)

# ------------------------------------------------------------------------------

source(test_path("helper-objects.R"))

basic_form <- Surv(time, status) ~ group
complete_form <- Surv(time) ~ group

surv_basic <- survival_reg() %>% set_engine("survival")
surv_lnorm <- survival_reg(dist = "lognormal") %>% set_engine("survival")
surv_exp <- survival_reg(dist = "exponential") %>% set_engine("survival")

# ------------------------------------------------------------------------------

test_that("survival execution", {

  skip_on_travis()

  expect_error(
    res <- fit(
      surv_basic,
      Surv(time, status) ~ age + sex,
      data = lung,
      control = ctrl
    ),
    regexp = NA
  )
  expect_output(print(res), "parsnip model object")
  expect_error(
    res <- fit(
      surv_lnorm,
      Surv(time) ~ age + sex,
      data = lung,
      control = ctrl
    ),
    regexp = NA
  )
  expect_error(
    res <- fit_xy(
      surv_basic,
      x = lung[, c("age", "sex")],
      y = lung$time,
      control = ctrl
    )
  )
})

test_that("survival time prediction", {
  skip_on_travis()

  res <- fit(
    surv_basic,
    Surv(time, status) ~ age + sex,
    data = lung,
    control = ctrl
  )
  exp_pred <- predict(res$fit, head(lung))
  exp_pred <- tibble(.pred_time = unname(exp_pred))
  expect_equal(exp_pred, predict(res, head(lung)))

  exp_quant <- predict(res$fit, head(lung), p = (2:4) / 5, type = "quantile")
  exp_quant <-
    apply(exp_quant, 1, function(x)
      tibble(.quantile = (2:4) / 5, .pred_quantile = x))
  exp_quant <- tibble(.pred = exp_quant)
  obs_quant <- predict(res, head(lung), type = "quantile", quantile = (2:4) / 5)

  expect_equal(as.data.frame(exp_quant), as.data.frame(obs_quant))

})


test_that("survival probability prediction", {
  skip_on_travis()

  res <- fit(
    surv_basic,
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
  expect_equal(exp_pred$.pred[[1]]$.pred_survival, rms_surv, tolerance = 0.001)
})


test_that("survival hazard prediction", {
  skip_on_travis()

  res <- fit(
    surv_basic,
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
  expect_equal(
    exp_pred$.pred[[1]]$.pred_hazard[-1],
    rms_haz[-1],
    tolerance = 0.001
  )
})

test_that("linear predictor", {
  f_fit <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung[1:5,], type = "linear_pred")

  exp_fit <- survreg(Surv(time, status) ~ age + sex, data = lung)
  exp_pred <- predict(exp_fit, lung[1:5,], type = "linear")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_pred))
  expect_equal(nrow(f_pred), 5)
})
