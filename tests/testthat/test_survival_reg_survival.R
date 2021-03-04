library(testthat)
library(parsnip)
library(survival)
library(tidyr)
library(tibble)
library(dplyr)
library(flexsurv)

# ------------------------------------------------------------------------------

context("Survival Regresion - survival")

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
      tibble(.pred = x, .quantile = (2:4) / 5))
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
    "a numeric vector '.time'"
  )

  exp_pred <- predict(res, head(lung), type = "survival", .time = prob_times)
  exp_pred_vert <- exp_pred %>% mutate(.patient = row_number()) %>% unnest(cols = .pred)
  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(names(exp_pred_vert), c(".time", ".pred_survival", ".patient"))

  # using rms for expected results
  expect_equal(exp_pred$.pred[[1]]$.pred_survival, rms_surv, tol = 0.001)
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
    "a numeric vector '.time'"
  )

  exp_pred <- predict(res, head(lung), type = "hazard", .time = prob_times)
  exp_pred_vert <- exp_pred %>% mutate(.patient = row_number()) %>% unnest(cols = .pred)
  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(names(exp_pred_vert), c(".time", ".pred_hazard", ".patient"))

  # using rms for expected results
  expect_equal(exp_pred$.pred[[1]]$.pred_hazard[-1], rms_haz[-1], tol = 0.001)
})
