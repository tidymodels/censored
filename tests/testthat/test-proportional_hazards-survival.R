library(testthat)
library(survival)

# ------------------------------------------------------------------------------

context("Cox Regression - survival")

# survival has some issues where missing predictor value get ommited despite
# na.action = na.exclude. See https://github.com/therneau/survival/issues/137

# ------------------------------------------------------------------------------

cox_spec <- proportional_hazards() %>% set_engine("survival")

exp_f_fit <- coxph(Surv(time, status) ~ age + sex, data = lung, x = TRUE)

# ------------------------------------------------------------------------------

test_that("model object", {

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung), NA)

  # Removing x element from f_fit and call from both
  expect_equal(coef(f_fit$fit), coef(exp_f_fit))
})

# ------------------------------------------------------------------------------

test_that("time predictions", {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung), NA)
  f_pred <- predict(f_fit, lung, type = "time")
  exp_f_pred <- unname(summary(survfit(exp_f_fit, lung, na.action = na.pass))$table[, "*rmean"])

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equivalent(f_pred$.pred_time, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))

  # stratified model but no strata info
  cox_model <- proportional_hazards() %>% set_engine("survival")
  cox_fit_strata <- cox_model %>%
    fit(Surv(time, status) ~ age + sex + wt.loss + strata(inst), data = lung)
  new_data_0 <- data.frame(age = c(50, 60), sex = 1, wt.loss = 5)
  expect_error(
    predict(cox_fit_strata, new_data = new_data_0, type = "time"),
    "provide the strata"
  )
})

# ------------------------------------------------------------------------------

test_that("survival predictions", {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung), NA)
  expect_error(predict(f_fit, lung, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")
  # Test at observed event times since we use the step function and pec does not
  f_pred <- predict(f_fit, lung, type = "survival", time = c(306, 455))
  exp_f_pred <- pec::predictSurvProb(exp_f_fit, lung, times = c(306, 455))

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    as.numeric(t(exp_f_pred))
  )

  # stratified model but no strata info
  cox_model <- proportional_hazards() %>% set_engine("survival")
  cox_fit_strata <- cox_model %>%
    fit(Surv(time, status) ~ age + sex + wt.loss + strata(inst), data = lung)
  new_data_0 <- data.frame(age = c(50, 60), sex = 1, wt.loss = 5)
  expect_error(
    predict(cox_fit_strata, new_data = new_data_0, type = "survival", time = 200),
    "provide the strata"
  )
})

# ------------------------------------------------------------------------------

test_that("linear_pred predictions", {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung), NA)
  f_pred <- predict(f_fit, lung, type = "linear_pred")
  exp_f_pred <- -unname(predict(exp_f_fit, newdata = lung))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))
})

# ------------------------------------------------------------------------------

test_that("api errors", {
  expect_error(
    proportional_hazards() %>% set_engine("lda"),
    regexp = "Available engines are:"
  )
})
