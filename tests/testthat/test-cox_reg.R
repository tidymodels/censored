library(testthat)
library(survival)

# ------------------------------------------------------------------------------

context("Cox Regression")

# ------------------------------------------------------------------------------

cox_spec <- cox_reg() %>% set_engine("survival")

exp_f_fit <- coxph(Surv(time, status) ~ age + ph.ecog, data = lung, x = TRUE)

# ------------------------------------------------------------------------------

test_that('model object', {

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung), NA)

  # Removing x element from f_fit and call from both
  expect_equal(f_fit$fit[-c(20)], exp_f_fit[-c(20)])
})

# ------------------------------------------------------------------------------

test_that('time predictions', {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung), NA)
  f_pred <- predict(f_fit, lung, type = "time")
  exp_f_pred <- unname(summary(survfit(exp_f_fit, lung, na.action = na.pass))$table[, "*rmean"])

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equivalent(f_pred$.pred_time, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))
})

# ------------------------------------------------------------------------------

test_that('survival predictions', {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung), NA)
  expect_error(predict(f_fit, lung, type = "survival"),
               'argument ".time" is missing, with no default')
  f_pred <- predict(f_fit, lung, type = "survival", .time = 100:200)
  exp_f_pred <- pec::predictSurvProb(exp_f_fit, lung, times = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred_survival")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred_survival,
                       ~ all(dim(.x) == c(101, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred_survival,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred_survival))$.pred_survival,
    as.numeric(t(exp_f_pred))
  )
})
# ------------------------------------------------------------------------------

test_that('api errors', {
  expect_error(
    cox_reg() %>% set_engine("lda"),
    regexp = "Engine 'lda' is not available"
  )
})


# ------------------------------------------------------------------------------

test_that('printing', {
  expect_output(
    print(cox_reg()),
    "Cox Model Specification \\(censored regression\\)"
  )
})
