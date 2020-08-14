library(testthat)
library(survival)
library(glmnet)

# ------------------------------------------------------------------------------

context("Cox Regression - glmnet")

# ------------------------------------------------------------------------------

lung2 <- lung[-14, ]

cox_spec <- cox_reg() %>% set_engine("glmnet")

exp_f_fit <- glmnet(x = as.matrix(lung2[, c(4, 6)]),
                    y = Surv(lung2$time, lung2$status),
                    family = "cox")

# ------------------------------------------------------------------------------

test_that('model object', {

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)

  # Removing call element
  expect_equal(f_fit$fit[-11], exp_f_fit[-11])
})

# ------------------------------------------------------------------------------

test_that('linear_pred predictions', {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01)
  exp_f_pred <- unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))
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
