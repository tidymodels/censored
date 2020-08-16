library(testthat)
library(survival)
library(mboost)

# ------------------------------------------------------------------------------

context("Boosted Tree - mboost")

# ------------------------------------------------------------------------------

lung2 <- lung[-14, ]

cox_spec <- boost_tree() %>% set_mode("censored regression") %>% set_engine("mboost")

exp_f_fit <- blackboost(Surv(time, status) ~ age + ph.ecog,
                        data = lung2,
                        family = CoxPH())

# ------------------------------------------------------------------------------

test_that('model object', {

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)

  # Removing x element from f_fit and call from both
  expect_equal(unclass(f_fit$fit)[-24], unclass(exp_f_fit)[-24])
})

# ------------------------------------------------------------------------------

test_that('survival predictions', {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)
  expect_error(predict(f_fit, lung2, type = "survival"),
               'argument ".time" is missing, with no default')
  f_pred <- predict(f_fit, lung2, type = "survival", .time = c(0, 100, 200, 10000))
  exp_f_pred <- mboost::survFit(exp_f_fit, lung2)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred_survival")
  expect_equal(nrow(f_pred), nrow(lung2))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred_survival,
                       ~ all(dim(.x) == c(4, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred_survival,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred_survival))$.pred_survival,
    as.numeric(t(floor_surv_mboost(exp_f_pred, c(0, 100, 200, 10000))))
  )
})

# ------------------------------------------------------------------------------

test_that('linear_pred predictions', {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)
  f_pred <- predict(f_fit, lung2, type = "linear_pred")
  exp_f_pred <- unname(predict(exp_f_fit, newdata = lung2))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))
})
