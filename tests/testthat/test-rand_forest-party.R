library(testthat)
library(survival)
library(pec)

# ------------------------------------------------------------------------------

context("Random forest - party")

# ------------------------------------------------------------------------------

mod_spec <- rand_forest() %>% set_mode("censored regression") %>% set_engine("party")

set.seed(1234)
suppressWarnings(
  {
    exp_f_fit <- pecCforest(Surv(time, status) ~ age + ph.ecog, data = lung)
  }
)


# ------------------------------------------------------------------------------

test_that('model object', {

  # formula method
  set.seed(1234)
  expect_error(
    suppressWarnings(
      {
    f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)
      }
    ), NA)

  # Removing x element from f_fit and call from both
  expect_equal(f_fit$fit$forest, exp_f_fit$forest)
})

# ------------------------------------------------------------------------------

test_that('time predictions', {
  # formula method
  set.seed(1234)
  expect_error(
    suppressWarnings(
      {
        f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)
      }
    ), NA)
  f_pred <- predict(f_fit, lung, type = "time")
  exp_f_pred <- predict(exp_f_fit$forest, newdata = lung)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equivalent(f_pred$.pred_time, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))
})

# ------------------------------------------------------------------------------

test_that('survival predictions', {
  # formula method
  set.seed(1234)
  expect_error(
    suppressWarnings(
      {
        f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)
      }
    ), NA)
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

  # Out of domain prediction
  f_pred <- predict(f_fit, lung, type = "survival", .time = 10000)
  exp_f_pred <- pec::predictSurvProb(exp_f_fit, lung, times = c(1, max(lung$time)))

  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred_survival))$.pred_survival,
    exp_f_pred[,-1]
  )
})
