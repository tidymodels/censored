library(testthat)
library(survival)

# ------------------------------------------------------------------------------

context("Random forest - party")

# ------------------------------------------------------------------------------

mod_spec <- rand_forest() %>%
  set_engine("party") %>%
  set_mode("censored regression")

# ------------------------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("party")

  set.seed(1234)
  suppressWarnings({
    exp_f_fit <- party::cforest(Surv(time, status) ~ age + ph.ecog, data = lung)
  })

  # formula method
  set.seed(1234)
  expect_error(
    suppressWarnings({
      f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)
    }),
    NA
  )

  # Removing x element from f_fit and call from both
  expect_equal(f_fit$fit, exp_f_fit)
})

# ------------------------------------------------------------------------------

test_that("time predictions", {
  skip_if_not_installed("party")

  set.seed(1234)
  suppressWarnings({
    exp_f_fit <- party::cforest(Surv(time, status) ~ age + ph.ecog, data = lung)
  })

  # formula method
  set.seed(1234)
  expect_error(
    suppressWarnings({
      f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)
    }),
    NA
  )
  f_pred <- predict(f_fit, lung, type = "time")
  exp_f_pred <- predict(exp_f_fit, newdata = lung)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equivalent(f_pred$.pred_time, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))
})

# ------------------------------------------------------------------------------

test_that("survival predictions", {
  skip_if_not_installed("party")

  set.seed(1234)
  suppressWarnings({
    exp_f_fit <- party::cforest(Surv(time, status) ~ age + ph.ecog, data = lung)
  })

  # formula method
  set.seed(1234)
  expect_error(
    suppressWarnings({
      f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)
    }),
    NA
  )
  expect_error(predict(f_fit, lung, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")
  f_pred <- predict(f_fit, lung, type = "survival", time = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_equal(
    unique(purrr::map_int(f_pred$.pred, nrow)),
    101
  )
  cf_names <-
    c(".time", ".pred_survival", ".pred_survival_lower", ".pred_survival_upper")
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ identical(names(.x), cf_names)))
  )

  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.time,
    rep(100:200, nrow(lung))
  )

  f_pred <- predict(f_fit, lung[1,], type = "survival", time = 306)
  new_km <- party::treeresponse(exp_f_fit, lung[1,])[[1]]
  # Prediction should be fairly near the actual value

  expect_equal(
    f_pred$.pred[[1]]$.pred_survival,
    new_km$surv[new_km$time == 306],
    tolerance = .1
  )
})
