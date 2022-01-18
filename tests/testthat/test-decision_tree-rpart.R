library(testthat)

test_that("model object", {
  set.seed(1234)
  exp_f_fit <- pec::pecRpart(Surv(time, status) ~ age + ph.ecog, data = lung)

  # formula method
  cox_spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("rpart")
  set.seed(1234)
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung),
    NA
  )

  expect_equal(f_fit$fit, exp_f_fit, ignore_formula_env = TRUE)
})

test_that("time predictions", {
  set.seed(1234)
  exp_f_fit <- pec::pecRpart(Surv(time, status) ~ age + ph.ecog, data = lung)

  cox_spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("rpart")
  set.seed(1234)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  f_pred <- predict(f_fit, lung, type = "time")
  exp_f_pred <- predict(exp_f_fit$rpart, lung)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(f_pred$.pred_time, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))
})

test_that("survival predictions", {
  set.seed(1234)
  exp_f_fit <- pec::pecRpart(Surv(time, status) ~ age + ph.ecog, data = lung)

  cox_spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("rpart")
  set.seed(1234)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_error(predict(f_fit, lung, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")

  f_pred <- predict(f_fit, lung, type = "survival", time = 100:200)
  exp_f_pred <- pec::predictSurvProb(exp_f_fit, lung, times = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(101, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.time,
    rep(100:200, nrow(lung))
  )

  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    as.numeric(t(exp_f_pred))
  )
})
