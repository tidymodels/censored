library(testthat)
library(survival)

# registration ------------------------------------------------------------

test_that("engine is registered and translate() works", {
  engines <- parsnip::show_engines("null_model")
  censored_engines <- engines$engine[engines$mode == "censored regression"]
  expect_in("survival", censored_engines)

  spec <- null_model() |>
    set_engine("survival") |>
    set_mode("censored regression")

  translated <- translate(spec)
  expect_equal(translated$method$fit$func[["fun"]], "survfit_null")
})

# fit ---------------------------------------------------------------------

test_that("model object ignores predictors", {
  spec <- null_model() |>
    set_engine("survival") |>
    set_mode("censored regression")

  f_fit <- fit(spec, Surv(time, status) ~ ., data = lung)

  exp_fit <- survfit(Surv(time, status) ~ 1, data = lung)
  expect_s3_class(f_fit$fit, "survfit")
  expect_equal(f_fit$fit$surv, exp_fit$surv)
  expect_equal(f_fit$fit$time, exp_fit$time)
})

test_that("time predictions", {
  spec <- null_model() |>
    set_engine("survival") |>
    set_mode("censored regression")
  f_fit <- fit(spec, Surv(time, status) ~ ., data = lung)
  f_pred <- predict(f_fit, lung, type = "time")

  km <- survfit(Surv(time, status) ~ 1, data = lung)
  exp_pred <- unname(quantile(km, probs = 0.5)$quantile)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred_time")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_all_equal(f_pred$.pred_time, exp_pred)

  f_pred_1 <- predict(f_fit, lung[1, ], type = "time")
  expect_equal(nrow(f_pred_1), 1)
})

test_that("survival predictions", {
  spec <- null_model() |>
    set_engine("survival") |>
    set_mode("censored regression")
  f_fit <- fit(spec, Surv(time, status) ~ ., data = lung)
  eval_time <- c(100, 300)
  f_pred <- predict(f_fit, lung, type = "survival", eval_time = eval_time)

  km <- survfit(Surv(time, status) ~ 1, data = lung)
  exp_surv <- summary(km, times = eval_time, extend = TRUE)$surv

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))

  expect_equal(names(f_pred$.pred[[1]]), c(".eval_time", ".pred_survival"))
  expect_equal(f_pred$.pred[[1]]$.eval_time, eval_time)
  expect_equal(f_pred$.pred[[1]]$.pred_survival, exp_surv)

  # identical curve across rows
  expect_equal(f_pred$.pred[[1]], f_pred$.pred[[nrow(lung)]])
})

test_that("predictions ignore missing values in predictors", {
  spec <- null_model() |>
    set_engine("survival") |>
    set_mode("censored regression")
  f_fit <- fit(spec, Surv(time, status) ~ ., data = lung)

  new_data <- lung[1:3, ]
  new_data$age <- NA
  new_data[2, "sex"] <- NA

  time_pred <- predict(f_fit, new_data, type = "time")
  expect_equal(nrow(time_pred), nrow(new_data))
  expect_equal(time_pred, predict(f_fit, lung[1:3, ], type = "time"))

  surv_pred <- predict(
    f_fit,
    new_data,
    type = "survival",
    eval_time = c(100, 300)
  )
  expect_equal(nrow(surv_pred), nrow(new_data))
  expect_equal(
    surv_pred,
    predict(f_fit, lung[1:3, ], type = "survival", eval_time = c(100, 300))
  )
})
