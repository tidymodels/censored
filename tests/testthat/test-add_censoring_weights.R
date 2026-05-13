skip_if_not_installed("parsnip", minimum_version = "1.5.0.9001")

test_that("predict() with add_censoring_weights = TRUE works when new_data has separate time and event columns", {
  cox_spec <- proportional_hazards(penalty = 0.1) |>
    set_engine("glmnet", cox.ties = "breslow")

  f_fit <- fit(
    cox_spec,
    Surv(time, status) ~ age + ph.ecog,
    data = lung
  )

  f_pred <- predict(
    f_fit,
    new_data = lung,
    type = "survival",
    eval_time = c(100, 200),
    add_censoring_weights = TRUE
  )
  f_aug <- augment(f_fit, new_data = lung, eval_time = c(100, 200))

  expect_contains(
    names(f_pred$.pred[[1]]),
    c(
      ".eval_time",
      ".pred_survival",
      ".weight_time",
      ".pred_censored",
      ".weight_censored"
    )
  )
  expect_equal(f_pred$.pred, f_aug$.pred)
})

test_that("predict() with add_censoring_weights = TRUE works when new_data has a single Surv column", {
  lung_surv <- lung[, c("age", "ph.ecog")]
  lung_surv$outcome <- Surv(lung$time, lung$status)
  cox_spec <- proportional_hazards(penalty = 0.1) |>
    set_engine("glmnet", cox.ties = "breslow")

  f_fit <- fit(cox_spec, outcome ~ age + ph.ecog, data = lung_surv)
  f_pred <- predict(
    f_fit,
    new_data = lung_surv,
    type = "survival",
    eval_time = c(100, 200),
    add_censoring_weights = TRUE
  )
  f_aug <- augment(f_fit, new_data = lung_surv, eval_time = c(100, 200))

  expect_contains(
    names(f_pred$.pred[[1]]),
    c(
      ".eval_time",
      ".pred_survival",
      ".weight_time",
      ".pred_censored",
      ".weight_censored"
    )
  )
  expect_equal(f_pred$.pred, f_aug$.pred)

  xy_fit <- fit_xy(
    cox_spec,
    x = as.matrix(lung_surv[, c("age", "ph.ecog")]),
    y = lung_surv$outcome
  )
  xy_pred <- predict(
    xy_fit,
    new_data = lung_surv,
    type = "survival",
    eval_time = c(100, 200),
    add_censoring_weights = TRUE
  )
  xy_aug <- augment(xy_fit, new_data = lung_surv, eval_time = c(100, 200))
  expect_equal(xy_pred$.pred, xy_aug$.pred)
})

test_that("predict() with add_censoring_weights = TRUE errors when new_data is missing the survival outcome", {
  cox_spec <- proportional_hazards(penalty = 0.1) |>
    set_engine("glmnet", cox.ties = "breslow")

  f_fit <- fit(
    cox_spec,
    Surv(time, status) ~ age + ph.ecog,
    data = lung
  )
  no_outcome <- lung[, c("age", "ph.ecog")]

  expect_snapshot(
    error = TRUE,
    predict(
      f_fit,
      new_data = no_outcome,
      type = "survival",
      eval_time = c(100, 200),
      add_censoring_weights = TRUE
    )
  )
})

test_that("predict() with add_censoring_weights = TRUE errors when fit_xy was used and new_data has no Surv column", {
  cox_spec <- proportional_hazards(penalty = 0.1) |>
    set_engine("glmnet", cox.ties = "breslow")

  xy_fit <- fit_xy(
    cox_spec,
    x = as.matrix(lung[, c("age", "ph.ecog")]),
    y = Surv(lung$time, lung$status)
  )
  no_outcome <- lung[, c("age", "ph.ecog")]

  expect_snapshot(
    error = TRUE,
    predict(
      xy_fit,
      new_data = no_outcome,
      type = "survival",
      eval_time = c(100, 200),
      add_censoring_weights = TRUE
    )
  )
})

test_that("default predict() output is unchanged when add_censoring_weights is omitted", {
  cox_spec <- proportional_hazards(penalty = 0.1) |>
    set_engine("glmnet", cox.ties = "breslow")

  f_fit <- fit(
    cox_spec,
    Surv(time, status) ~ age + ph.ecog,
    data = lung
  )

  f_pred <- predict(
    f_fit,
    new_data = lung,
    type = "survival",
    eval_time = c(100, 200)
  )

  expect_named(f_pred$.pred[[1]], c(".eval_time", ".pred_survival"))
})

test_that("add_censoring_weights = FALSE matches default behavior", {
  cox_spec <- proportional_hazards(penalty = 0.1) |>
    set_engine("glmnet", cox.ties = "breslow")

  f_fit <- fit(
    cox_spec,
    Surv(time, status) ~ age + ph.ecog,
    data = lung
  )

  f_pred_default <- predict(
    f_fit,
    new_data = lung,
    type = "survival",
    eval_time = c(100, 200)
  )
  f_pred_off <- predict(
    f_fit,
    new_data = lung,
    type = "survival",
    eval_time = c(100, 200),
    add_censoring_weights = FALSE
  )

  expect_equal(f_pred_default, f_pred_off)
})

test_that("add_censoring_weights validates the flag", {
  cox_spec <- proportional_hazards(penalty = 0.1) |>
    set_engine("glmnet", cox.ties = "breslow")

  f_fit <- fit(
    cox_spec,
    Surv(time, status) ~ age + ph.ecog,
    data = lung
  )

  expect_snapshot(
    error = TRUE,
    predict(
      f_fit,
      new_data = lung,
      type = "survival",
      eval_time = c(100, 200),
      add_censoring_weights = "yes"
    )
  )
})
