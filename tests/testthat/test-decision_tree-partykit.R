library(testthat)

test_that("model object", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("coin")

  set.seed(1234)
  exp_f_fit <- partykit::ctree(Surv(time, status) ~ age + ph.ecog, data = lung)

  # formula method
  cox_spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("partykit")
  set.seed(1234)
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung),
    NA
  )

  # Removing `call` element from comparison
  f_fit$fit$info$call <- NULL
  exp_f_fit$info$call <- NULL
  expect_equal(
    f_fit$fit,
    exp_f_fit,
    ignore_function_env = TRUE,
    ignore_formula_env = TRUE
  )
})


# prediction: time --------------------------------------------------------

test_that("time predictions", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("coin")

  set.seed(1234)
  exp_f_fit <- partykit::ctree(Surv(time, status) ~ age + ph.ecog, data = lung)

  cox_spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("partykit")
  set.seed(1234)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  f_pred <- predict(f_fit, lung, type = "time")
  exp_f_pred <- predict(exp_f_fit, lung)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(f_pred$.pred_time, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))

  # single observation
  f_pred_1 <- predict(f_fit, lung[1, ], type = "time")
  expect_identical(nrow(f_pred_1), 1L)
})


# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("coin")

  set.seed(1234)
  exp_f_fit <- partykit::ctree(Surv(time, status) ~ age + ph.ecog, data = lung)

  cox_spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("partykit")
  set.seed(1234)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  # move snapshot test below back here after parsnip v1.3.0 release

  f_pred <- predict(f_fit, lung, type = "survival", eval_time = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_equal(
    unique(purrr::map_int(f_pred$.pred, nrow)),
    101
  )
  cf_names <-
    c(".eval_time", ".pred_survival")
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~identical(names(.x), cf_names)
      )
    )
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.eval_time,
    rep(100:200, nrow(lung))
  )

  # single observation
  f_pred <- predict(f_fit, lung[1, ], type = "survival", eval_time = 306)
  new_km <- predict(exp_f_fit, newdata = lung[1, ], type = "prob")[[1]]
  # Prediction should be fairly near the actual value

  expect_equal(
    f_pred$.pred[[1]]$.pred_survival,
    new_km$surv[new_km$time == 306],
    tolerance = .1
  )
})

test_that("survival predictions - error snapshot", {
  skip_if_not_installed("parsnip", minimum_version = "1.3.0")
  skip_if_not_installed("partykit")
  skip_if_not_installed("coin")

  cox_spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("partykit")
  set.seed(1234)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_snapshot(error = TRUE, {
    predict(f_fit, lung, type = "survival")
  })
})

test_that("can predict for out-of-domain timepoints", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("coin")

  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[2,]

  mod <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("partykit") %>%
    fit(Surv(time, status) ~ ., data = lung)

  expect_no_error(
    preds <- predict(mod, obs_without_NA, type = "survival", eval_time = eval_time_obs_max_and_ood)
  )
})

# fit via matrix interface ------------------------------------------------

test_that("`fix_xy()` works", {
  skip_if_not_installed("partykit")
  skip_if_not_installed("coin")
  
  lung_x <- as.matrix(lung[, c("age", "ph.ecog")])
  lung_y <- Surv(lung$time, lung$status)
  lung_pred <- lung[1:5, ]

  spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("partykit")
  set.seed(1)
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung)
  set.seed(1)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  # prep for model comparison
  f_fit_fit <- unclass(f_fit$fit)
  xy_fit_fit <- unclass(xy_fit$fit)
  elements_to_ignore <- c("call", "data", "terms")
  f_ignore <- which(names(f_fit_fit) %in% elements_to_ignore)
  xy_ignore <- which(names(xy_fit_fit) %in% elements_to_ignore)

  expect_equal(
    f_fit_fit[-f_ignore],
    xy_fit_fit[-xy_ignore],
    ignore_function_env = TRUE
  )

  f_pred_time <- predict(f_fit, new_data = lung_pred, type = "time")
  xy_pred_time <- predict(xy_fit, new_data = lung_pred, type = "time")
  expect_equal(f_pred_time, xy_pred_time)

  f_pred_survival <- predict(
    f_fit,
    new_data = lung_pred,
    type = "survival",
    eval_time = c(100, 200)
  )
  xy_pred_survival <- predict(
    xy_fit,
    new_data = lung_pred,
    type = "survival",
    eval_time = c(100, 200)
  )
  expect_equal(f_pred_survival, xy_pred_survival)
})
