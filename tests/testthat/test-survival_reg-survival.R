library(testthat)

test_that("model object", {
  set.seed(1234)
  exp_f_fit <- survival::survreg(
    Surv(time, status) ~ age + ph.ecog,
    data = lung,
    model = TRUE
  )

  mod_spec <- survival_reg() %>%
    set_engine("survival") %>%
    set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  # remove `call` from comparison
  f_fit$fit$call <- NULL
  exp_f_fit$call <- NULL

  expect_equal(
    f_fit$fit,
    exp_f_fit,
    ignore_formula_env = TRUE
  )
})


# prediction: time --------------------------------------------------------

test_that("survival time prediction", {
  res <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  exp_pred <- predict(res$fit, head(lung))
  exp_pred <- tibble::tibble(.pred_time = unname(exp_pred))
  expect_equal(exp_pred, predict(res, head(lung)))

  # single observation
  f_pred_1 <- predict(res, lung[1, ], type = "time")
  expect_identical(nrow(f_pred_1), 1L)
})

# prediction: survival ----------------------------------------------------

test_that("survival probability prediction", {
  rms_surv <- readRDS(test_path("data", "rms_surv.rds"))
  res <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_error(
    predict(res, head(lung), type = "survival"),
    "a numeric vector `eval_time`"
  )

  exp_pred <- predict(res, head(lung), type = "survival", eval_time = c(0, 500, 1000))
  exp_pred_vert <- exp_pred %>%
    dplyr::mutate(.patient = dplyr::row_number()) %>%
    tidyr::unnest(cols = .pred)

  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(names(exp_pred_vert), c(".eval_time", ".pred_survival", ".patient"))

  # using rms for expected results
  expect_equal(
    exp_pred$.pred[[1]]$.pred_survival,
    rms_surv,
    tolerance = 0.001
  )

  # single observation
  f_pred_1 <- predict(res, lung[1, ], type = "survival", eval_time = c(100, 500))
  expect_identical(nrow(f_pred_1), 1L)
})

test_that("can predict for out-of-domain timepoints", {
  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[2,]

  mod <- survival_reg() %>%
    set_mode("censored regression") %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ ., data = lung)

  expect_no_error(
    preds <- predict(mod, obs_without_NA, type = "survival", eval_time = eval_time_obs_max_and_ood)
  )
  expect_no_error(
    preds <- predict(mod, obs_without_NA, type = "hazard", eval_time = eval_time_obs_max_and_ood)
  )
})

# prediction: linear_pred -------------------------------------------------

test_that("linear predictor", {
  f_fit <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung[1:5, ], type = "linear_pred")

  exp_fit <- survreg(Surv(time, status) ~ age + sex, data = lung)
  exp_pred <- predict(exp_fit, lung[1:5, ], type = "linear")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, unname(exp_pred))
  expect_equal(nrow(f_pred), 5)

  # single observation
  f_pred_1 <- predict(f_fit, lung[1, ], type = "linear_pred")
  expect_identical(nrow(f_pred_1), 1L)
})


# prediction: quantile ----------------------------------------------------

test_that("prediction of survival time quantile", {
  res <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  exp_quant <- predict(res$fit, head(lung), p = (2:4) / 5, type = "quantile")
  exp_quant <- apply(exp_quant, 1, function(x) {
    tibble::tibble(.quantile = (2:4) / 5, .pred_quantile = x)
  })
  exp_quant <- tibble::tibble(.pred = exp_quant)
  obs_quant <- predict(res, head(lung), type = "quantile", quantile = (2:4) / 5)

  expect_equal(as.data.frame(exp_quant), as.data.frame(obs_quant))

  # single observation
  f_pred_1 <- predict(res, lung[1, ], type = "quantile")
  expect_identical(nrow(f_pred_1), 1L)
})


# prediction: hazard ------------------------------------------------------

test_that("survival hazard prediction", {
  rms_haz <- readRDS(test_path("data", "rms_haz.rds"))
  res <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_error(
    predict(res, head(lung), type = "hazard"),
    "a numeric vector `eval_time`"
  )

  exp_pred <- predict(res, head(lung), type = "hazard", eval_time = c(0, 500, 1000))
  exp_pred_vert <- exp_pred %>%
    dplyr::mutate(.patient = dplyr::row_number()) %>%
    tidyr::unnest(cols = .pred)

  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(names(exp_pred_vert), c(".eval_time", ".pred_hazard", ".patient"))

  # using rms for expected results
  expect_equal(
    exp_pred$.pred[[1]]$.pred_hazard[-1],
    rms_haz[-1],
    tolerance = 0.001
  )

  # single observation
  f_pred_1 <- predict(res, lung[1, ], type = "hazard", eval_time = c(100, 500))
  expect_identical(nrow(f_pred_1), 1L)
})

# fit via matrix interface ------------------------------------------------

test_that("`fix_xy()` works", {
  lung_x <- as.matrix(lung[, c("age", "ph.ecog")])
  lung_y <- Surv(lung$time, lung$status)
  lung_pred <- lung[1:5, ]

  spec <- survival_reg() %>%
    set_engine("survival") %>%
    set_mode("censored regression")
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  elements_to_ignore <- c("call", "terms", "model")
  f_ignore <- which(names(f_fit$fit) %in% elements_to_ignore)
  xy_ignore <- which(names(xy_fit$fit) %in% elements_to_ignore)
  expect_equal(f_fit$fit[-f_ignore], xy_fit$fit[-xy_ignore])

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

  f_pred_lp <- predict(f_fit, new_data = lung_pred, type = "linear_pred")
  xy_pred_lp <- predict(xy_fit, new_data = lung_pred, type = "linear_pred")
  expect_equal(f_pred_lp, xy_pred_lp)

  f_pred_quantile <- predict(
    f_fit,
    new_data = lung_pred,
    type = "quantile",
    quantile = c(0.2, 0.8)
  )
  xy_pred_quantile <- predict(
    xy_fit,
    new_data = lung_pred,
    type = "quantile",
    quantile = c(0.2, 0.8)
  )
  expect_equal(f_pred_quantile, xy_pred_quantile)

  f_pred_hazard <- predict(
    f_fit,
    new_data = lung_pred,
    type = "hazard",
    eval_time = c(100, 200)
  )
  xy_pred_hazard <- predict(
    xy_fit,
    new_data = lung_pred,
    type = "hazard",
    eval_time = c(100, 200)
  )
  expect_equal(f_pred_hazard, xy_pred_hazard)
})


# deprecation of time arg -------------------------------------------------

test_that("deprecation of `time` arg for type 'survival'", {
  f_fit <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  exp_pred <- predict(f_fit, head(lung), type = "survival", eval_time = c(0, 500, 1000))

  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(
    predict(f_fit, head(lung), type = "survival", time = c(0, 500, 1000)),
    class = "defunctError"
  )

  rlang::local_options(lifecycle_verbosity = "quiet")
  pred <- predict(f_fit, head(lung), type = "survival", time = c(0, 500, 1000))
  expect_identical(pred, exp_pred)
})

test_that("deprecation of `time` arg for type 'hazard'", {
  f_fit <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  exp_pred <- predict(f_fit, head(lung), type = "hazard", eval_time = c(0, 500, 1000))

  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(
    predict(f_fit, head(lung), type = "hazard", time = c(0, 500, 1000)),
    class = "defunctError"
  )

  rlang::local_options(lifecycle_verbosity = "quiet")
  pred <- predict(f_fit, head(lung), type = "hazard", time = c(0, 500, 1000))
  expect_identical(pred, exp_pred)
})
