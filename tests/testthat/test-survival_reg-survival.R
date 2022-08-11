library(testthat)

test_that("survival execution", {

  expect_error(
    res <- survival_reg() %>%
      set_engine("survival") %>%
      fit(Surv(time, status) ~ age + sex, data = lung),
    regexp = NA
  )
  expect_output(print(res), "parsnip model object")

  expect_error(
    res <- survival_reg(dist = "lognormal") %>%
      set_engine("survival") %>%
      fit(Surv(time) ~ age + sex, data = lung),
    regexp = NA
  )

  expect_error(
    survival_reg() %>%
      set_engine("survival") %>%
      fit_xy(x = lung[, c("age", "sex")], y = lung$time)
  )
})

test_that("survival time prediction", {
  res <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  exp_pred <- predict(res$fit, head(lung))
  exp_pred <- tibble::tibble(.pred_time = unname(exp_pred))
  expect_equal(exp_pred, predict(res, head(lung)))
})

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
})

test_that("survival probability prediction", {
  rms_surv <- readRDS(test_path("data", "rms_surv.rds"))
  res <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_error(
    predict(res, head(lung), type = "survival"),
    "a numeric vector 'time'"
  )

  exp_pred <- predict(res, head(lung), type = "survival", time = c(0, 500, 1000))
  exp_pred_vert <- exp_pred %>%
    dplyr::mutate(.patient = dplyr::row_number()) %>%
    tidyr::unnest(cols = .pred)

  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(names(exp_pred_vert), c(".time", ".pred_survival", ".patient"))

  # using rms for expected results
  expect_equal(
    exp_pred$.pred[[1]]$.pred_survival,
    rms_surv,
    tolerance = 0.001
  )
})

test_that("survival hazard prediction", {
  rms_haz <- readRDS(test_path("data", "rms_haz.rds"))
  res <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_error(
    predict(res, head(lung), type = "hazard"),
    "a numeric vector 'time'"
  )

  exp_pred <- predict(res, head(lung), type = "hazard", time = c(0, 500, 1000))
  exp_pred_vert <- exp_pred %>%
    dplyr::mutate(.patient = dplyr::row_number()) %>%
    tidyr::unnest(cols = .pred)

  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(names(exp_pred_vert), c(".time", ".pred_hazard", ".patient"))

  # using rms for expected results
  expect_equal(
    exp_pred$.pred[[1]]$.pred_hazard[-1],
    rms_haz[-1],
    tolerance = 0.001
  )
})

test_that("linear predictor", {
  f_fit <- survival_reg() %>%
    set_engine("survival") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung[1:5,], type = "linear_pred")

  exp_fit <- survreg(Surv(time, status) ~ age + sex, data = lung)
  exp_pred <- predict(exp_fit, lung[1:5,], type = "linear")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, unname(exp_pred))
  expect_equal(nrow(f_pred), 5)
})
