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


# prediction: time --------------------------------------------------------

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

  # single observation
  f_pred_1 <- predict(f_fit, lung[2,], type = "time")
  expect_identical(nrow(f_pred_1), 1L)
})


# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  set.seed(1234)
  exp_f_fit <- pec::pecRpart(Surv(time, status) ~ age + ph.ecog, data = lung)

  cox_spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("rpart")
  set.seed(1234)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_error(
    predict(f_fit, lung, type = "survival"),
    "When using `type` values of 'survival' or 'hazard', a numeric vector"
  )

  f_pred <- predict(f_fit, lung, type = "survival", eval_time = 100:200)
  exp_f_pred <- pec::predictSurvProb(exp_f_fit, lung, times = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(
      f_pred$.pred,
      ~ all(dim(.x) == c(101, 2))
    ))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(names(.x) == c(".eval_time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.eval_time,
    rep(100:200, nrow(lung))
  )

  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    as.numeric(t(exp_f_pred))
  )

  # single observation
  f_pred <- predict(f_fit, lung[2,], type = "survival", eval_time = 100:200)
  expect_identical(nrow(f_pred), 1L)
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(names(.x) == c(".eval_time", ".pred_survival"))))
  )
  expect_equal(f_pred$.pred[[1]]$.eval_time, 100:200)
})


# fit via matrix interface ------------------------------------------------

test_that("`fix_xy()` works", {
  skip_if_not_installed("prodlim", minimum_version = "2023.3.31")

  lung_x <- as.matrix(lung[, c("age", "ph.ecog")])
  lung_y <- Surv(lung$time, lung$status)
  lung_pred <- lung[1:5, ]

  spec <- decision_tree() %>%
    set_mode("censored regression") %>%
    set_engine("rpart")
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  elements_to_ignore_rpart <- c("call", "terms")
  elements_to_ignore_survfit <- "formula"
  f_fit_modified <- f_fit$fit
  xy_fit_modified <- xy_fit$fit
  f_fit_modified$rpart[elements_to_ignore_rpart] <- NULL
  xy_fit_modified$rpart[elements_to_ignore_rpart] <- NULL
  f_fit_modified$survfit[elements_to_ignore_survfit] <- NULL
  xy_fit_modified$survfit[elements_to_ignore_survfit] <- NULL
  expect_equal(
    f_fit_modified$survfit,
    xy_fit_modified$survfit
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
