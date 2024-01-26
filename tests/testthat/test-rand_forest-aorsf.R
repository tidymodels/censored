library(testthat)

test_that("model object", {
  skip_if_not_installed("aorsf")

  lung_orsf <- na.omit(lung)

  set.seed(1234)
  exp_f_fit <- aorsf::orsf(
    data = lung_orsf,
    formula = Surv(time, status) ~ age + ph.ecog
  )

  # formula method
  mod_spec <- rand_forest() %>%
    set_engine("aorsf") %>%
    set_mode("censored regression")

  set.seed(1234)
  expect_error(
    f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung_orsf),
    NA
  )

  expect_equal(
    f_fit$fit,
    exp_f_fit,
    ignore_formula_env = TRUE
  )
})

# prediction: time --------------------------------------------------------

test_that("time predictions", {
  skip_if_not_installed("aorsf", "0.1.2")

  lung_orsf <- na.omit(lung)

  set.seed(1234)
  exp_f_fit <- aorsf::orsf(
    data = lung_orsf,
    formula = Surv(time, status) ~ age + ph.ecog
  )
  exp_f_pred <- predict(
    exp_f_fit, 
    new_data = lung,
    pred_type = "time",
    na_action = "pass"
  )

  mod_spec <- rand_forest() %>%
    set_engine("aorsf") %>%
    set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung_orsf)
  f_pred <- predict(f_fit, lung, type = "time")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(f_pred$.pred_time, as.vector(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))

  # single observation
  f_pred_1 <- predict(f_fit, lung[2,], type = "time")
  expect_identical(nrow(f_pred_1), 1L)
})

# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  skip_if_not_installed("aorsf")

  lung_orsf <- na.omit(lung)

  set.seed(1234)
  exp_f_fit <- aorsf::orsf(
    data = lung_orsf,
    formula = Surv(time, status) ~ age + ph.ecog
  )

  mod_spec <- rand_forest() %>%
    set_engine("aorsf") %>%
    set_mode("censored regression")

  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung_orsf)

  expect_error(
    predict(f_fit, lung_orsf, type = "survival"),
    "When using `type` values of 'survival' or 'hazard', a numeric vector"
  )

  f_pred <- predict(f_fit, lung, type = "survival", eval_time = c(100, 500, 1200))

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_equal(
    unique(purrr::map_int(f_pred$.pred, nrow)),
    3
  )

  cf_names <-
    c(".eval_time", ".pred_survival")

  expect_true(
    all(purrr::map_lgl(
      f_pred$.pred,
      ~ identical(names(.x), cf_names)
    ))
  )

  # correct prediction times in object
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.eval_time,
    rep(c(100, 500, 1200), nrow(lung))
  )

  # comparing predictions b/t aorsf & parnsip fit

  # equal predictions with multiple times and multiple testing observations
  new_km <- predict(
    exp_f_fit,
    new_data = lung,
    pred_type = "surv",
    pred_horizon = c(100, 500, 1200),
    na_action = "pass",
    boundary_checks = FALSE
  )

  expect_equal(
    matrix(
      data = tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
      ncol = 3,
      byrow = TRUE
    ),
    new_km
  )

  # equal predictions with multiple times and one testing observation
  f_pred <- predict(f_fit, lung[1, ], type = "survival", eval_time = c(100, 500, 1200))

  new_km <- predict(
    exp_f_fit,
    new_data = lung[1, ],
    pred_type = "surv",
    pred_horizon = c(100, 500, 1200),
    na_action = "pass",
    boundary_checks = FALSE
  )

  expect_equal(
    matrix(
      data = tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
      ncol = 3,
      byrow = TRUE
    ),
    new_km
  )

  # equal predictions with one time and multiple testing observation
  f_pred <- predict(f_fit, lung, type = "survival", eval_time = 306)

  new_km <- predict(
    exp_f_fit,
    new_data = lung,
    pred_type = "surv",
    pred_horizon = 306,
    na_action = "pass"
  )

  expect_equal(
    matrix(
      data = tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
      ncol = 1,
      byrow = TRUE
    ),
    new_km
  )

  # equal predictions with one time and one testing observation
  f_pred <- predict(f_fit, lung[1, ], type = "survival", eval_time = 306)

  new_km <- predict(
    exp_f_fit,
    new_data = lung[1, ],
    pred_type = "surv",
    pred_horizon = 306,
    na_action = "pass"
  )[1, 1]

  expect_equal(
    f_pred$.pred[[1]]$.pred_survival,
    new_km
  )
})

test_that("can predict for out-of-domain timepoints", {
  skip_if_not_installed("aorsf")

  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[2,]
  lung_orsf <- na.omit(lung)

  mod <- rand_forest() %>%
    set_mode("censored regression") %>%
    set_engine("aorsf") %>%
    fit(Surv(time, status) ~ ., data = lung_orsf)

  expect_no_error(
    preds <- predict(mod, obs_without_NA, type = "survival", eval_time = eval_time_obs_max_and_ood)
  )
})

# fit via matrix interface ------------------------------------------------

test_that("`fix_xy()` works", {
  skip_if_not_installed("aorsf")

  lung_orsf <- na.omit(lung)

  lung_x <- as.matrix(lung_orsf[, c("age", "ph.ecog")])
  lung_y <- Surv(lung_orsf$time, lung_orsf$status)
  lung_pred <- lung_orsf[1:5, ]

  spec <- rand_forest() %>%
    set_engine("aorsf") %>%
    set_mode("censored regression")
  set.seed(1)
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung_orsf)
  set.seed(1)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  if (utils::packageVersion("aorsf") >= "0.1.2") {
    f_fit_modified <- f_fit$fit$forest
    xy_fit_modified <- xy_fit$fit$forest
  } else {
    elements_to_ignore <- "data"
    f_fit_modified <- f_fit$fit
    xy_fit_modified <- xy_fit$fit
    f_fit_modified[elements_to_ignore] <- NULL
    xy_fit_modified[elements_to_ignore] <- NULL
  }

  expect_equal(
    f_fit_modified,
    xy_fit_modified,
    ignore_attr = c("names_y", "max_time"),
    ignore_function_env = TRUE
  )

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


# case weights ------------------------------------------------------------

test_that("can handle case weights", {
  skip_if_not_installed("aorsf")
  
  dat <- make_cens_wts()

  expect_error(
    {
      wt_fit <- rand_forest() %>%
        set_engine("aorsf") %>%
        set_mode("censored regression") %>%
        fit(Surv(time, event) ~ ., data = dat$full, case_weights = dat$wts)
    },
    regexp = NA
  )

  if (utils::packageVersion("aorsf") >= "0.1.2") {
    fit_weights <- wt_fit$fit$weights
  } else {
    fit_weights <- attr(wt_fit$fit, "weights_user")
  }

  expect_equal(
    fit_weights,
    as.vector(dat$wts)
  )
})
