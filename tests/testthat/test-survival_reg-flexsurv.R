library(testthat)

test_that("model object", {
  skip_if_not_installed("flexsurv")
  
  set.seed(1234)
  exp_f_fit <- flexsurv::flexsurvreg(
    Surv(time, status) ~ age + ph.ecog,
    data = lung,
    dist = "weibull"
  )

  mod_spec <- survival_reg() %>%
    set_engine("flexsurv") %>%
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

test_that("flexsurv time prediction", {
  skip_if_not_installed("flexsurv")

  exp_fit <- flexsurv::flexsurvreg(
    Surv(time, status) ~ age,
    data = lung,
    dist = "lognormal"
  )
  exp_pred <- predict(exp_fit, head(lung), type = "response")

  f_fit <- survival_reg(dist = "lognormal") %>%
    set_engine("flexsurv") %>%
    fit(Surv(time, status) ~ age, data = lung)
  f_pred <- predict(f_fit, head(lung), type = "time")

  expect_equal(f_pred, exp_pred)

  # single observation
  f_pred_1 <- predict(f_fit, lung[2,], type = "time")
  expect_identical(nrow(f_pred_1), 1L)
})


# prediction: survival ----------------------------------------------------

test_that("survival probability prediction", {
  skip_if_not_installed("flexsurv")

  rms_surv <- readRDS(test_path("data", "rms_surv.rds"))
  f_fit <- survival_reg(dist = "weibull") %>%
    set_engine("flexsurv") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_error(
    predict(f_fit, head(lung), type = "survival"),
    "a numeric vector `eval_time`"
  )

  f_pred <- predict(
    f_fit,
    head(lung),
    type = "survival",
    eval_time = c(0, 500, 1000)
  )

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(head(lung)))
  expect_true(
    all(purrr::map_lgl(
      f_pred$.pred,
      ~ all(dim(.x) == c(3, 2))
    ))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~ all(names(.x) == c(".eval_time", ".pred_survival"))
      )
    )
  )

  # using rms for expected results
  expect_equal(
    f_pred$.pred[[1]]$.pred_survival,
    rms_surv,
    tolerance = 0.001
  )

  # add confidence interval
  pred <- predict(
    f_fit,
    head(lung),
    type = "survival",
    eval_time = c(500, 1000),
    interval = "confidence",
    level = 0.7
  )
  expect_true(
    all(purrr::map_lgl(
      pred$.pred,
      ~ all(names(.x) == c(
        ".eval_time",
        ".pred_survival",
        ".pred_lower",
        ".pred_upper"
      ))
    ))
  )

  # single observation
  f_pred_1 <- predict(f_fit, lung[2,], type = "survival", eval_time = 100)
  expect_identical(nrow(f_pred_1), 1L)
})

test_that("survival probabilities for single eval time point", {
  skip_if_not_installed("flexsurv")

  f_fit <- survival_reg(engine = "flexsurv") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  pred <- predict(f_fit, lung[1:3, ], type = "survival", eval_time = 100)

  expect_identical(nrow(pred), 3L)
  expect_identical(names(pred), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred$.pred,
      ~ all(names(.x) == c(
        ".eval_time",
        ".pred_survival"
      ))
    ))
  )
})

test_that("can predict for out-of-domain timepoints", {
  skip_if_not_installed("flexsurv")
  
  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[2,]

  mod <- survival_reg() %>%
    set_mode("censored regression") %>%
    set_engine("flexsurv") %>%
    fit(Surv(time, status) ~ ., data = lung) %>%
    suppressWarnings()

  expect_no_error(
    preds <- predict(mod, obs_without_NA, type = "survival", eval_time = eval_time_obs_max_and_ood)
  )
  expect_no_error(
    preds <- predict(mod, obs_without_NA, type = "hazard", eval_time = eval_time_obs_max_and_ood)
  )
})

# prediction: linear_pred -------------------------------------------------

test_that("linear predictor", {
  skip_if_not_installed("flexsurv")

  f_fit <- survival_reg() %>%
    set_engine("flexsurv") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung[1:5, ], type = "linear_pred")

  exp_fit <- flexsurv::flexsurvreg(
    Surv(time, status) ~ age + sex,
    data = lung,
    dist = "weibull"
  )
  exp_pred <- predict(exp_fit, lung[1:5, ], type = "linear")

  expect_equal(f_pred$.pred_linear_pred, log(exp_pred$.pred_link))
  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(nrow(f_pred), 5)


  f_fit <- survival_reg(dist = "lnorm") %>%
    set_engine("flexsurv") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung[1:5, ], type = "linear_pred")

  exp_fit <- flexsurv::flexsurvreg(
    Surv(time, status) ~ age + sex,
    data = lung,
    dist = "lnorm"
  )
  exp_pred <- predict(exp_fit, lung[1:5, ], type = "linear")

  expect_equal(f_pred$.pred_linear_pred, exp_pred$.pred_link)

  # single observation
  f_pred_1 <- predict(f_fit, lung[2,], type = "linear_pred")
  expect_identical(nrow(f_pred_1), 1L)
})


# prediction: quantile ----------------------------------------------------

test_that("quantile predictions", {
  skip_if_not_installed("flexsurv")

  set.seed(1)
  fit_s <- survival_reg() %>%
    set_engine("flexsurv") %>%
    set_mode("censored regression") %>%
    fit(Surv(stop, event) ~ rx + size + enum, data = bladder)
  pred <- predict(fit_s, new_data = bladder[1:3, ], type = "quantile")

  set.seed(1)
  exp_fit <- flexsurv::flexsurvreg(
    Surv(stop, event) ~ rx + size + enum,
    data = bladder,
    dist = "weibull"
  )
  exp_pred <- summary(
    exp_fit,
    newdata = bladder[1:3, ],
    type = "quantile",
    quantiles = (1:9) / 10
  )

  expect_s3_class(pred, "tbl_df")
  expect_equal(names(pred), ".pred")
  expect_equal(nrow(pred), 3)
  expect_true(
    all(purrr::map_lgl(
      pred$.pred,
      ~ all(dim(.x) == c(9, 2))
    ))
  )
  expect_true(
    all(purrr::map_lgl(
      pred$.pred,
      ~ all(names(.x) == c(".quantile", ".pred_quantile"))
    ))
  )
  expect_equal(
    tidyr::unnest(pred, cols = .pred)$.pred_quantile,
    do.call(rbind, exp_pred)$est
  )

  # add confidence interval
  pred <- predict(fit_s,
    new_data = bladder[1:3, ], type = "quantile",
    interval = "confidence", level = 0.7
  )
  expect_true(
    all(purrr::map_lgl(
      pred$.pred,
      ~ all(names(.x) == c(
        ".quantile",
        ".pred_quantile",
        ".pred_lower",
        ".pred_upper"
      ))
    ))
  )

  # single observation
  f_pred_1 <- predict(fit_s, bladder[2,], type = "quantile")
  expect_identical(nrow(f_pred_1), 1L)
})

# prediction: hazard ------------------------------------------------------

test_that("hazard prediction", {
  skip_if_not_installed("flexsurv")

  rms_haz <- readRDS(test_path("data", "rms_haz.rds"))
  f_fit <- survival_reg(dist = "weibull") %>%
    set_engine("flexsurv") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_error(
    predict(f_fit, head(lung), type = "hazard"),
    "a numeric vector `eval_time`"
  )

  f_pred <- predict(
    f_fit,
    head(lung),
    type = "hazard",
    eval_time = c(0, 500, 1000)
  )

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(head(lung)))
  expect_true(
    all(purrr::map_lgl(
      f_pred$.pred,
      ~ all(dim(.x) == c(3, 2))
    ))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~ all(names(.x) == c(".eval_time", ".pred_hazard"))
      )
    )
  )

  # using rms for expected results
  expect_equal(
    f_pred$.pred[[1]]$.pred_hazard,
    rms_haz,
    tolerance = 0.001
  )

  # single observation
  f_pred_1 <- predict(f_fit, lung[2,], type = "hazard", eval_time = c(100, 200))
  expect_identical(nrow(f_pred_1), 1L)
})

test_that("hazard for single eval time point", {
  skip_if_not_installed("flexsurv")

  f_fit <- survival_reg(engine = "flexsurv") %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  pred <- predict(f_fit, lung[1:3, ], type = "hazard", eval_time = 100)

  expect_identical(nrow(pred), 3L)
  expect_identical(names(pred), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred$.pred,
      ~ all(names(.x) == c(
        ".eval_time",
        ".pred_hazard"
      ))
    ))
  )
})

# fit via matrix interface ------------------------------------------------

test_that("`fix_xy()` works", {
  skip_if_not_installed("flexsurv")
  
  lung_x <- as.matrix(lung[, c("age", "ph.ecog")])
  lung_y <- Surv(lung$time, lung$status)
  lung_pred <- lung[1:5, ]

  spec <- survival_reg() %>%
    set_engine("flexsurv") %>%
    set_mode("censored regression")
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  elements_to_ignore <- c(
    "call", "data", "concat.formula", "all.formulae", "covdata"
  )
  f_ignore <- which(names(f_fit$fit) %in% elements_to_ignore)
  xy_ignore <- which(names(xy_fit$fit) %in% elements_to_ignore)
  expect_equal(
    f_fit$fit[-f_ignore],
    xy_fit$fit[-xy_ignore]
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
