# registration ------------------------------------------------------------

test_that("engine is registered and translate() works", {
  skip_if_not_installed("randomForestSRC")

  engines <- parsnip::show_engines("rand_forest")
  censored_engines <- engines$engine[engines$mode == "censored regression"]
  expect_in("randomForestSRC", censored_engines)

  spec <- rand_forest(trees = 100, min_n = 5) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression")

  translated <- translate(spec)

  expect_equal(translated$method$fit$func[["fun"]], "rfsrc_train")
  expect_in(c("ntree", "nodesize"), names(translated$method$fit$args))
  expect_equal(rlang::eval_tidy(translated$method$fit$args$ntree), 100)
})

# fit ---------------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("randomForestSRC")

  mod_spec <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression")

  # parsnip fit on raw `lung` (status 1/2); the wrapper normalizes via `Surv()`
  set.seed(1)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  # reference: a direct fit on 0/1-normalized data builds the same forest
  set.seed(1)
  exp_f_fit <- randomForestSRC::rfsrc(
    Surv(time, status) ~ age + ph.ecog,
    data = dplyr::mutate(lung, status = status - 1),
    ntree = 50
  )

  expect_s3_class(f_fit$fit, "rfsrc")
  expect_equal(f_fit$fit$family, "surv")
  expect_equal(f_fit$fit$survival, exp_f_fit$survival)
})

# fit: response handling --------------------------------------------------

test_that("pre-made Surv response works", {
  skip_if_not_installed("randomForestSRC")

  d <- lung
  d$surv <- Surv(d$time, d$status)

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(surv ~ age + ph.ecog, data = d)

  expect_equal(f_fit$fit$family, "surv")
})

test_that("raw 1/2 status is normalized, not read as competing risks", {
  skip_if_not_installed("randomForestSRC")

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_equal(f_fit$fit$family, "surv")
})

test_that("a `.` right-hand side does not leak the outcome columns", {
  skip_if_not_installed("randomForestSRC")

  d <- lung[, c("time", "status", "age", "ph.ecog", "sex")]

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ ., data = d)

  expect_setequal(f_fit$fit$xvar.names, c("age", "ph.ecog", "sex"))
})

test_that("injected response column names avoid collision", {
  skip_if_not_installed("randomForestSRC")

  d <- lung[, c("time", "status", "age", "ph.ecog")]
  d$..y_time <- d$age

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ ., data = d)

  expect_equal(f_fit$fit$family, "surv")
  expect_in("..y_time", f_fit$fit$xvar.names)
})

test_that("non-right-censored responses error", {
  skip_if_not_installed("randomForestSRC")

  d <- na.omit(lung[, c("time", "status", "age", "ph.ecog")])

  d_cr <- d
  d_cr$y <- Surv(
    d$time,
    factor(rep(c("censor", "a", "b"), length.out = nrow(d)))
  )

  d_interval <- d
  d_interval$y <- Surv(d$time, d$time + 5, type = "interval2")

  d_counting <- d
  d_counting$y <- Surv(rep(0, nrow(d)), d$time, d$status)

  spec <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression")

  expect_snapshot(error = TRUE, {
    fit(spec, y ~ age + ph.ecog, data = d_cr)
  })
  expect_snapshot(error = TRUE, {
    fit(spec, y ~ age + ph.ecog, data = d_interval)
  })
  expect_snapshot(error = TRUE, {
    fit(spec, y ~ age + ph.ecog, data = d_counting)
  })
})

# prediction: time --------------------------------------------------------

test_that("survival_time_rfsrc() works", {
  skip_if_not_installed("randomForestSRC")

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)

  new_data <- lung[1:3, ]
  result <- survival_time_rfsrc(f_fit, new_data)

  expect_type(result, "double")
  expect_length(result, nrow(new_data))

  engine_fit <- hardhat::extract_fit_engine(f_fit)
  pred <- predict(engine_fit, newdata = new_data, na.action = "na.impute")
  times <- pred$time.interest
  expected <- apply(pred$survival, 1, function(srow) {
    below <- which(srow <= 0.5)
    if (length(below) == 0) NA_real_ else times[min(below)]
  })
  expect_equal(result, expected)

  # a non-crossing curve gives NA, never Inf (unlike ranger)
  expect_false(any(is.infinite(result)))

  # single observation
  result_1 <- survival_time_rfsrc(f_fit, lung[1, ])
  expect_length(result_1, 1)
})

test_that("time predictions", {
  skip_if_not_installed("randomForestSRC")

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)

  # complete-predictor rows so the deterministic path is compared (na.impute is
  # stochastic; its row-keeping behavior is covered separately below)
  new_data <- na.omit(lung)[1:5, ]

  f_pred <- predict(f_fit, new_data, type = "time")

  expect_s3_class(f_pred, "tbl_df")
  expect_named(f_pred, ".pred_time")
  expect_equal(nrow(f_pred), nrow(new_data))
  expect_equal(f_pred$.pred_time, survival_time_rfsrc(f_fit, new_data))

  # single observation
  f_pred_1 <- predict(f_fit, new_data[1, ], type = "time")
  expect_equal(nrow(f_pred_1), 1L)
})

# prediction: survival ----------------------------------------------------

test_that("survival_prob_rfsrc() works", {
  skip_if_not_installed("randomForestSRC")

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)

  new_data <- na.omit(lung)[1:5, ]
  eval_time <- c(0, 100, 300, 1000)

  result <- survival_prob_rfsrc(f_fit, new_data, eval_time)

  expect_s3_class(result, "tbl_df")
  expect_named(result, ".pred")
  expect_equal(nrow(result), nrow(new_data))
  expect_all_equal(purrr::map_int(result$.pred, nrow), length(eval_time))
  expect_all_true(purrr::map_lgl(
    result$.pred,
    \(x) identical(names(x), c(".eval_time", ".pred_survival"))
  ))

  # S = 1 at eval_time = 0 (before any event)
  expect_all_true(purrr::map_dbl(result$.pred, \(x) x$.pred_survival[[1]]) == 1)

  # hand computation via step-function lookup
  engine_fit <- hardhat::extract_fit_engine(f_fit)
  pred <- predict(engine_fit, newdata = new_data, na.action = "na.impute")
  times <- pred$time.interest
  idx <- findInterval(eval_time, times)
  expected <- matrix(1, nrow = nrow(new_data), ncol = length(eval_time))
  for (j in seq_along(eval_time)) {
    if (idx[j] >= 1) {
      expected[, j] <- pred$survival[, idx[j]]
    }
  }
  expect_equal(
    matrix(
      tidyr::unnest(result, cols = c(.pred))$.pred_survival,
      ncol = length(eval_time),
      byrow = TRUE
    ),
    expected
  )

  # single row
  result_1 <- survival_prob_rfsrc(f_fit, new_data[1, ], eval_time)
  expect_equal(nrow(result_1), 1L)
})

test_that("survival predictions", {
  skip_if_not_installed("randomForestSRC")

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)

  new_data <- na.omit(lung)[1:5, ]
  eval_time <- c(100, 300)

  f_pred <- predict(f_fit, new_data, type = "survival", eval_time = eval_time)

  expect_s3_class(f_pred, "tbl_df")
  expect_named(f_pred, ".pred")
  expect_equal(nrow(f_pred), nrow(new_data))
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.eval_time,
    rep(eval_time, nrow(new_data))
  )
  expect_equal(f_pred, survival_prob_rfsrc(f_fit, new_data, eval_time))

  # single observation
  f_pred_1 <- predict(
    f_fit,
    new_data[1, ],
    type = "survival",
    eval_time = eval_time
  )
  expect_equal(nrow(f_pred_1), 1L)
})

test_that("survival predictions - error snapshot", {
  skip_if_not_installed("parsnip", minimum_version = "1.3.0")
  skip_if_not_installed("randomForestSRC")

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_snapshot(error = TRUE, {
    predict(f_fit, lung, type = "survival")
  })
})

test_that("can predict for out-of-domain timepoints", {
  skip_if_not_installed("randomForestSRC")

  # 1022 is the largest event time in lung; 2000 is beyond observed follow-up
  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[2, ]

  mod <- rand_forest(trees = 50) |>
    set_mode("censored regression") |>
    set_engine("randomForestSRC") |>
    fit(Surv(time, status) ~ ., data = lung)

  expect_no_error(
    predict(
      mod,
      obs_without_NA,
      type = "survival",
      eval_time = eval_time_obs_max_and_ood
    )
  )
})

# fit via matrix interface ------------------------------------------------

test_that("`fit_xy()` works", {
  skip_if_not_installed("randomForestSRC")

  lung_no_na <- na.omit(lung)
  lung_x <- as.matrix(lung_no_na[, c("age", "ph.ecog")])
  lung_y <- Surv(lung_no_na$time, lung_no_na$status)
  lung_pred <- lung_no_na[1:5, ]

  spec <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression")

  set.seed(1)
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung_no_na)
  set.seed(1)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  expect_s3_class(xy_fit$fit, "rfsrc")
  expect_equal(xy_fit$fit$survival, f_fit$fit$survival)

  f_pred_time <- predict(f_fit, lung_pred, type = "time")
  xy_pred_time <- predict(xy_fit, lung_pred, type = "time")
  expect_equal(f_pred_time, xy_pred_time)

  f_pred_surv <- predict(
    f_fit,
    lung_pred,
    type = "survival",
    eval_time = c(100, 200)
  )
  xy_pred_surv <- predict(
    xy_fit,
    lung_pred,
    type = "survival",
    eval_time = c(100, 200)
  )
  expect_equal(f_pred_surv, xy_pred_surv)
})

# missing data ------------------------------------------------------------

test_that("missing predictors don't drop rows", {
  skip_if_not_installed("randomForestSRC")

  f_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)

  new_data <- na.omit(lung)[1:5, ]
  new_data$age[2] <- NA
  eval_time <- c(100, 300)

  f_pred_surv <- predict(
    f_fit,
    new_data,
    type = "survival",
    eval_time = eval_time
  )
  expect_equal(nrow(f_pred_surv), nrow(new_data))
  expect_all_true(is.finite(f_pred_surv$.pred[[2]]$.pred_survival))

  f_pred_time <- predict(f_fit, new_data, type = "time")
  expect_equal(nrow(f_pred_time), nrow(new_data))
})

# tuning ------------------------------------------------------------------

test_that("tuning parameters are inherited", {
  skip_if_not_installed("randomForestSRC")

  spec <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression")

  params <- hardhat::extract_parameter_set_dials(spec)

  expect_setequal(params$name, c("mtry", "trees", "min_n"))
})

# case weights ------------------------------------------------------------

test_that("can handle case weights", {
  skip_if_not_installed("randomForestSRC")

  dat <- make_cens_wts()

  set.seed(1)
  wt_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, event) ~ ., data = dat$full, case_weights = dat$wts)

  set.seed(1)
  unwt_fit <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression") |>
    fit(Surv(time, event) ~ ., data = dat$full)

  expect_unequal(wt_fit$fit$survival, unwt_fit$fit$survival)
})

test_that("case weights change predictions", {
  skip_if_not_installed("randomForestSRC")

  dat <- make_cens_wts()

  spec <- rand_forest(trees = 50) |>
    set_engine("randomForestSRC") |>
    set_mode("censored regression")

  set.seed(1)
  wt_fit <- fit(
    spec,
    Surv(time, event) ~ .,
    data = dat$full,
    case_weights = dat$wts
  )
  set.seed(1)
  unwt_fit <- fit(spec, Surv(time, event) ~ ., data = dat$full)

  new_data <- dat$full
  eval_time <- c(100, 300)

  expect_unequal(
    predict(wt_fit, new_data, type = "time"),
    predict(unwt_fit, new_data, type = "time")
  )
  expect_unequal(
    predict(wt_fit, new_data, type = "survival", eval_time = eval_time),
    predict(unwt_fit, new_data, type = "survival", eval_time = eval_time)
  )
})

# input checks ------------------------------------------------------------

test_that("prediction helpers error informatively on bad input", {
  skip_if_not_installed("randomForestSRC")

  raw_fit <- randomForestSRC::rfsrc(
    Surv(time, status) ~ age + ph.ecog,
    data = dplyr::mutate(na.omit(lung), status = status - 1)
  )
  wrong_engine <- structure(
    list(fit = structure(list(), class = "coxph")),
    class = "model_fit"
  )

  expect_snapshot(
    error = TRUE,
    survival_time_rfsrc(raw_fit, new_data = lung[1:3, ])
  )
  expect_snapshot(
    error = TRUE,
    survival_time_rfsrc(wrong_engine, new_data = lung[1:3, ])
  )
  expect_snapshot(
    error = TRUE,
    survival_prob_rfsrc(raw_fit, new_data = lung[1:3, ], eval_time = 100)
  )
  expect_snapshot(
    error = TRUE,
    survival_prob_rfsrc(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
  )
})

test_that("survival_prob_rfsrc() accepts eval_time values that it can handle", {
  skip_if_not_installed("randomForestSRC")

  mod <- rand_forest(trees = 50) |>
    set_mode("censored regression") |>
    set_engine("randomForestSRC") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)
  new_data <- lung[1:2, ]

  # eval_time handling is delegated to survival_curve_to_prob(), so the
  # handleable values match the mboost engine, which shares that helper
  expect_no_error(
    survival_prob_rfsrc(mod, new_data = new_data, eval_time = numeric(0))
  )
  expect_no_error(
    survival_prob_rfsrc(mod, new_data = new_data, eval_time = c(100, NA))
  )
  expect_no_error(
    survival_prob_rfsrc(mod, new_data = new_data, eval_time = c(100, Inf))
  )
  expect_no_error(
    survival_prob_rfsrc(mod, new_data = new_data, eval_time = c(100, -Inf))
  )
  expect_no_error(
    survival_prob_rfsrc(mod, new_data = new_data, eval_time = c(100, -50))
  )
  expect_no_error(
    survival_prob_rfsrc(mod, new_data = new_data, eval_time = c(100, 100, 200))
  )
})
