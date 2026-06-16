library(testthat)

# registration ------------------------------------------------------------

test_that("engine is registered and translate() works", {
  skip_if_not_installed("ranger")

  engines <- parsnip::show_engines("rand_forest")
  censored_engines <- engines$engine[engines$mode == "censored regression"]
  expect_in("ranger", censored_engines)

  spec <- rand_forest(trees = 100) |>
    set_engine("ranger") |>
    set_mode("censored regression")

  translated <- translate(spec)
  expect_equal(translated$method$fit$defaults$num.threads, 1)
  expect_equal(translated$method$fit$defaults$verbose, FALSE)
})

# fit ---------------------------------------------------------------------

test_that("model object", {
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  mod_spec <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression")

  expect_no_error(
    f_fit <- fit(
      mod_spec,
      Surv(time, status) ~ age + ph.ecog,
      data = lung_ranger
    )
  )

  expect_s3_class(f_fit$fit, "ranger")
  expect_equal(f_fit$fit$treetype, "Survival")
  expect_equal(f_fit$fit$num.trees, 100L)
})

# prediction: time --------------------------------------------------------

test_that("survival_time_ranger() returns correct values", {
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  f_fit <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung_ranger)

  new_data <- lung_ranger[1:5, ]

  result <- survival_time_ranger(f_fit, new_data)

  expect_type(result, "double")
  expect_length(result, nrow(new_data))

  engine_fit <- hardhat::extract_fit_engine(f_fit)
  pred <- predict(engine_fit, data = new_data)
  udt <- pred$unique.death.times
  expected <- apply(matrix(pred$survival, nrow = nrow(new_data)), 1, \(srow) {
    below <- which(srow <= 0.5)
    if (length(below) == 0) Inf else udt[min(below)]
  })
  expect_equal(result, expected)

  # single row
  result_1 <- survival_time_ranger(f_fit, lung_ranger[1, ])
  expect_length(result_1, 1)
})

test_that("time predictions", {
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  f_fit <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung_ranger)

  new_data <- lung_ranger[1:5, ]

  f_pred <- predict(f_fit, new_data, type = "time")

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred_time")
  expect_equal(nrow(f_pred), nrow(new_data))
  expect_equal(f_pred$.pred_time, survival_time_ranger(f_fit, new_data))

  # single observation
  f_pred_1 <- predict(f_fit, new_data[1, ], type = "time")
  expect_equal(nrow(f_pred_1), 1L)
})

# prediction: survival ----------------------------------------------------

test_that("survival_prob_ranger() returns correct values", {
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  f_fit <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung_ranger)

  new_data <- lung_ranger[1:5, ]
  eval_time <- c(0, 100, 300, 500, 1000)

  result <- survival_prob_ranger(f_fit, new_data, eval_time)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), ".pred")
  expect_equal(nrow(result), nrow(new_data))
  expect_all_equal(purrr::map_int(result$.pred, nrow), length(eval_time))
  expect_all_true(purrr::map_lgl(
    result$.pred,
    \(x) identical(names(x), c(".eval_time", ".pred_survival"))
  ))

  # S = 1 before first death time (eval_time = 0)
  expect_equal(result$.pred[[1]]$.pred_survival[[1]], 1)

  # hand computation
  engine_fit <- hardhat::extract_fit_engine(f_fit)
  pred <- predict(engine_fit, data = new_data)
  udt <- pred$unique.death.times
  idx <- findInterval(eval_time, udt)
  cols <- pmax(idx, 1L)
  prob_matrix <- matrix(pred$survival, nrow = nrow(new_data))[,
    cols,
    drop = FALSE
  ]
  prob_matrix[, idx == 0L] <- 1

  expect_equal(
    matrix(
      data = tidyr::unnest(result, cols = c(.pred))$.pred_survival,
      ncol = length(eval_time),
      byrow = TRUE
    ),
    prob_matrix
  )

  # single row
  result_1 <- survival_prob_ranger(f_fit, new_data[1, ], eval_time)
  expect_equal(nrow(result_1), 1L)
})

test_that("survival predictions", {
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  f_fit <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung_ranger)

  new_data <- lung_ranger[1:5, ]
  eval_time <- c(100, 300, 500)

  f_pred <- predict(f_fit, new_data, type = "survival", eval_time = eval_time)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(new_data))
  expect_all_equal(purrr::map_int(f_pred$.pred, nrow), length(eval_time))
  expect_all_true(purrr::map_lgl(
    f_pred$.pred,
    \(x) identical(names(x), c(".eval_time", ".pred_survival"))
  ))

  # eval_time repeats per row
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.eval_time,
    rep(eval_time, nrow(new_data))
  )

  # values match helper output
  expect_equal(f_pred, survival_prob_ranger(f_fit, new_data, eval_time))

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
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  f_fit <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung_ranger)

  expect_snapshot(error = TRUE, {
    predict(f_fit, lung_ranger, type = "survival")
  })
})

test_that("can predict for out-of-domain timepoints", {
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  f_fit <- rand_forest() |>
    set_engine("ranger") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ ., data = lung_ranger)

  expect_no_error(
    predict(
      f_fit,
      lung_ranger[1, ],
      type = "survival",
      eval_time = c(1022, 2000)
    )
  )
})

# fit via matrix interface ------------------------------------------------

test_that("`fit_xy()` works", {
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  lung_x <- as.matrix(lung_ranger[, c("age", "ph.ecog")])
  lung_y <- Surv(lung_ranger$time, lung_ranger$status)
  lung_pred <- lung_ranger[1:5, ]

  spec <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression")
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung_ranger)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  elements_to_ignore <- c(
    "call",
    "dependent.variable.name",
    "status.variable.name"
  )
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
})

# missing data -------------------------------------------------------------

test_that("missing predictors don't drop rows", {
  skip_if_not_installed("ranger")

  lung_ranger <- na.omit(lung)

  f_fit <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung_ranger)

  new_data <- lung_ranger[1:5, ]
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

# tuning --------------------------------------------------------------------

test_that("tuning parameters are inherited", {
  skip_if_not_installed("ranger")

  spec <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) |>
    set_engine("ranger", splitrule = tune()) |>
    set_mode("censored regression")

  params <- hardhat::extract_parameter_set_dials(spec)

  expect_setequal(params$name, c("mtry", "trees", "min_n", "splitrule"))
})

# case weights --------------------------------------------------------------

test_that("can handle case weights", {
  skip_if_not_installed("ranger")

  dat <- make_cens_wts()

  expect_no_error(
    wt_fit <- rand_forest(trees = 100) |>
      set_engine("ranger", seed = 1) |>
      set_mode("censored regression") |>
      fit(Surv(time, event) ~ ., data = dat$full, case_weights = dat$wts)
  )

  expect_snapshot(wt_fit$fit$call)

  unwt_fit <- rand_forest(trees = 100) |>
    set_engine("ranger", seed = 1) |>
    set_mode("censored regression") |>
    fit(Surv(time, event) ~ ., data = dat$full)

  expect_unequal(wt_fit$fit$survival, unwt_fit$fit$survival)
})
