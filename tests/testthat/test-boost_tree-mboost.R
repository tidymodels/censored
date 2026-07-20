library(testthat)

test_that("model object", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  exp_f_fit <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung2,
    family = mboost::CoxPH()
  )

  # formula method
  cox_spec <- boost_tree() |>
    set_engine("mboost") |>
    set_mode("censored regression")
  expect_no_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2)
  )

  # Removing `call` element from both
  expect_equal(
    unclass(f_fit$fit)[-24],
    unclass(exp_f_fit)[-24],
    ignore_function_env = TRUE
  )
})

# prediction: time --------------------------------------------------------

test_that("time predictions", {
  skip_if_not_installed("mboost")

  cox_spec <- boost_tree() |>
    set_engine("mboost") |>
    set_mode("censored regression")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  f_pred <- predict(f_fit, lung, type = "time")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(nrow(f_pred), nrow(lung))

  # single observation
  # skip until mboost::survFit() works with a single row for `newdata`
  # fix submitted: https://github.com/boost-R/mboost/pull/118
  # expect_no_error(f_pred_1 <- predict(f_fit, lung[1,], type = "time"))
  # expect_equal(nrow(f_pred_1), 1)
})


# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  skip_if_not_installed("mboost")

  pred_time <- c(0, 100, 200, 10000)

  set.seed(403)
  exp_f_fit <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung,
    family = mboost::CoxPH()
  )
  cox_spec <- boost_tree() |>
    set_engine("mboost") |>
    set_mode("censored regression")
  set.seed(403)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  # move snapshot test below back here after parsnip v1.3.0 release

  set.seed(403)
  f_pred <- predict(f_fit, lung, type = "survival", eval_time = pred_time)
  set.seed(403)
  exp_survFit <- mboost::survFit(exp_f_fit, lung)
  exp_f_pred <- survival_curve_to_prob(
    eval_time = pred_time,
    event_times = exp_survFit$time,
    survival_prob = exp_survFit$surv
  )

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, \(.x) all(dim(.x) == c(4, 2))))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        \(.x) all(names(.x) == c(".eval_time", ".pred_survival"))
      )
    )
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.eval_time,
    rep(pred_time, nrow(lung))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    as.vector(exp_f_pred)
  )
})

test_that("survival predictions - error snapshot", {
  skip_if_not_installed("parsnip", minimum_version = "1.3.0")
  skip_if_not_installed("mboost")

  cox_spec <- boost_tree() |>
    set_engine("mboost") |>
    set_mode("censored regression")
  set.seed(403)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_snapshot(error = TRUE, {
    predict(f_fit, lung, type = "survival")
  })
})

test_that("survival_curve_to_prob() works", {
  lung_pred <- tidyr::drop_na(lung)

  # make a survfit object for comparisons with summary.survfit()
  mod <- coxph(Surv(time, status) ~ ., data = lung)
  surv_fit <- survfit(mod, newdata = lung_pred)

  # general case
  pred_time_general <- c(100, 200)
  exp_prob <- summary(surv_fit, time = pred_time_general)$surv
  prob <- survival_curve_to_prob(
    eval_time = pred_time_general,
    event_times = surv_fit$time,
    survival_prob = surv_fit$surv
  )
  expect_equal(prob, exp_prob)

  # can handle unordered time
  pred_time_unordered <- c(300, 100, 200)
  exp_prob <- summary(surv_fit, time = pred_time_unordered)$surv
  prob <- survival_curve_to_prob(
    eval_time = pred_time_unordered,
    event_times = surv_fit$time,
    survival_prob = surv_fit$surv
  )
  expect_equal(prob, exp_prob)

  # can handle out of range time (before and after events)
  pred_time_extend <- c(-2, 0, 3000)
  exp_prob <- summary(surv_fit, time = pred_time_extend, extend = TRUE)$surv
  prob <- survival_curve_to_prob(
    eval_time = pred_time_extend,
    event_times = surv_fit$time,
    survival_prob = surv_fit$surv
  )
  expect_equal(prob, exp_prob)

  # can handle infinite time
  pred_time_inf <- c(-Inf, 0, Inf, 1022, -Inf)
  exp_prob <- summary(surv_fit, time = pred_time_inf, extend = TRUE)$surv
  prob <- survival_curve_to_prob(
    eval_time = pred_time_inf,
    event_times = surv_fit$time,
    survival_prob = surv_fit$surv
  )
  expect_equal(nrow(prob), length(pred_time_inf))

  expect_equal(prob[-3, ], exp_prob[-3, ])
  expect_equal(
    prob[3, ] |> unname(),
    rep(0, nrow(lung_pred))
  )
})

test_that("survival_prob_mboost() works", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  mod <- boost_tree() |>
    set_engine("mboost") |>
    set_mode("censored regression") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  # can handle missings
  pred <- survival_prob_mboost(
    mod,
    new_data = lung[14:15, ],
    eval_time = c(0, 100, 200)
  )
  expect_equal(nrow(pred), 2)

  # can handle single observation
  # skip until mboost::survFit() works with a single row for `newdata`
  # fix submitted: https://github.com/boost-R/mboost/pull/118
  # pred <- survival_prob_mboost(
  #   mod,
  #   new_data = lung[1,],
  #   eval_time = c(0, 100, 200)
  # )
  # expect_equal(nrow(pred), 1)
})

test_that("can predict for out-of-domain timepoints", {
  skip_if_not_installed("mboost")

  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[c(2, 4), ] # two observations because of https://github.com/boost-R/mboost/issues/117

  mod <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ ., data = lung)

  expect_no_error(
    preds <- predict(
      mod,
      obs_without_NA,
      type = "survival",
      eval_time = eval_time_obs_max_and_ood
    )
  )
})

# prediction: linear_pred -------------------------------------------------

test_that("linear_pred predictions", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  exp_f_fit <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung2,
    family = mboost::CoxPH()
  )
  cox_spec <- boost_tree() |>
    set_engine("mboost") |>
    set_mode("censored regression")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2)

  f_pred <- predict(f_fit, lung2, type = "linear_pred")
  exp_f_pred <- -unname(predict(exp_f_fit, newdata = lung2))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, as.vector(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))

  # don't flip the sign
  f_pred <- predict(f_fit, lung2, type = "linear_pred", increasing = FALSE)
  exp_f_pred <- unname(predict(exp_f_fit, newdata = lung2))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, as.vector(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))

  # single observation
  f_pred_1 <- predict(f_fit, lung2[1, ], type = "linear_pred")
  expect_identical(nrow(f_pred_1), 1L)
})


# fit via matrix interface ------------------------------------------------

test_that("`fix_xy()` works", {
  skip_if_not_installed("mboost")

  lung_x <- as.matrix(lung[, c("age", "ph.ecog")])
  lung_y <- Surv(lung$time, lung$status)
  lung_pred <- lung[1:5, ]

  spec <- boost_tree() |>
    set_engine("mboost") |>
    set_mode("censored regression")
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  expect_equal(
    f_fit$fit,
    xy_fit$fit,
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

  f_pred_lp <- predict(f_fit, new_data = lung_pred, type = "linear_pred")
  xy_pred_lp <- predict(xy_fit, new_data = lung_pred, type = "linear_pred")
  expect_equal(f_pred_lp, xy_pred_lp)
})

# input checks ------------------------------------------------------------

test_that("survival_time_mboost() errors informatively on bad input", {
  skip_if_not_installed("mboost")

  raw_fit <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung[-14, ],
    family = mboost::CoxPH()
  )
  wrong_engine <- structure(
    list(fit = structure(list(), class = "coxph")),
    class = "model_fit"
  )

  expect_snapshot(error = TRUE, survival_time_mboost(raw_fit))
  expect_snapshot(error = TRUE, survival_time_mboost(wrong_engine))
})

test_that("survival_prob_mboost() errors informatively on bad input", {
  skip_if_not_installed("mboost")

  raw_fit <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung[-14, ],
    family = mboost::CoxPH()
  )
  wrong_engine <- structure(
    list(fit = structure(list(), class = "coxph")),
    class = "model_fit"
  )

  expect_snapshot(
    error = TRUE,
    survival_prob_mboost(raw_fit, new_data = lung[1:3, ], eval_time = 100)
  )
  expect_snapshot(
    error = TRUE,
    survival_prob_mboost(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
  )
})

test_that("survival_prob_mboost() accepts eval_time values that it can handle", {
  skip_if_not_installed("mboost")
  mod <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung[-14, ])
  new_data <- lung[1:2, ]

  expect_no_error(
    survival_prob_mboost(mod, new_data = new_data, eval_time = numeric(0))
  )
  expect_no_error(
    survival_prob_mboost(mod, new_data = new_data, eval_time = c(100, NA))
  )
  expect_no_error(
    survival_prob_mboost(mod, new_data = new_data, eval_time = c(100, Inf))
  )
  expect_no_error(
    survival_prob_mboost(mod, new_data = new_data, eval_time = c(100, -Inf))
  )
  expect_no_error(
    survival_prob_mboost(mod, new_data = new_data, eval_time = c(100, -50))
  )
  expect_no_error(
    survival_prob_mboost(
      mod,
      new_data = new_data,
      eval_time = c(100, 100, 200)
    )
  )
})

test_that("survival_prob_mboost() warns about deprecated `time` argument", {
  skip_if_not_installed("mboost")
  mod <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung[-14, ])
  new_data <- lung[1:2, ]

  expect_snapshot(
    pred_deprecated <- survival_prob_mboost(
      mod,
      new_data = new_data,
      time = 100
    )
  )
  expect_equal(
    pred_deprecated,
    survival_prob_mboost(mod, new_data = new_data, eval_time = 100)
  )
})

# `multi_predict()` works for all `type`s available for `predict()` -------

test_that("multi_predict(type = time)", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "time",
    trees = c(25, 100)
  )

  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      \(.x) all(names(.x) == c("trees", ".pred_time"))
    ))
  )
  expect_equal(
    purrr::map_int(pred_multi$.pred, nrow),
    rep(2L, nrow(new_data_3))
  )
})

test_that("multi_predict(type = survival) for multiple eval_time points", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(100, 500),
    trees = c(25, 100)
  )

  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      \(.x) all(names(.x) == c("trees", ".eval_time", ".pred_survival"))
    ))
  )
  expect_equal(
    purrr::map_int(pred_multi$.pred, nrow),
    rep(4L, nrow(new_data_3))
  )
})

test_that("multi_predict(type = survival) for a single eval_time", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = 100,
    trees = c(25, 100)
  )

  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      \(.x) all(names(.x) == c("trees", ".eval_time", ".pred_survival"))
    ))
  )
  expect_equal(
    purrr::map_int(pred_multi$.pred, nrow),
    rep(2L, nrow(new_data_3))
  )
})

test_that("multi_predict(type = time) values match per-tree predict()", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "time",
    trees = c(25, 100)
  )
  unnested <- tidyr::unnest(pred_multi, cols = .pred)

  # mboost objects share mutable state, so mutating `engine_fit` also mutates
  # `f_fit$fit` — that's how `predict(f_fit, ...)` sees the subset iterations.
  engine_fit <- hardhat::extract_fit_engine(f_fit)
  mstop_original <- mboost::mstop(engine_fit)
  engine_fit[25]
  exp_25 <- predict(f_fit, new_data = new_data_3, type = "time")
  engine_fit[100]
  exp_100 <- predict(f_fit, new_data = new_data_3, type = "time")
  engine_fit[mstop_original]

  expect_equal(
    unnested$.pred_time[unnested$trees == 25],
    exp_25$.pred_time
  )
  expect_equal(
    unnested$.pred_time[unnested$trees == 100],
    exp_100$.pred_time
  )
})

test_that("multi_predict(type = survival) values match per-tree predict()", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]
  eval_time <- c(100, 500)

  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = eval_time,
    trees = c(25, 100)
  )
  unnested <- tidyr::unnest(pred_multi, cols = .pred)

  # mboost objects share mutable state, so mutating `engine_fit` also mutates
  # `f_fit$fit` — that's how `predict(f_fit, ...)` sees the subset iterations.
  engine_fit <- hardhat::extract_fit_engine(f_fit)
  mstop_original <- mboost::mstop(engine_fit)
  engine_fit[25]
  exp_25 <- predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = eval_time
  ) |>
    tidyr::unnest(cols = .pred)
  engine_fit[100]
  exp_100 <- predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = eval_time
  ) |>
    tidyr::unnest(cols = .pred)
  engine_fit[mstop_original]

  expect_equal(
    unnested$.pred_survival[unnested$trees == 25],
    exp_25$.pred_survival
  )
  expect_equal(
    unnested$.pred_survival[unnested$trees == 100],
    exp_100$.pred_survival
  )
})

test_that("multi_predict(type = linear_pred) values match per-tree predict()", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "linear_pred",
    trees = c(25, 100)
  )
  unnested <- tidyr::unnest(pred_multi, cols = .pred)

  # mboost objects share mutable state, so mutating `engine_fit` also mutates
  # `f_fit$fit` — that's how `predict(f_fit, ...)` sees the subset iterations.
  engine_fit <- hardhat::extract_fit_engine(f_fit)
  mstop_original <- mboost::mstop(engine_fit)
  engine_fit[25]
  exp_25 <- predict(f_fit, new_data = new_data_3, type = "linear_pred")
  engine_fit[100]
  exp_100 <- predict(f_fit, new_data = new_data_3, type = "linear_pred")
  engine_fit[mstop_original]

  expect_equal(
    unnested$.pred_linear_pred[unnested$trees == 25],
    exp_25$.pred_linear_pred
  )
  expect_equal(
    unnested$.pred_linear_pred[unnested$trees == 100],
    exp_100$.pred_linear_pred
  )

  # increasing = FALSE forwarded via opts
  pred_multi_raw <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "linear_pred",
    trees = c(25, 100),
    increasing = FALSE
  )
  unnested_raw <- tidyr::unnest(pred_multi_raw, cols = .pred)
  expect_equal(
    unnested_raw$.pred_linear_pred,
    -unnested$.pred_linear_pred
  )
})

test_that("multi_predict() works with a single `trees` value", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_time <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "time",
    trees = 50
  )
  expect_equal(names(pred_time), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_time$.pred,
      \(.x) all(names(.x) == c("trees", ".pred_time"))
    ))
  )
  expect_equal(
    purrr::map_int(pred_time$.pred, nrow),
    rep(1L, nrow(new_data_3))
  )

  pred_surv <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(100, 500),
    trees = 50
  )
  expect_true(
    all(purrr::map_lgl(
      pred_surv$.pred,
      \(.x) all(names(.x) == c("trees", ".eval_time", ".pred_survival"))
    ))
  )
  expect_equal(
    purrr::map_int(pred_surv$.pred, nrow),
    rep(2L, nrow(new_data_3))
  )

  pred_lp <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "linear_pred",
    trees = 50
  )
  expect_true(
    all(purrr::map_lgl(
      pred_lp$.pred,
      \(.x) all(names(.x) == c("trees", ".pred_linear_pred"))
    ))
  )
  expect_equal(
    purrr::map_int(pred_lp$.pred, nrow),
    rep(1L, nrow(new_data_3))
  )
})

test_that("multi_predict(type = linear_pred)", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "linear_pred",
    trees = c(25, 100)
  )

  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      \(.x) all(names(.x) == c("trees", ".pred_linear_pred"))
    ))
  )
  expect_equal(
    purrr::map_int(pred_multi$.pred, nrow),
    rep(2L, nrow(new_data_3))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1, ],
    type = "linear_pred",
    trees = c(25, 100)
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_equal(nrow(pred_multi_1), 1L)
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      \(.x) all(names(.x) == c("trees", ".pred_linear_pred"))
    ))
  )
  expect_equal(purrr::map_int(pred_multi_1$.pred, nrow), 2L)
})

test_that("multi_predict() reports `trees` as a submodel arg", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  expect_equal(multi_predict_args(f_fit), "trees")
  expect_true(has_multi_predict(f_fit))
})

test_that("multi_predict() leaves the fitted model's mstop unchanged", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]
  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)
  # mboost objects share mutable state: `multi_predict()` mutates the engine fit
  # while iterating over `trees` and must reset it before returning, otherwise
  # `f_fit$fit` would be left at the last `trees` value.
  engine_fit <- hardhat::extract_fit_engine(f_fit)
  mstop_before <- mboost::mstop(engine_fit)

  multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "time",
    trees = c(25, 100)
  )
  expect_equal(mboost::mstop(engine_fit), mstop_before)

  multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(100, 500),
    trees = c(25, 100)
  )
  expect_equal(mboost::mstop(engine_fit), mstop_before)

  multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "linear_pred",
    trees = c(25, 100)
  )
  expect_equal(mboost::mstop(engine_fit), mstop_before)

  # `mstop` is also restored when the helper errors after the on.exit is set
  try(
    multi_predict(
      f_fit,
      new_data = new_data_3,
      type = "survival",
      trees = c(25, 100)
    ),
    silent = TRUE
  )
  expect_equal(mboost::mstop(engine_fit), mstop_before)
})

test_that("multi_predict() errors informatively on bad input", {
  skip_if_not_installed("mboost")

  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]
  set.seed(403)
  f_fit <- boost_tree() |>
    set_mode("censored regression") |>
    set_engine("mboost") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  # missing eval_time for type = "survival"
  expect_snapshot(
    error = TRUE,
    multi_predict(
      f_fit,
      new_data = new_data_3,
      type = "survival",
      trees = c(25, 100)
    )
  )

  # eval_time supplied for a type that does not use it
  expect_snapshot(
    error = TRUE,
    multi_predict(
      f_fit,
      new_data = new_data_3,
      type = "time",
      eval_time = 100,
      trees = c(25, 100)
    )
  )

  # `trees` above the fitted model's mstop
  expect_snapshot(
    error = TRUE,
    multi_predict(
      f_fit,
      new_data = new_data_3,
      type = "time",
      trees = c(50, 10000)
    )
  )

  # non-positive `trees`
  expect_snapshot(
    error = TRUE,
    multi_predict(
      f_fit,
      new_data = new_data_3,
      type = "time",
      trees = c(0, 50)
    )
  )

  # non-integer `trees`
  expect_snapshot(
    error = TRUE,
    multi_predict(
      f_fit,
      new_data = new_data_3,
      type = "time",
      trees = c(1.5, 50)
    )
  )
})
