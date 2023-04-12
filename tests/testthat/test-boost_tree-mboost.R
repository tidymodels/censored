library(testthat)

test_that("model object", {
  lung2 <- lung[-14, ]
  exp_f_fit <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung2,
    family = mboost::CoxPH()
  )

  # formula method
  cox_spec <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2),
    NA
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
  cox_spec <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  f_pred <- predict(f_fit, lung, type = "time")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(nrow(f_pred), nrow(lung))

  # single observation
  # skip until mboost::survFit() works with a single row for `newdata`
  # fix submitted: https://github.com/boost-R/mboost/pull/118
  # expect_error(f_pred_1 <- predict(f_fit, lung[1,], type = "time"), NA)
  # expect_equal(nrow(f_pred_1), 1)
})


# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  pred_time <- c(0, 100, 200, 10000)

  set.seed(403)
  exp_f_fit <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung,
    family = mboost::CoxPH()
  )
  cox_spec <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  set.seed(403)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_error(
    predict(f_fit, lung, type = "survival"),
    "When using `type` values of 'survival' or 'hazard', a numeric vector"
  )

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
    all(purrr::map_lgl(f_pred$.pred, ~ all(dim(.x) == c(4, 2))))
  )
  expect_true(
    all(purrr::map_lgl(
      f_pred$.pred,
      ~ all(names(.x) == c(".eval_time", ".pred_survival"))
    ))
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
  expect_equal(prob[c(2, 3, 1), ], exp_prob)

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
  exp_prob <- summary(surv_fit, time = pred_time_inf)$surv
  prob <- survival_curve_to_prob(
    eval_time = pred_time_inf,
    event_times = surv_fit$time,
    survival_prob = surv_fit$surv
  )
  expect_equal(nrow(prob), length(pred_time_inf))
  expect_equal(prob[c(2, 4), ], exp_prob)
  expect_equal(
    prob[c(1, 5), ],
    matrix(1, nrow = 2, ncol = nrow(lung_pred)),
    ignore_attr = "dimnames"
  )
  expect_equal(
    prob[3, ] %>% unname(),
    rep(0, nrow(lung_pred))
  )
})

test_that("survival_prob_mboost() works", {
  lung2 <- lung[-14, ]
  mboost_object <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung2,
    family = mboost::CoxPH()
  )

  # can handle missings
  pred <- survival_prob_mboost(
    mboost_object,
    new_data = lung[14:15, ],
    eval_time = c(0, 100, 200)
  )
  expect_equal(nrow(pred), 2)

  # can handle single observation
  # skip until mboost::survFit() works with a single row for `newdata`
  # fix submitted: https://github.com/boost-R/mboost/pull/118
  # pred <- survival_prob_mboost(
  #   mboost_object,
  #   new_data = lung[1,],
  #   eval_time = c(0, 100, 200)
  # )
  # expect_equal(nrow(pred), 1)
})

# prediction: linear_pred -------------------------------------------------

test_that("linear_pred predictions", {
  lung2 <- lung[-14, ]
  exp_f_fit <- mboost::blackboost(
    Surv(time, status) ~ age + ph.ecog,
    data = lung2,
    family = mboost::CoxPH()
  )
  cox_spec <- boost_tree() %>%
    set_engine("mboost") %>%
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
  lung_x <- as.matrix(lung[, c("age", "ph.ecog")])
  lung_y <- Surv(lung$time, lung$status)
  lung_pred <- lung[1:5, ]

  spec <- boost_tree() %>%
    set_engine("mboost") %>%
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
