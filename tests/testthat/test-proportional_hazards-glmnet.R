library(testthat)
skip_if_not_installed("glmnet")
suppressPackageStartupMessages(library(glmnet))

test_that("model object", {
  lung2 <- lung[-14, ]
  exp_f_fit <- glmnet(
    x = as.matrix(lung2[, c(4, 6)]),
    y = Surv(lung2$time, lung2$status),
    family = "cox"
  )

  # formula method
  cox_spec <- proportional_hazards(penalty = 0.123) %>% set_engine("glmnet")
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2),
    NA
  )

  # Removing call element
  expect_equal(f_fit$fit[-11], exp_f_fit[-11])
})

test_that("print coxnet model", {
  lung2 <- lung[-14, ]
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  expect_snapshot(f_fit)
})

# prediction: time --------------------------------------------------------

test_that("time predictions without strata", {
  # remove row with missing value in ph.ecog
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)
  new_x <- as.matrix(lung2[1:3, c("age", "ph.ecog")])
  set.seed(14)
  exp_f <- glmnet::glmnet(lung_x, lung_y, family = "cox")
  exp_sf <- survfit(exp_f, newx = new_x, x = lung_x, y = lung_y, s = 0.1)
  exp_f_pred <- summary(exp_sf)$table[, "rmean"] %>% unname()

  cox_spec <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")
  set.seed(14)
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2)

  # predict
  new_data_3 <- lung2[1:3, ]
  # should default to penalty value specified at fit time
  expect_error(
    f_pred <- predict(f_fit, new_data = new_data_3, type = "time"),
    NA
  )
  f_pred <- predict(f_fit, new_data = new_data_3, type = "time", penalty = 0.1)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_equal(f_pred$.pred_time, exp_f_pred)

  # single observation
  f_pred_1 <- predict(f_fit, lung2[1, ], type = "time")
  expect_equal(nrow(f_pred_1), 1)
})

test_that("time predictions with strata", {
  # remove row with missing value in ph.ecog
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status) %>%
    glmnet::stratifySurv(lung2$sex)
  new_x <- as.matrix(lung2[1:3, c("age", "ph.ecog")])
  new_strata <- lung2$sex[1:3]
  set.seed(14)
  suppressWarnings(
    exp_f <- glmnet::glmnet(lung_x, lung_y, family = "cox")
  )
  exp_sf <- survfit(
    exp_f,
    newx = new_x,
    newstrata = new_strata,
    x = lung_x,
    y = lung_y,
    s = 0.1
  )
  exp_f_pred <- summary(exp_sf)$table[, "rmean"] %>% unname()

  cox_spec <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")
  set.seed(14)
  suppressWarnings(
    f_fit <- fit(
      cox_spec,
      Surv(time, status) ~ age + ph.ecog + strata(sex),
      data = lung2
    )
  )

  # predict
  new_data_3 <- lung2[1:3, ]
  # should default to penalty value specified at fit time
  expect_error(
    f_pred <- predict(f_fit, new_data = new_data_3, type = "time"),
    NA
  )
  f_pred <- predict(f_fit, new_data = new_data_3, type = "time", penalty = 0.1)

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_equal(f_pred$.pred_time, exp_f_pred)

  # single observation
  f_pred_1 <- predict(f_fit, lung2[1, ], type = "time")
  expect_equal(nrow(f_pred_1), 1)
})

test_that("time predictions with NA in predictor", {
  lung2 <- lung[-14, ]

  suppressWarnings(
    f_fit <- proportional_hazards(penalty = 0.123) %>%
      set_engine("glmnet") %>%
      fit(Surv(time, status) ~ age + ph.ecog + strata(sex), data = lung2)
  )

  # survfit.coxph() is not type-stable,
  # thus test against single or multiple survival curves
  # lung$ph.ecog[14] is NA
  na_x_data_x <- lung[c(13:15, 14), ]
  na_x_data_1 <- lung[c(13, 14, 14), ]
  na_x_data_0 <- lung[c(14, 14), ]
  na_1_data_x <- lung[13:15, ]
  na_1_data_1 <- lung[13:14, ]
  na_1_data_0 <- lung[14, ]

  # survival times
  f_pred <- predict(f_fit, na_x_data_x, type = "time")
  expect_equal(nrow(f_pred), nrow(na_x_data_x))
  expect_identical(which(is.na(f_pred$.pred_time)), c(2L, 4L))

  f_pred <- predict(f_fit, na_x_data_1, type = "time")
  expect_equal(nrow(f_pred), nrow(na_x_data_1))
  expect_identical(which(is.na(f_pred$.pred_time)), c(2L, 3L))

  f_pred <- predict(f_fit, na_x_data_0, type = "time")
  expect_equal(nrow(f_pred), nrow(na_x_data_0))
  expect_identical(which(is.na(f_pred$.pred_time)), c(1L, 2L))

  f_pred <- predict(f_fit, na_1_data_x, type = "time")
  expect_equal(nrow(f_pred), nrow(na_1_data_x))
  expect_identical(which(is.na(f_pred$.pred_time)), 2L)

  f_pred <- predict(f_fit, na_1_data_1, type = "time")
  expect_equal(nrow(f_pred), nrow(na_1_data_1))
  expect_identical(which(is.na(f_pred$.pred_time)), 2L)

  f_pred <- predict(f_fit, na_1_data_0, type = "time")
  expect_equal(nrow(f_pred), nrow(na_1_data_0))
  expect_identical(which(is.na(f_pred$.pred_time)), 1L)
})

test_that("time predictions with NA in strata", {
  lung2 <- lung[-14, ]

  suppressWarnings(
    f_fit <- proportional_hazards(penalty = 0.123) %>%
      set_engine("glmnet") %>%
      fit(Surv(time, status) ~ age + ph.ecog + strata(sex), data = lung2)
  )

  # survfit.coxph() is not type-stable,
  # thus test against single or multiple survival curves
  lung2$sex[2] <- NA
  na_x_data_x <- lung2[c(1:3, 2), ]
  na_x_data_1 <- lung2[c(1, 2, 2), ]
  na_x_data_0 <- lung2[c(2, 2), ]
  na_1_data_x <- lung2[1:3, ]
  na_1_data_1 <- lung2[1:2, ]
  na_1_data_0 <- lung2[2, ]

  # survival times
  f_pred <- predict(f_fit, na_x_data_x, type = "time")
  expect_equal(nrow(f_pred), nrow(na_x_data_x))
  expect_identical(which(is.na(f_pred$.pred_time)), c(2L, 4L))

  f_pred <- predict(f_fit, na_x_data_1, type = "time")
  expect_equal(nrow(f_pred), nrow(na_x_data_1))
  expect_identical(which(is.na(f_pred$.pred_time)), c(2L, 3L))

  f_pred <- predict(f_fit, na_x_data_0, type = "time")
  expect_equal(nrow(f_pred), nrow(na_x_data_0))
  expect_identical(which(is.na(f_pred$.pred_time)), c(1L, 2L))

  f_pred <- predict(f_fit, na_1_data_x, type = "time")
  expect_equal(nrow(f_pred), nrow(na_1_data_x))
  expect_identical(which(is.na(f_pred$.pred_time)), 2L)

  f_pred <- predict(f_fit, na_1_data_1, type = "time")
  expect_equal(nrow(f_pred), nrow(na_1_data_1))
  expect_identical(which(is.na(f_pred$.pred_time)), 2L)

  f_pred <- predict(f_fit, na_1_data_0, type = "time")
  expect_equal(nrow(f_pred), nrow(na_1_data_0))
  expect_identical(which(is.na(f_pred$.pred_time)), 1L)
})

test_that("survival_time_coxnet() works for single penalty value", {
  # single penalty value
  pred_penalty <- 0.1

  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)

  exp_f_fit <- suppressWarnings(
    glmnet::glmnet(x = lung_x, y = lung_y, family = "cox")
  )
  f_fit <- suppressWarnings(
    proportional_hazards(penalty = 0.1) %>%
      set_engine("glmnet") %>%
      fit(Surv(time, status) ~ age + ph.ecog, data = lung2)
  )

  # multiple observations (with 1 missing)
  lung_pred <- lung[13:15, c("age", "ph.ecog")]
  surv_fit <- survfit(
    exp_f_fit,
    newx = as.matrix(lung_pred),
    s = pred_penalty,
    x = lung_x,
    y = lung_y
  )

  pred <- survival_time_coxnet(
    f_fit,
    new_data = lung_pred,
    penalty = pred_penalty
  )
  exp_pred <- extract_patched_survival_time(
    surv_fit,
    missings_in_new_data = 2,
    n_obs = 3
  )

  expect_identical(pred, exp_pred)
  expect_identical(which(is.na(pred)), 2L)

  # single observation
  lung_pred <- lung[13, c("age", "ph.ecog")]
  surv_fit <- survfit(
    exp_f_fit,
    newx = as.matrix(lung_pred),
    s = pred_penalty,
    x = lung_x,
    y = lung_y
  )

  pred <- survival_time_coxnet(
    f_fit,
    new_data = lung_pred,
    penalty = pred_penalty
  )
  exp_pred <- extract_patched_survival_time(
    surv_fit,
    missings_in_new_data = NULL,
    n_obs = 1
  )

  expect_identical(pred, exp_pred)

  # all observations with missings
  lung_pred <- lung[c(14, 14), ]

  pred <- survival_time_coxnet(
    f_fit,
    new_data = lung_pred,
    penalty = pred_penalty
  )
  exp_pred <- rep(NA, 2)

  expect_identical(pred, exp_pred)
})

test_that("survival_time_coxnet() works for multiple penalty values", {
  # multiple penalty values
  pred_penalty <- c(0.1, 0.2)

  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)

  exp_f_fit <- suppressWarnings(
    glmnet::glmnet(x = lung_x, y = lung_y, family = "cox")
  )
  f_fit <- suppressWarnings(
    proportional_hazards(penalty = 0.1) %>%
      set_engine("glmnet") %>%
      fit(Surv(time, status) ~ age + ph.ecog, data = lung2)
  )

  # multiple observations (with 1 missing)
  lung_pred <- lung[13:15, c("age", "ph.ecog")]
  surv_fit <- survfit(
    exp_f_fit,
    newx = as.matrix(lung_pred),
    s = pred_penalty,
    x = lung_x,
    y = lung_y
  )

  pred <- survival_time_coxnet(
    f_fit,
    new_data = lung_pred,
    penalty = pred_penalty,
    multi = TRUE
  )
  exp_pred <- purrr::map(
    surv_fit,
    extract_patched_survival_time,
    missings_in_new_data = 2,
    n_obs = 3
  ) %>%
    purrr::list_c()

  expect_named(pred, ".pred")
  expect_named(pred$.pred[[1]], c("penalty", ".pred_time"))
  expect_identical(
    pred %>% tidyr::unnest(cols = .pred) %>% dplyr::arrange(penalty) %>% dplyr::pull(.pred_time),
    exp_pred
  )
  expect_identical(
    pred$.pred[[2]] %>% dplyr::pull(.pred_time),
    rep(NA_real_, 2)
  )

  # single observation
  lung_pred <- lung[13, c("age", "ph.ecog")]
  surv_fit <- survfit(
    exp_f_fit,
    newx = as.matrix(lung_pred),
    s = pred_penalty,
    x = lung_x,
    y = lung_y
  )

  pred <- survival_time_coxnet(
    f_fit,
    new_data = lung_pred,
    penalty = pred_penalty,
    multi = TRUE
  )
  exp_pred <- purrr::map(
    surv_fit,
    extract_patched_survival_time,
    missings_in_new_data = NULL,
    n_obs = 1
  ) %>%
    purrr::list_c()

  expect_named(pred, ".pred")
  expect_named(pred$.pred[[1]], c("penalty", ".pred_time"))
  expect_identical(
    pred %>% tidyr::unnest(cols = .pred) %>% dplyr::arrange(penalty) %>% dplyr::pull(.pred_time),
    exp_pred
  )

  # all observations with missings
  lung_pred <- lung[c(14, 14), ]

  pred <- survival_time_coxnet(
    f_fit,
    new_data = lung_pred,
    penalty = pred_penalty,
    multi = TRUE
  )
  exp_pred <- rep(NA, 4)

  expect_named(pred, ".pred")
  expect_named(pred$.pred[[1]], c("penalty", ".pred_time"))
  expect_identical(
    pred %>% tidyr::unnest(cols = .pred) %>% dplyr::arrange(penalty) %>% dplyr::pull(.pred_time),
    exp_pred
  )
})

# prediction: survival ----------------------------------------------------

test_that("survival probabilities without strata", {
  # load the `lung` dataset
  data(cancer, package = "survival")
  # remove row with missing value
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  cox_spec <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  set.seed(14)
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2),
    NA
  )

  # predict
  expect_error(
    pred_1 <- predict(
      f_fit,
      new_data = lung2[1, ],
      type = "survival",
      eval_time = c(100, 200)
    ),
    NA
  )

  f_pred <- predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(100, 200),
    penalty = 0.1
  )

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(
      f_pred$.pred,
      ~ all(names(.x) == c(".eval_time", ".pred_survival"))
    ))
  )

  # single observation
  expect_error(
    f_pred_1 <- predict(f_fit, lung2[1, ], type = "survival", eval_time = c(100, 200)),
    NA
  )
  expect_equal(nrow(f_pred_1), 1)

  # multi_predict
  f_pred_unnested_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(penalty = 0.1, .row = rep(1:3, each = 2))
  f_pred_unnested_005 <-
    predict(
      f_fit,
      new_data = new_data_3,
      type = "survival",
      eval_time = c(100, 200),
      penalty = 0.05
    ) %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(
      penalty = 0.05,
      .row = rep(1:3, each = 2)
    )
  exp_pred_multi_unnested <-
    dplyr::bind_rows(f_pred_unnested_005, f_pred_unnested_01) %>%
    dplyr::arrange(.row, penalty, .eval_time) %>%
    dplyr::select(penalty, .eval_time, .pred_survival)


  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(100, 200),
    penalty = c(0.05, 0.1)
  )
  expect_s3_class(pred_multi, "tbl_df")
  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(dim(.x) == c(2 * 2, 3))
    ))
  )
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )
  expect_equal(
    pred_multi %>% tidyr::unnest(cols = .pred),
    exp_pred_multi_unnested
  )
})

test_that("survival probabilities with strata", {
  cox_spec <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  set.seed(14)
  expect_error(
    f_fit <- fit(
      cox_spec,
      Surv(stop, event) ~ rx + size + number + strata(enum),
      data = bladder
    ),
    NA
  )
  new_data_3 <- bladder[1:3, ]

  # predict
  f_pred <- predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(10, 20),
    penalty = 0.1
  )

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(
      f_pred$.pred,
      ~ all(names(.x) == c(".eval_time", ".pred_survival"))
    ))
  )
  # single observation
  expect_error(
    f_pred_1 <- predict(f_fit, bladder[1, ], type = "survival", eval_time = c(10, 20)),
    NA
  )
  expect_equal(nrow(f_pred_1), 1)

  # multi_predict
  f_pred_unnested_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(penalty = 0.1, .row = rep(1:3, each = 2))
  f_pred_unnested_005 <-
    predict(
      f_fit,
      new_data = new_data_3,
      type = "survival",
      eval_time = c(10, 20),
      penalty = 0.05
    ) %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(
      penalty = 0.05,
      .row = rep(1:3, each = 2)
    )
  exp_pred_multi_unnested <-
    dplyr::bind_rows(f_pred_unnested_005, f_pred_unnested_01) %>%
    dplyr::arrange(.row, penalty, .eval_time) %>%
    dplyr::select(penalty, .eval_time, .pred_survival)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(10, 20),
    penalty = c(0.05, 0.1)
  )
  expect_s3_class(pred_multi, "tbl_df")
  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(dim(.x) == c(2 * 2, 3))
    ))
  )
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )
  expect_equal(
    pred_multi %>% tidyr::unnest(cols = .pred),
    exp_pred_multi_unnested
  )
})

test_that("survival prediction with NA in predictor", {
  lung2 <- lung[-14, ]

  suppressWarnings(
    f_fit <- proportional_hazards(penalty = 0.123) %>%
      set_engine("glmnet") %>%
      fit(Surv(time, status) ~ age + ph.ecog + strata(sex), data = lung2)
  )

  # survfit.coxph() is not type-stable,
  # thus test against single or multiple survival curves
  # lung$ph.ecog[14] is NA
  na_x_data_x <- lung[c(13:15, 14), ]
  na_x_data_1 <- lung[c(13, 14, 14), ]
  na_x_data_0 <- lung[c(14, 14), ]
  na_1_data_x <- lung[13:15, ]
  na_1_data_1 <- lung[13:14, ]
  na_1_data_0 <- lung[14, ]

  # survival probabilities
  expect_error(
    f_pred <- predict(f_fit, na_x_data_x, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_x))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[4]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_x_data_1, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_1))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[3]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_x_data_0, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_0))
  expect_true(all(is.na(f_pred$.pred[[1]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_x, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_x))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_1, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_1))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_0, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_0))
  expect_true(all(is.na(f_pred$.pred[[1]]$.pred_survival)))
})

test_that("survival prediction with NA in strata", {
  lung2 <- lung[-14, ]

  suppressWarnings(
    f_fit <- proportional_hazards(penalty = 0.123) %>%
      set_engine("glmnet") %>%
      fit(Surv(time, status) ~ age + ph.ecog + strata(sex), data = lung2)
  )

  # survfit.coxph() is not type-stable,
  # thus test against single or multiple survival curves
  lung2$sex[2] <- NA
  na_x_data_x <- lung2[c(1:3, 2), ]
  na_x_data_1 <- lung2[c(1, 2, 2), ]
  na_x_data_0 <- lung2[c(2, 2), ]
  na_1_data_x <- lung2[1:3, ]
  na_1_data_1 <- lung2[1:2, ]
  na_1_data_0 <- lung2[2, ]

  # survival probabilities
  expect_error(
    f_pred <- predict(f_fit, na_x_data_x, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_x))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[4]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_x_data_1, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_1))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[3]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_x_data_0, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_0))
  expect_true(all(is.na(f_pred$.pred[[1]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_x, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_x))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_1, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_1))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_0, type = "survival", eval_time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_0))
  expect_true(all(is.na(f_pred$.pred[[1]]$.pred_survival)))
})

test_that("survival_prob_coxnet() works for single penalty value", {
  # single penalty value
  pred_penalty <- 0.1

  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)

  exp_f_fit <- suppressWarnings(
    glmnet::glmnet(x = lung_x, y = lung_y, family = "cox")
  )
  f_fit <- suppressWarnings(
    proportional_hazards(penalty = 0.1) %>%
      set_engine("glmnet") %>%
      fit(Surv(time, status) ~ age + ph.ecog, data = lung2)
  )

  # time: combination of order, out-of-range, infinite
  pred_time <- c(-Inf, 0, 100, Inf, 1022, 3000)

  # multiple observations (with 1 missing)
  lung_pred <- lung[13:15, c("age", "ph.ecog")]
  surv_fit <- survfit(
    exp_f_fit,
    newx = as.matrix(lung_pred),
    s = pred_penalty,
    x = lung_x,
    y = lung_y
  )
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  prob <- survival_prob_coxnet(
    f_fit,
    new_data = lung_pred,
    eval_time = pred_time,
    penalty = pred_penalty
  )
  exp_prob <- surv_fit_summary$surv

  prob_na <- prob$.pred[[2]]
  prob_non_na <- prob$.pred[[3]]
  exp_prob_non_na <- exp_prob[, 2]

  # get missings right
  expect_true(all(is.na(prob_na$.pred_survival)))
  # for non-missings, get probs right
  expect_equal(prob_non_na$.eval_time, pred_time)
  expect_equal(
    prob_non_na$.pred_survival[c(1, 4)],
    c(1, 0)
  )
  expect_equal(
    prob_non_na %>%
      dplyr::filter(is.finite(.eval_time)) %>%
      dplyr::arrange(.eval_time) %>%
      dplyr::pull(.pred_survival),
    exp_prob_non_na
  )

  # single observation
  lung_pred <- lung[13, c("age", "ph.ecog")]
  surv_fit <- survfit(exp_f_fit, newx = as.matrix(lung_pred), s = pred_penalty, x = lung_x, y = lung_y)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  prob <- survival_prob_coxnet(f_fit, new_data = lung_pred, eval_time = pred_time, penalty = pred_penalty)
  prob <- tidyr::unnest(prob, cols = .pred)
  exp_prob <- surv_fit_summary$surv

  expect_equal(
    prob$.pred_survival[c(1, 4)],
    c(1, 0)
  )
  expect_equal(
    prob %>%
      dplyr::filter(is.finite(.eval_time)) %>%
      dplyr::arrange(.eval_time) %>%
      dplyr::pull(.pred_survival),
    exp_prob
  )

  # all observations with missings
  lung_pred <- lung[c(14, 14), ]

  prob <- survival_prob_coxnet(f_fit, new_data = lung_pred, eval_time = pred_time, penalty = pred_penalty)
  prob <- tidyr::unnest(prob, cols = .pred)
  expect_true(all(is.na(prob$.pred_survival)))
})

test_that("survival_prob_coxnet() works for multiple penalty values", {
  # multiple penalty values
  pred_penalty <- c(0.1, 0.2)

  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)

  exp_f_fit <- suppressWarnings(
    glmnet::glmnet(x = lung_x, y = lung_y, family = "cox")
  )
  f_fit <- suppressWarnings(
    proportional_hazards(penalty = 0.1) %>%
      set_engine("glmnet") %>%
      fit(Surv(time, status) ~ age + ph.ecog, data = lung2)
  )

  # time: combination of order, out-of-range, infinite
  pred_time <- c(-Inf, 0, 100, Inf, 1022, 3000)

  # multiple observations (with 1 missing)
  lung_pred <- lung[13:15, c("age", "ph.ecog")]
  surv_fit <- survfit(exp_f_fit, newx = as.matrix(lung_pred), s = pred_penalty, x = lung_x, y = lung_y)
  surv_fit_summary <- purrr::map(surv_fit, summary, times = pred_time, extend = TRUE)

  prob <- survival_prob_coxnet(
    f_fit,
    new_data = lung_pred,
    eval_time = pred_time,
    penalty = pred_penalty,
    multi = TRUE
  )
  prob_na <- prob$.pred[[2]]
  prob_non_na <- prob$.pred[[3]]
  # observation in row 15
  exp_prob <- purrr::map(surv_fit_summary, ~ .x$surv[, 2]) %>% unlist()

  # get missings right
  expect_true(all(is.na(prob_na$.pred_survival)))
  # for non-missings, get probs right
  expect_equal(prob_non_na$.eval_time, rep(pred_time, length(pred_penalty)))
  expect_equal(
    prob_non_na$.pred_survival[c(1, 4, 7, 10)],
    c(1, 0, 1, 0)
  )
  expect_equal(
    prob_non_na %>%
      dplyr::filter(is.finite(.eval_time)) %>%
      dplyr::arrange(penalty, .eval_time) %>%
      dplyr::pull(.pred_survival),
    exp_prob
  )

  # single observation
  lung_pred <- lung[13, c("age", "ph.ecog")]
  surv_fit <- survfit(exp_f_fit, newx = as.matrix(lung_pred), s = pred_penalty, x = lung_x, y = lung_y)
  surv_fit_summary <- purrr::map(surv_fit, summary, times = pred_time, extend = TRUE)

  prob <- survival_prob_coxnet(
    f_fit,
    new_data = lung_pred,
    eval_time = pred_time,
    penalty = pred_penalty,
    multi = TRUE
  )

  prob <- tidyr::unnest(prob, cols = .pred)
  exp_prob <- purrr::map(surv_fit_summary, purrr::pluck, "surv") %>% unlist()

  expect_equal(prob_non_na$.eval_time, rep(pred_time, length(pred_penalty)))
  expect_equal(
    prob$.pred_survival[c(1, 4, 7, 10)],
    c(1, 0, 1, 0)
  )
  expect_equal(
    prob %>%
      dplyr::filter(is.finite(.eval_time)) %>%
      dplyr::arrange(penalty, .eval_time) %>%
      dplyr::pull(.pred_survival),
    exp_prob
  )

  # all observations with missings
  lung_pred <- lung[c(14, 14), ]

  pred <- survival_prob_coxnet(
    f_fit,
    new_data = lung_pred,
    eval_time = pred_time,
    penalty = pred_penalty,
    multi = TRUE
  )
  exp_pred <- rep(
    NA_real_,
    times = length(pred_penalty) * length(pred_time) * nrow(lung_pred)
  )

  expect_named(pred, ".pred")
  expect_named(pred$.pred[[1]], c("penalty", ".eval_time", ".pred_survival"))
  expect_identical(
    pred %>%
      tidyr::unnest(cols = .pred) %>%
      dplyr::arrange(penalty) %>%
      dplyr::pull(.pred_survival),
    exp_pred
  )
})

test_that("can predict for out-of-domain timepoints", {
  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[2,]

  mod <- proportional_hazards(penalty = 0.1) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ ., data = lung)

  expect_no_error(
    preds <- predict(mod, obs_without_NA, type = "survival", eval_time = eval_time_obs_max_and_ood)
  )
})

# prediction: linear_pred -------------------------------------------------

test_that("linear_pred predictions without strata", {
  lung2 <- lung[-14, ]
  exp_f_fit <- glmnet::glmnet(
    x = as.matrix(lung2[, c(4, 6)]),
    y = Surv(lung2$time, lung2$status),
    family = "cox"
  )
  cox_spec <- proportional_hazards(penalty = 0.123) %>% set_engine("glmnet")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2)

  # predict
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01)
  exp_f_pred <- -unname(
    predict(
      exp_f_fit,
      newx = as.matrix(lung2[, c(4, 6)]),
      type = "link",
      s = 0.01
    )
  )

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, as.vector(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))

  # single observation
  expect_error(f_pred_1 <- predict(f_fit, lung2[1, ], type = "linear_pred"), NA)
  expect_equal(nrow(f_pred_1), 1)

  # predict without the sign flip
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01, increasing = FALSE)
  exp_f_pred <- unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, as.vector(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))


  # multi_predict
  new_data_3 <- lung2[1:3, ]
  f_pred_unnested_01 <-
    predict(f_fit, new_data_3, type = "linear_pred", penalty = 0.1) %>%
    dplyr::mutate(penalty = 0.1, .row = seq_len(nrow(new_data_3)))
  f_pred_unnested_005 <-
    predict(f_fit, new_data_3, type = "linear_pred", penalty = 0.05) %>%
    dplyr::mutate(penalty = 0.05, .row = seq_len(nrow(new_data_3)))
  exp_pred_multi_unnested <-
    dplyr::bind_rows(
      f_pred_unnested_005,
      f_pred_unnested_01
    ) %>%
    dplyr::arrange(.row, penalty) %>%
    dplyr::select(penalty, .pred_linear_pred)

  pred_multi <- multi_predict(
    f_fit,
    new_data_3,
    type = "linear_pred",
    penalty = c(0.05, 0.1)
  )
  expect_s3_class(pred_multi, "tbl_df")
  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(dim(.x) == c(2, 2))
    ))
  )
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".pred_linear_pred"))
    ))
  )
  expect_equal(
    pred_multi %>% tidyr::unnest(cols = .pred),
    exp_pred_multi_unnested
  )
})

test_that("linear_pred predictions with strata", {
  # TODO find a better example?

  lung2 <- lung[-14, ]
  exp_f_fit <- suppressWarnings(
    glmnet::glmnet(
      x = as.matrix(lung2[, c(4, 6)]),
      y = stratifySurv(Surv(lung2$time, lung2$status), lung2$sex),
      family = "cox"
    )
  )
  cox_spec <- proportional_hazards(penalty = 0.123) %>% set_engine("glmnet")
  expect_error(
    suppressWarnings(
      f_fit <- fit(
        cox_spec,
        Surv(time, status) ~ age + ph.ecog + strata(sex),
        data = lung2
      )
    ),
    NA
  )

  # predict
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01)
  exp_f_pred <- -unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, as.vector(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))

  # single observation
  expect_error(f_pred_1 <- predict(f_fit, lung2[1, ], type = "linear_pred"), NA)
  expect_equal(nrow(f_pred_1), 1)

  # predict without the sign flip
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01, increasing = FALSE)
  exp_f_pred <- unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, as.vector(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))


  # multi_predict
  new_data_3 <- lung2[1:3, ]
  f_pred_unnested_01 <-
    predict(f_fit, new_data_3, type = "linear_pred", penalty = 0.1) %>%
    dplyr::mutate(penalty = 0.1, .row = seq_len(nrow(new_data_3)))
  f_pred_unnested_005 <-
    predict(f_fit, new_data_3, type = "linear_pred", penalty = 0.05) %>%
    dplyr::mutate(penalty = 0.05, .row = seq_len(nrow(new_data_3)))
  exp_pred_multi_unnested <-
    dplyr::bind_rows(
      f_pred_unnested_005,
      f_pred_unnested_01
    ) %>%
    dplyr::arrange(.row, penalty) %>%
    dplyr::select(penalty, .pred_linear_pred)

  pred_multi <- multi_predict(
    f_fit,
    new_data_3,
    type = "linear_pred",
    penalty = c(0.05, 0.1)
  )
  expect_s3_class(pred_multi, "tbl_df")
  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(dim(.x) == c(2, 2))
    ))
  )
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".pred_linear_pred"))
    ))
  )
  expect_equal(
    pred_multi %>% tidyr::unnest(cols = .pred),
    exp_pred_multi_unnested
  )
})

# helper functions --------------------------------------------------------

test_that("stratification is specified in a single term", {
  spec <- proportional_hazards(penalty = 0.123) %>%
    set_engine("glmnet")
  expect_snapshot(error = TRUE, {
    fit(spec, Surv(time, status) ~ age + ph.ecog + strata(sex) + strata(inst), data = lung)
  })
})

test_that("formula modifications to remove strata", {
  # base case
  expect_equal(
    drop_strata(rlang::expr(x + strata(s))),
    rlang::expr(x)
  )

  expect_equal(
    drop_strata(rlang::expr(x + x + x + strata(s))),
    rlang::expr(x + x + x)
  )
  expect_equal(
    drop_strata(rlang::expr(x * (y + strata(s)) + z)),
    rlang::expr(x * (y + strata(s)) + z)
  )

  expect_error(
    check_strata_remaining(rlang::expr(x * (y + strata(s)) + z))
  )

  skip_if(R.version$major == "3")
  spec <- proportional_hazards(penalty = 0.1) %>%
    set_engine("glmnet")
  expect_snapshot(error = TRUE, {
    fit(spec, Surv(time, status) ~ strata(sex), data = lung)
  })
  expect_snapshot(error = TRUE, {
    fit(spec, Surv(time, status) ~ age + (ph.ecog + strata(sex)), data = lung)
  })
})

test_that("protect certain glmnet engine args", {
  expect_snapshot(error = TRUE, {
    proportional_hazards(penalty = 0.1) %>%
      set_engine("glmnet", family = "gaussian") %>%
      fit(Surv(time, status) ~ age + sex, data = lung)
  })
})



# ------------------------------------------------------------------------------

test_that("predictions with strata and dot in formula", {
  # For R <= 3.6 only , glmnet models below give a warning for lack of convergence.
  skip_if(R.version$major == "3")

  cox_spec <- proportional_hazards(penalty = 0.001) %>% set_engine("glmnet")
  lung2 <- lung[, c("time", "status", "ph.ecog", "age", "sex")]
  lung2$sex <- factor(lung2$sex)
  lung2 <- lung2[complete.cases(lung2), ]

  # formula method
  # expect warnings "cox.fit: algorithm did not converge"
  expect_snapshot(
    f_fit <- fit(cox_spec, Surv(time, status) ~ . - sex + strata(sex), data = lung2)
  )
  # expect warnings "cox.fit: algorithm did not converge"
  expect_snapshot(
    f_fit_2 <- fit(cox_spec, Surv(time, status) ~ ph.ecog + age + strata(sex), data = lung2)
  )
  # expect warnings "'to new 6 after EncodeVars()"
  expect_snapshot({
    predict(f_fit, lung2, type = "linear_pred")
    predict(f_fit, lung2, type = "survival", eval_time = c(100, 300))
  })
  expect_equal(
    predict(f_fit, lung2, type = "linear_pred"),
    predict(f_fit_2, lung2, type = "linear_pred")
  )
  # expect warnings "'to new 6 after EncodeVars()"
  expect_snapshot({
    f_pred <- predict(f_fit, lung2, type = "survival", eval_time = c(100, 300))
    f_pred_2 <- predict(f_fit_2, lung2, type = "survival", eval_time = c(100, 300))
  })
  expect_equal(f_pred, f_pred_2)
})

# fit via matrix interface ------------------------------------------------

test_that("`fit_xy()` works with matrix input", {
  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)
  lung_pred <- lung2[1:5, ]

  spec <- proportional_hazards(penalty = 0.1) %>%
    set_engine("glmnet")
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung2)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  expect_equal(f_fit$fit, xy_fit$fit)

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

test_that("`fit_xy()` works with data frame input", {
  lung2 <- lung[-14, ]
  lung_x <- lung2[, c("age", "ph.ecog")]
  lung_y <- data.frame(surv = Surv(lung2$time, lung2$status))
  lung_pred <- lung2[1:5, ]

  spec <- proportional_hazards(penalty = 0.1) %>%
    set_engine("glmnet")
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung2)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  expect_equal(f_fit$fit$fit, xy_fit$fit$fit)

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

test_that("`fit_xy()` errors with stratification", {
  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)
  lung_y_s <- glmnet::stratifySurv(lung_y, lung2$sex)

  spec <- proportional_hazards(penalty = 0.1) %>%
    set_engine("glmnet")

  expect_snapshot(error = TRUE, {
    fit_xy(spec, x = lung_x, y = lung_y_s)
  })
})

# `multi_predict()` works for all `type`s available for `predict()` -------

test_that("multi_predict(type = time)", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "time",
    penalty = c(0.01, 0.1)
  )

  expect_equal(names(pred_multi), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".pred_time"))
    ))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1,],
    type = "time",
    penalty = c(0.01, 0.1)
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      ~ all(names(.x) == c("penalty", ".pred_time"))
    ))
  )
})

test_that("multi_predict(type = survival) for multiple eval_time points", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(100, 500),
    penalty = c(0.01, 0.1)
  )

  expect_equal(names(pred_multi), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1,],
    type = "survival",
    eval_time = c(100, 500),
    penalty = c(0.01, 0.1)
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )
})

test_that("multi_predict(type = survival) for a single eval_time", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = 100,
    penalty = c(0.01, 0.1)
  )

  expect_equal(names(pred_multi), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1,],
    type = "survival",
    eval_time = 100,
    penalty = c(0.01, 0.1)
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )
})

test_that("multi_predict(type = linear_pred)", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "linear_pred",
    penalty = c(0.01, 0.1)
  )

  expect_equal(names(pred_multi), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".pred_linear_pred"))
    ))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1,],
    type = "linear_pred",
    penalty = c(0.01, 0.1)
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      ~ all(names(.x) == c("penalty", ".pred_linear_pred"))
    ))
  )
})

test_that("multi_predict(): type = raw", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  exp_f_fit <- glmnet::glmnet(
    x = as.matrix(lung2[, c(4, 6)]),
    y = Surv(lung2$time, lung2$status),
    family = "cox"
  )
  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  exp_pred <- predict(exp_f_fit, as.matrix(new_data_3[, c(4, 6)]),
                      s = c(0.01, 0.1))
  f_pred <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "raw",
    penalty = c(0.01, 0.1)
  )
  expect_identical(f_pred, exp_pred)

  # single observation
  exp_pred_1 <- predict(exp_f_fit, as.matrix(new_data_3[1, c(4, 6)]),
                        s = c(0.01, 0.1))
  f_pred_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1, ],
    type = "raw",
    penalty = c(0.01, 0.1)
  )
  expect_identical(f_pred_1, exp_pred_1)
})

# multi_predict() works with a single penalty value -----------------------

test_that("multi_predict(type = time) works with single penalty", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "time",
    penalty = 0.1
  )

  expect_equal(names(pred_multi), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".pred_time"))
    ))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1,],
    type = "time",
    penalty = 0.1
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      ~ all(names(.x) == c("penalty", ".pred_time"))
    ))
  )
})

test_that("multi_predict(type = survival) works with single penalty for multiple eval_time points", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = 100,
    penalty = 0.1
  )

  expect_equal(names(pred_multi), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1,],
    type = "survival",
    eval_time = 100,
    penalty = 0.1
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )
})

test_that("multi_predict(type = survival) works with single penalty for a single eval_time", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = 100,
    penalty = 0.1
  )

  expect_equal(names(pred_multi), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1,],
    type = "survival",
    eval_time = 100,
    penalty = 0.1
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      ~ all(names(.x) == c("penalty", ".eval_time", ".pred_survival"))
    ))
  )
})

test_that("multi_predict(type = linear_pred) works with single penalty", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "linear_pred",
    penalty = 0.1
  )

  expect_equal(names(pred_multi), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi$.pred,
      ~ all(names(.x) == c("penalty", ".pred_linear_pred"))
    ))
  )

  # single observation
  pred_multi_1 <- multi_predict(
    f_fit,
    new_data = new_data_3[1,],
    type = "linear_pred",
    penalty = 0.1
  )
  expect_equal(names(pred_multi_1), ".pred")
  expect_true(
    all(purrr::map_lgl(
      pred_multi_1$.pred,
      ~ all(names(.x) == c("penalty", ".pred_linear_pred"))
    ))
  )
})

test_that("multi_predict(type = raw) works with single penalty", {
  lung2 <- lung[-14, ]
  exp_f_fit <- glmnet::glmnet(
    x = as.matrix(lung2[, c(4, 6)]),
    y = Surv(lung2$time, lung2$status),
    family = "cox"
  )
  cox_spec <- proportional_hazards(penalty = 0.123) %>% set_engine("glmnet")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2)

  f_pred <- multi_predict(f_fit, lung2, type = "raw", penalty = 0.01)
  exp_f_pred <- predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01)
  expect_equal(f_pred, exp_f_pred)

  # single observation
  f_pred_1 <- multi_predict(f_fit, lung2[1, ], type = "raw", penalty = 0.01)
  exp_f_pred_1 <- predict(exp_f_fit, newx = as.matrix(lung2[1, c(4, 6)]), s = 0.01)
  expect_equal(f_pred_1, exp_f_pred_1)
})

# multi_predict() handles defaults correctly ------------------------------

test_that("multi_predict(): type = NULL", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  pred_multi_null <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = NULL,
    penalty = c(0.01, 0.1)
  )
  exp_pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "time",
    penalty = c(0.01, 0.1)
  )

  expect_identical(pred_multi_null, exp_pred_multi)
})

test_that("multi_predict() recognises default penalty", {
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  set.seed(14)
  f_fit <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet") %>%
    fit(Surv(time, status) ~ age + ph.ecog, data = lung2)

  # can use any prediction type to test
  pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(100, 200)
  )
  exp_pred_multi <- multi_predict(
    f_fit,
    new_data = new_data_3,
    type = "survival",
    eval_time = c(100, 200),
    penalty = 0.123
  )

  expect_identical(pred_multi, exp_pred_multi)
})
