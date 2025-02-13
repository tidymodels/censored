library(testthat)

test_that("model object", {
  skip_if_not_installed("ipred")

  set.seed(1234)
  exp_f_fit <- ipred::bagging(Surv(time, status) ~ age + ph.ecog, data = lung)

  # formula method
  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  set.seed(1234)
  expect_error(
    f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung),
    NA
  )

  # Removing `call` element from both, it differs in the `data` arg
  expect_equal(f_fit$fit[-6], exp_f_fit[-6], ignore_formula_env = TRUE)
})

test_that("main args work without set_model_arg()", {
  skip_if_not_installed("ipred")

  set.seed(1234)
  exp_f_fit <- ipred::bagging(
    Surv(time, status) ~ age + ph.ecog,
    data = lung,
    # already part of translate?
    maxdepth = 20,
    minsplit = 10,
    cp = 0.5
  )

  # formula method
  mod_spec <- bag_tree(tree_depth = 20, min_n = 10, cost_complexity = 0.5) %>%
    set_mode("censored regression") %>%
    set_engine("rpart")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  # Removing `call` element from both
  expect_equal(f_fit$fit[-6], exp_f_fit[-6], ignore_formula_env = TRUE)
})

# prediction: time --------------------------------------------------------

test_that("time predictions", {
  skip_if_not_installed("ipred")

  set.seed(1234)
  exp_f_fit <- ipred::bagging(Surv(time, status) ~ age + ph.ecog, data = lung)
  exp_f_pred <- predict(exp_f_fit, lung)

  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)
  f_pred <- predict(f_fit, lung, type = "time")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(
    f_pred$.pred_time,
    purrr::map_dbl(exp_f_pred, ~ quantile(.x, probs = .5)$quantile)
  )
  expect_equal(nrow(f_pred), nrow(lung))

  # single observation
  f_pred_1 <- predict(f_fit, lung[1, ], type = "time")
  expect_identical(nrow(f_pred_1), 1L)
})

test_that("time predictions without surrogate splits for NA", {
  skip_if_not_installed("ipred")

  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  f_fit <- fit(mod_spec, Surv(time, status) ~ ph.ecog, data = lung)

  # lung$ph.ecog[14] is NA
  new_data_3 <- lung[13:15, ]

  expect_error(
    f_pred <- predict(f_fit, new_data_3, type = "time"),
    NA
  )
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_equal(which(is.na(f_pred$.pred_time)), 2)
})

test_that("survival_time_survbagg() throws an informative error with an engine object", {
  skip_if_not_installed("ipred")
  mod <- ipred::bagging(Surv(time, status) ~ age + ph.ecog, data = lung)
  expect_snapshot(error = TRUE, {
    survival_time_survbagg(mod)
  })
})

# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  skip_if_not_installed("ipred")

  set.seed(1234)
  exp_f_fit <- ipred::bagging(Surv(time, status) ~ age + ph.ecog, data = lung)

  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  # move snapshot test below back here after parsnip v1.3.0 release

  f_pred <- predict(f_fit, lung, type = "survival", eval_time = 100:200)
  exp_f_pred <- purrr::map(
    predict(exp_f_fit, lung),
    ~summary(.x, times = c(100:200))$surv
  )

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~all(dim(.x) == c(101, 2))))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~all(names(.x) == c(".eval_time", ".pred_survival"))
      )
    )
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    unlist(exp_f_pred)
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.eval_time,
    rep(100:200, nrow(lung))
  )

  # Out of domain prediction
  f_pred <- predict(f_fit, lung, type = "survival", eval_time = 10000)
  exp_f_pred <- purrr::map(
    predict(exp_f_fit, lung),
    ~summary(.x, times = c(max(.x$time)))$surv
  )

  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    unlist(exp_f_pred)
  )
})

test_that("survival predictions - error snapshot", {
  skip_if_not_installed("parsnip", minimum_version = "1.3.0")
  skip_if_not_installed("ipred")
  
  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_snapshot(error = TRUE, {
    predict(f_fit, lung, type = "survival")
  })
})

test_that("survival_prob_survbagg() works", {
  skip_if_not_installed("ipred")

  set.seed(1234)
  # use only ph.ecog to force missings by avoiding surrogate splits
  mod <-  bag_tree(engine = "rpart") %>%
    set_mode("censored regression") %>%
    fit(Surv(time, status) ~ ph.ecog, data = lung)
  engine_mod <- extract_fit_engine(mod)
  # time: combination of order, out-of-range, infinite
  pred_time <- c(-Inf, 0, 100, Inf, 1022, 3000)

  # multiple observations (with 1 missing)
  lung_pred <- lung[13:15, ]
  surv_fit <- predict(engine_mod, newdata = lung_pred[c(1, 3), ])
  surv_fit_summary <- purrr::map(
    surv_fit,
    summary,
    times = pred_time,
    extend = TRUE
  ) %>%
    combine_list_of_survfit_summary(eval_time = pred_time)

  prob <- survival_prob_survbagg(mod, new_data = lung_pred, eval_time = pred_time)
  exp_prob <- surv_fit_summary$surv

  prob_na <- prob$.pred[[2]]
  prob_non_na <- prob$.pred[[3]]
  exp_prob_non_na <- exp_prob[, 2]

  # get missings right
  expect_true(all(is.na(prob_na$.pred_survival)))
  # for non-missings, get probs right
  expect_equal(prob_non_na$.eval_time, pred_time)
  expect_equal(prob_non_na$.pred_survival, exp_prob_non_na)

  # single observation
  lung_pred <- lung[13, ]
  surv_fit <- predict(engine_mod, newdata = lung_pred)
  surv_fit_summary <- purrr::map(
    surv_fit,
    summary,
    times = pred_time,
    extend = TRUE
  ) %>%
    combine_list_of_survfit_summary(eval_time = pred_time)

  prob <- survival_prob_survbagg(mod, new_data = lung_pred, eval_time = pred_time)
  prob <- tidyr::unnest(prob, cols = .pred)
  exp_prob <- surv_fit_summary$surv

  expect_equal(prob$.pred_survival, as.vector(exp_prob))

  # all observations with missings
  lung_pred <- lung[c(14, 14), ]

  prob <- survival_prob_survbagg(mod, new_data = lung_pred, eval_time = pred_time)
  prob <- tidyr::unnest(prob, cols = .pred)
  expect_true(all(is.na(prob$.pred_survival)))
})

test_that("survival predictions without surrogate splits for NA", {
  skip_if_not_installed("ipred")

  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  f_fit <- fit(mod_spec, Surv(time, status) ~ ph.ecog, data = lung)

  # lung$ph.ecog[14] is NA
  new_data_3 <- lung[13:15, ]

  expect_error(
    f_pred <- predict(
      f_fit,
      new_data_3,
      type = "survival",
      eval_time = c(100, 500, 1000)
    ),
    NA
  )
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_true(!any(is.na(f_pred$.pred[[1]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))
  expect_true(!any(is.na(f_pred$.pred[[3]]$.pred_survival)))
})

test_that("can predict for out-of-domain timepoints", {
  skip_if_not_installed("ipred")

  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[2,]

  mod <- bag_tree() %>%
    set_mode("censored regression") %>%
    set_engine("rpart") %>%
    fit(Surv(time, status) ~ ., data = lung)

  expect_no_error(
    preds <- predict(mod, obs_without_NA, type = "survival", eval_time = eval_time_obs_max_and_ood)
  )
})

# fit via matrix interface ------------------------------------------------

test_that("`fix_xy()` works", {
  skip_if_not_installed("ipred")
  
  lung_x <- as.matrix(lung[, c("age", "ph.ecog")])
  lung_y <- Surv(lung$time, lung$status)
  lung_pred <- lung[1:5, ]

  spec <- bag_tree() %>%
    set_engine("rpart") %>%
    set_mode("censored regression")

  set.seed(1)
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung)
  set.seed(1)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  f_ignore <- which(names(f_fit$fit) %in% c("call"))
  xy_ignore <- which(names(xy_fit$fit) %in% c("call"))
  expect_equal(
    f_fit$fit[-f_ignore],
    xy_fit$fit[-xy_ignore],
    ignore_formula_env = TRUE
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
