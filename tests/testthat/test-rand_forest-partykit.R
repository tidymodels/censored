library(testthat)

test_that("model object", {
  skip_if_not_installed("partykit")

  set.seed(1234)
  exp_f_fit <- partykit::cforest(
    Surv(time, status) ~ age + ph.ecog,
    data = lung
  )

  # formula method
  mod_spec <- rand_forest() %>%
    set_engine("partykit") %>%
    set_mode("censored regression")
  set.seed(1234)
  expect_error(
    f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung),
    NA
  )

  # remove `call` from comparison
  f_fit$fit$info$call <- NULL
  exp_f_fit$info$call <- NULL

  expect_equal(
    f_fit$fit,
    exp_f_fit,
    ignore_function_env = TRUE,
    ignore_formula_env = TRUE
  )
})


# prediction: time --------------------------------------------------------

test_that("time predictions", {
  skip_if_not_installed("partykit")

  set.seed(1234)
  exp_f_fit <- partykit::cforest(
    Surv(time, status) ~ age + ph.ecog,
    data = lung
  )

  mod_spec <- rand_forest() %>%
    set_engine("partykit") %>%
    set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  set.seed(1234)
  f_pred <- predict(f_fit, lung, type = "time")
  set.seed(1234)
  exp_f_pred <- predict(exp_f_fit, newdata = lung, type = "response")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(f_pred$.pred_time, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung))
})


# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  skip_if_not_installed("partykit")

  set.seed(1234)
  exp_f_fit <- partykit::cforest(
    Surv(time, status) ~ age + ph.ecog,
    data = lung
  )

  mod_spec <- rand_forest() %>%
    set_engine("partykit") %>%
    set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_error(predict(f_fit, lung, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")

  f_pred <- predict(f_fit, lung, type = "survival", time = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_equal(
    unique(purrr::map_int(f_pred$.pred, nrow)),
    101
  )
  cf_names <-
    c(".time", ".pred_survival")
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ identical(names(.x), cf_names)))
  )

  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.time,
    rep(100:200, nrow(lung))
  )


  f_pred <- predict(f_fit, lung[1,], type = "survival", time = 306)
  new_km <- predict(exp_f_fit, lung[1,], type = "prob")[[1]]

  expect_equal(
    f_pred$.pred[[1]]$.pred_survival,
    new_km$surv[new_km$time == 306]
  )

  # with NA in one of the predictors
  set.seed(1234)
  f_pred <- predict(f_fit, lung[14,], type = "survival", time = 71)
  set.seed(1234)
  new_km <- predict(exp_f_fit, lung[14,], type = "prob")[[1]]

  expect_equal(
    f_pred$.pred[[1]]$.pred_survival,
    new_km$surv[new_km$time == 71]
  )
})
