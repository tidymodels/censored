library(testthat)

# aorsf is picky with inputs, status must be 0's and 1's
lung_orsf <- lung
lung_orsf$status <- lung_orsf$status - 1
lung_orsf <- na.omit(lung_orsf)

test_that("model object", {
  skip_if_not_installed("aorsf")

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


# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  skip_if_not_installed("aorsf")

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

  expect_error(predict(f_fit, lung_orsf, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")

  f_pred <- predict(f_fit, lung_orsf, type = "survival", time = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung_orsf))
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
    rep(100:200, nrow(lung_orsf))
  )


  f_pred <- predict(f_fit, lung_orsf[1,], type = "survival", time = 306)

  new_km <- predict(exp_f_fit,
                    new_data = lung_orsf[1,],
                    pred_type = "surv",
                    pred_horizon = 306)[1,1]

  expect_equal(
    f_pred$.pred[[1]]$.pred_survival,
    new_km
  )

})


# prediction: hazard ----------------------------------------------------

test_that("hazard predictions", {
  skip_if_not_installed("aorsf")

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

  expect_error(predict(f_fit, lung_orsf, type = "hazard"),
               "When using 'type' values of 'survival' or 'hazard' are given")

  f_pred <- predict(f_fit, lung_orsf, type = "hazard", time = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung_orsf))
  expect_equal(
    unique(purrr::map_int(f_pred$.pred, nrow)),
    101
  )
  cf_names <-
    c(".time", ".pred_hazard")

  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ identical(names(.x), cf_names)))
  )

  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.time,
    rep(100:200, nrow(lung_orsf))
  )


  f_pred <- predict(f_fit, lung_orsf[1,], type = "hazard", time = 306)

  new_km <- predict(exp_f_fit,
                    new_data = lung_orsf[1,],
                    pred_type = "chf",
                    pred_horizon = 306)[1,1]

  expect_equal(
    f_pred$.pred[[1]]$.pred_hazard,
    new_km
  )

})
