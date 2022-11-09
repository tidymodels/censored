library(testthat)

test_that("model object", {
  set.seed(1234)
  exp_f_fit <- flexsurv::flexsurvspline(
    Surv(time, status) ~ age + ph.ecog,
    data = lung,
    k = 1
  )

  mod_spec <- survival_reg() %>%
    set_engine("flexsurvspline", k = 1) %>%
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

test_that("time prediction", {
  exp_fit <- flexsurv::flexsurvspline(Surv(time, status) ~ age, data = lung, k = 1)
  exp_pred <- predict(exp_fit, head(lung), type = "response")

  f_fit <- survival_reg() %>%
    set_engine("flexsurvspline", k = 1) %>%
    fit(Surv(time, status) ~ age, data = lung)
  f_pred <- predict(f_fit, head(lung), type = "time")

  expect_equal(f_pred, exp_pred)
})

# prediction: survival ----------------------------------------------------

test_that("survival probability prediction", {
  exp_fit <- flexsurv::flexsurvspline(Surv(time, status) ~ age + sex,
                                      data = lung, k = 1)
  exp_pred <- predict(exp_fit, head(lung), type = "survival",
                      times = c(0, 500, 1000))

  f_fit <- survival_reg() %>%
    set_engine("flexsurvspline", k = 1) %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_error(
    predict(f_fit, head(lung), type = "survival"),
    "a numeric vector 'time'"
  )

  f_pred <- predict(f_fit, head(lung), type = "survival",
                    time = c(0, 500, 1000))

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(head(lung)))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(3, 2))))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~ all(names(.x) == c(".time", ".pred_survival"))))
  )

  expect_equal(f_pred, exp_pred)

  # add confidence interval
  pred <- predict(f_fit, head(lung), type = "survival",
                    time = c(500, 1000), interval = "confidence", level = 0.7)
  expect_true(
    all(purrr::map_lgl(pred$.pred,
                       ~ all(names(.x) == c(".time",
                                            ".pred_survival",
                                            ".pred_lower",
                                            ".pred_upper"))))
  )
})

# prediction: linear_pred -------------------------------------------------

test_that("linear predictor", {
  f_fit <- survival_reg() %>%
    set_engine("flexsurvspline", k = 1) %>%
    fit(Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung[1:5,], type = "linear_pred")

  exp_fit <- flexsurv::flexsurvspline(
    Surv(time, status) ~ age + sex,
    data = lung,
    k = 1
  )
  exp_pred <- predict(exp_fit, lung[1:5,], type = "linear")

  expect_equal(f_pred$.pred_linear_pred, exp_pred$.pred_link)
  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(nrow(f_pred), 5)
})

# prediction: quantile ----------------------------------------------------

test_that("quantile predictions", {
  set.seed(1)
  fit_s <- survival_reg() %>%
    set_engine("flexsurvspline", k = 1) %>%
    set_mode("censored regression") %>%
    fit(Surv(stop, event) ~ rx + size + enum, data = bladder)
  pred <- predict(fit_s, new_data = bladder[1:3,], type = "quantile")

  set.seed(1)
  exp_fit <- flexsurv::flexsurvspline(
    Surv(stop, event) ~ rx + size + enum,
    data = bladder,
    k = 1
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
    all(purrr::map_lgl(pred$.pred,
                       ~ all(dim(.x) == c(9, 2))))
  )
  expect_true(
    all(purrr::map_lgl(pred$.pred,
                       ~ all(names(.x) == c(".quantile",
                                            ".pred_quantile"))))
  )
  expect_equal(
    tidyr::unnest(pred, cols = .pred)$.pred_quantile,
    do.call(rbind, exp_pred)$est
  )

  # add confidence interval
  pred <- predict(fit_s, new_data = bladder[1:3,], type = "quantile",
                  interval = "confidence", level = 0.7)
  expect_true(
    all(purrr::map_lgl(pred$.pred,
                       ~ all(names(.x) == c(".quantile",
                                            ".pred_quantile",
                                            ".pred_lower",
                                            ".pred_upper"))))
  )

})

# prediction: hazard ------------------------------------------------------

test_that("hazard prediction", {
  exp_fit <- flexsurv::flexsurvspline(Surv(time, status) ~ age + sex,
                                      data = lung, k = 1)
  exp_pred <- predict(exp_fit, head(lung), type = "hazard",
                      times = c(0, 500, 1000))

    f_fit <- survival_reg() %>%
    set_engine("flexsurvspline", k = 1) %>%
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_error(
    predict(f_fit, head(lung), type = "hazard"),
    "a numeric vector 'time'"
  )

  f_pred <- predict(f_fit, head(lung), type = "hazard",
                    time = c(0, 500, 1000))

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(head(lung)))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(3, 2))))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~ all(names(.x) == c(".time", ".pred_hazard"))))
  )
  expect_equal(f_pred, exp_pred)
})
