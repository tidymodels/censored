library(testthat)
library(survival)
library(glmnet)
library(rlang)

# ------------------------------------------------------------------------------

context("Cox Regression - glmnet")

# ------------------------------------------------------------------------------

lung2 <- lung[-14, ]

cox_spec <- proportional_hazards(penalty = 0.123) %>% set_engine("glmnet")

exp_f_fit <- glmnet(x = as.matrix(lung2[, c(4, 6)]),
                    y = Surv(lung2$time, lung2$status),
                    family = "cox")

# ------------------------------------------------------------------------------

test_that("model object", {

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)

  # Removing call element
  expect_equal(f_fit$fit[-11], exp_f_fit[-11])
})

# ------------------------------------------------------------------------------

test_that("linear_pred predictions", {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01)
  exp_f_pred <- unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))

  # TODO tests for multi_predict()
})

# ------------------------------------------------------------------------------

test_that("api errors", {
  expect_error(
    proportional_hazards() %>% set_engine("lda"),
    regexp = "Available engines are:"
  )
})

# ------------------------------------------------------------------------------

test_that("primary arguments", {

  # penalty ------------------------------------------------------
  penalty <- proportional_hazards(penalty = 0.05) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  expect_equal(translate(penalty)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 family = expr(missing_arg())
               )
  )

  expect_error(
    translate(proportional_hazards() %>% set_engine("glmnet")),
    "For the glmnet engine, `penalty` must be a single"
  )

  # mixture -----------------------------------------------------------
  mixture <- proportional_hazards(mixture = 0.34, penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  expect_equal(translate(mixture)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 family = expr(missing_arg()),
                 alpha = new_empty_quosure(0.34)
               )
  )

  mixture_v <- proportional_hazards(mixture = varying(), penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  expect_equal(translate(mixture_v)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 family = expr(missing_arg()),
                 alpha = new_empty_quosure(varying())
               )
  )
})

# ------------------------------------------------------------------------------

test_that("updating", {
  expr1 <- proportional_hazards() %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")
  expr1_exp <- proportional_hazards(mixture = 0.76) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  expr2 <- proportional_hazards() %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")
  expr2_exp <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")


  expect_equal(update(expr1, mixture = 0.76), expr1_exp)
  expect_equal(update(expr2, penalty = 0.123), expr2_exp)
})


# -------------------------------------------------------------------------

test_that("survival probabilities - non-stratified model", {

  # load the `lung` dataset
  data(cancer, package = "survival")
  # remove row with missing value
  lung2 <- lung[-14, ]

  cox_spec <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  set.seed(14)
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2),
    NA
  )

  expect_error(
    pred_1 <- predict(f_fit, new_data = lung2[1, ], type = "survival",
                      time = c(100, 200)),
    NA
  )

  new_data_3 <- lung2[1:3, ]
  f_pred <- predict(f_fit, new_data = new_data_3, type = "survival",
                    time = c(100, 200), penalty = 0.1)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
  )

  exp_pred_multi <-
    predict(f_fit, new_data = new_data_3, type = "survival",
            time = c(100, 200), penalty = 0.05) %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(penalty = 0.05,
                  .row = rep(1:3, each = 2)) %>%
    dplyr::bind_rows(
      f_pred %>%
        tidyr::unnest(cols = .pred) %>%
        dplyr::mutate(penalty = 0.1, .row = rep(1:3, each = 2))
    ) %>%
    dplyr::arrange(.row, .time) %>%
    dplyr::select(penalty, .time, .pred_survival)


  pred_multi <- multi_predict(f_fit, new_data = new_data_3,
                              type = "survival", time = c(100, 200),
                              penalty = c(0.05, 0.1))
  expect_s3_class(pred_multi, "tbl_df")
  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(pred_multi$.pred,
                       ~ all(dim(.x) == c(2*2, 3))))
  )
  expect_true(
    all(purrr::map_lgl(pred_multi$.pred,
                       ~ all(names(.x) == c("penalty", ".time", ".pred_survival"))))
  )
  expect_equal(
    pred_multi %>% tidyr::unnest(cols = .pred),
    exp_pred_multi
  )

})


test_that("survival probabilities - stratified model", {

  cox_spec <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  set.seed(14)
  expect_error(
    f_fit <- fit(cox_spec,
                 Surv(stop, event) ~ rx + size + number + strata(enum),
                 data = bladder),
    NA
  )

  new_data_3 <- bladder[1:3, ]
  f_pred <- predict(f_fit, new_data = new_data_3, type = "survival",
                    time = c(5, 10), penalty = 0.1)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
  )

  exp_pred_multi <-
    predict(f_fit, new_data = new_data_3, type = "survival",
            time = c(5, 10), penalty = 0.05) %>% ## change the time at the end
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(penalty = 0.05,
                  .row = rep(1:3, each = 2)) %>%
    dplyr::bind_rows(
      f_pred %>%
        tidyr::unnest(cols = .pred) %>%
        dplyr::mutate(penalty = 0.1, .row = rep(1:3, each = 2))
      ) %>%
    dplyr::arrange(.row, .time) %>%
    dplyr::select(penalty, .time, .pred_survival)

  pred_multi <- multi_predict(f_fit, new_data = new_data_3,
                              type = "survival", time = c(5, 10), ## change the time at the end
                              penalty = c(0.05, 0.1))
  expect_s3_class(pred_multi, "tbl_df")
  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(pred_multi$.pred,
                       ~ all(dim(.x) == c(2*2, 3))))
  )
  expect_true(
    all(purrr::map_lgl(pred_multi$.pred,
                       ~ all(names(.x) == c("penalty", ".time", ".pred_survival"))))
  )
  expect_equal(
    pred_multi %>% tidyr::unnest(cols = .pred),
    exp_pred_multi
  )

})

# helper functions --------------------------------------------------------

test_that("formula modifications", {
  # base case
  expect_equal(
    drop_strata(expr(x + strata(s))),
    expr(x)
  )

  expect_equal(
    drop_strata(expr(x + x + x + strata(s))),
    expr(x + x + x)
  )
  expect_equal(
    drop_strata(expr(x * (y + strata(s)) + z)),
    expr(x * (y + strata(s)) + z)
  )

  expect_error(
    check_strata_remaining(expr(x * (y + strata(s)) + z))
  )
})
