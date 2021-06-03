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

  pred_2 <- predict(f_fit, new_data = lung2[1:2, ], type = "survival",
                    time = c(100, 200), penalty = 0.1)

  expect_s3_class(pred_2, "tbl_df")
  expect_equal(names(pred_2), ".pred")
  expect_equal(nrow(pred_2), 2)
  expect_true(
    all(purrr::map_lgl(pred_2$.pred, ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(pred_2$.pred,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
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

  pred_2 <- predict(f_fit, new_data = bladder[1:2, ], type = "survival",
                    time = c(5, 10), penalty = 0.1)

  expect_s3_class(pred_2, "tbl_df")
  expect_equal(names(pred_2), ".pred")
  expect_equal(nrow(pred_2), 2)
  expect_true(
    all(purrr::map_lgl(pred_2$.pred, ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(pred_2$.pred,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
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
    check_for_incorrect_strata_usage(expr(x * (y + strata(s)) + z))
  )
})
