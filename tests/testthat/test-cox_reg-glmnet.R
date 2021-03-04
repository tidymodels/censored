library(testthat)
library(survival)
library(glmnet)
library(rlang)

# ------------------------------------------------------------------------------

context("Cox Regression - glmnet")

# ------------------------------------------------------------------------------

lung2 <- lung[-14, ]

cox_spec <- cox_reg() %>% set_engine("glmnet")

exp_f_fit <- glmnet(x = as.matrix(lung2[, c(4, 6)]),
                    y = Surv(lung2$time, lung2$status),
                    family = "cox")

# ------------------------------------------------------------------------------

test_that("model object", {

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)

  # Removing call element
  expect_equal(f_fit$fit$fit[-11], exp_f_fit[-11])
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

test_that("survival predictions", {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)
  expect_error(predict(f_fit, lung2, type = "survival", penalty = 0.01),
               "When using 'type' values of 'survival' or 'hazard' are given")
  f_pred <- predict(f_fit, lung2, type = "survival", penalty = 0.01, .time = 100:200)

  lp <- predict(exp_f_fit, as.matrix(lung2[, c(4, 6)]), s = 0.01, type = "link")
  exp_surv <- Surv(lung2$time, lung2$status)
  exp_f_pred <- calculate_survival_prop(lp = lp,
                                        time = exp_surv[, 1],
                                        event = exp_surv[, 2],
                                        survtime = 100:200)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred_survival")
  expect_equal(nrow(f_pred), nrow(lung2))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred_survival,
                       ~ all(dim(.x) == c(101, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred_survival,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred_survival))$.pred_survival,
    as.numeric(t(exp_f_pred))
  )
})

# ------------------------------------------------------------------------------

test_that("api errors", {
  expect_error(
    cox_reg() %>% set_engine("lda"),
    regexp = "Engine 'lda' is not available"
  )
})


# ------------------------------------------------------------------------------

test_that("printing", {
  expect_output(
    print(cox_reg()),
    "Cox Model Specification \\(censored regression\\)"
  )
})

# ------------------------------------------------------------------------------

test_that("primary arguments", {

  # penalty ------------------------------------------------------
  penalty <- cox_reg(penalty = 0.05) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  expect_equal(translate(penalty)$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg())
               )
  )

  # mixture -----------------------------------------------------------
  mixture <- cox_reg(mixture = 0.34) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  expect_equal(translate(mixture)$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 alpha = new_empty_quosure(0.34)
               )
  )

  mixture_v <- cox_reg(mixture = varying()) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  expect_equal(translate(mixture_v)$method$fit$args,
               list(
                 x = expr(missing_arg()),
                 y = expr(missing_arg()),
                 weights = expr(missing_arg()),
                 alpha = new_empty_quosure(varying())
               )
  )
})

# ------------------------------------------------------------------------------

test_that("updating", {
  expr1 <- cox_reg() %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")
  expr1_exp <- cox_reg(mixture = 0.76) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  expr2 <- cox_reg() %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")
  expr2_exp <- cox_reg(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")


  expect_equal(update(expr1, mixture = 0.76), expr1_exp)
  expect_equal(update(expr2, penalty = 0.123), expr2_exp)
})
