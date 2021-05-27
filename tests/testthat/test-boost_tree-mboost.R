library(testthat)
library(survival)
library(mboost)
library(rlang)

# ------------------------------------------------------------------------------

context("Boosted Tree - mboost")

# ------------------------------------------------------------------------------

lung2 <- lung[-14, ]

cox_spec <- boost_tree() %>% set_mode("censored regression") %>% set_engine("mboost")

exp_f_fit <- blackboost(Surv(time, status) ~ age + ph.ecog,
                        data = lung2,
                        family = CoxPH())

# ------------------------------------------------------------------------------

test_that("model object", {

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)

  # Removing x element from f_fit and call from both
  # expect_equal(unclass(f_fit$fit)[-24], unclass(exp_f_fit)[-24])
})

# ------------------------------------------------------------------------------

test_that("survival predictions", {
  pred_time <- c(0, 100, 200, 10000)

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)
  expect_error(predict(f_fit, lung2, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")
  f_pred <- predict(f_fit, lung2, type = "survival", time = pred_time)
  exp_f_pred <- mboost::survFit(exp_f_fit, lung2)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred_survival")
  expect_equal(nrow(f_pred), nrow(lung2))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred_survival,
                       ~ all(dim(.x) == c(4, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred_survival,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred_survival))$.time,
    rep(pred_time, nrow(lung2))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred_survival))$.pred_survival,
    as.numeric(t(floor_surv_mboost(exp_f_pred, pred_time)))
  )
})

# ------------------------------------------------------------------------------

test_that("linear_pred predictions", {
  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)
  f_pred <- predict(f_fit, lung2, type = "linear_pred")
  exp_f_pred <- unname(predict(exp_f_fit, newdata = lung2))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))
})

# ------------------------------------------------------------------------------

test_that("primary arguments", {

  # mtry ------------------------------------------------------
  mtry <- boost_tree(mtry = 5) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(mtry)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 mtry = new_empty_quosure(5),
                 family = mboost::CoxPH()
               )
  )

  mtry_v <- boost_tree(mtry = varying()) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(mtry_v)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 mtry = new_empty_quosure(varying()),
                 family = mboost::CoxPH()
               )
  )

  # trees -----------------------------------------------------------
  trees <- boost_tree(trees = 1000) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(trees)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 mstop = new_empty_quosure(1000),
                 family = mboost::CoxPH()
               )
  )

  trees_v <- boost_tree(trees = varying()) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(trees_v)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 mstop = new_empty_quosure(varying()),
                 family = mboost::CoxPH()
               )
  )

  # tree_depth ------------------------------------------------------
  tree_depth <- boost_tree(tree_depth = 3) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(tree_depth)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 maxdepth = new_empty_quosure(3),
                 family = mboost::CoxPH()
               )
  )

  tree_depth_v <- boost_tree(tree_depth = varying()) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(tree_depth_v)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 maxdepth = new_empty_quosure(varying()),
                 family = mboost::CoxPH()
               )
  )

  # min_n ------------------------------------------------------
  min_n <- boost_tree(min_n = 10) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(min_n)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 minsplit = new_empty_quosure(10),
                 family = mboost::CoxPH()
               )
  )

  min_n_v <- boost_tree(min_n = varying()) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(min_n_v)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 minsplit = new_empty_quosure(varying()),
                 family = mboost::CoxPH()
               )
  )

  # loss_reduction ------------------------------------------------------
  loss_reduction <- boost_tree(loss_reduction = 0.2) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(loss_reduction)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 mincriterion = new_empty_quosure(0.2),
                 family = mboost::CoxPH()
               )
  )

  loss_reduction_v <- boost_tree(loss_reduction = varying()) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(translate(loss_reduction_v)$method$fit$args,
               list(
                 formula = expr(missing_arg()),
                 data = expr(missing_arg()),
                 mincriterion = new_empty_quosure(varying()),
                 family = mboost::CoxPH()
               )
  )

})

# ------------------------------------------------------------------------------

test_that("updating", {
  expr1 <- boost_tree() %>%
    set_mode("censored regression") %>%
    set_engine("mboost")
  expr1_exp <- boost_tree(tree_depth = 10) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expr2 <- boost_tree() %>%
    set_mode("censored regression") %>%
    set_engine("mboost")
  expr2_exp <- boost_tree(trees = 10) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expr3 <- boost_tree() %>%
    set_mode("censored regression") %>%
    set_engine("mboost")
  expr3_exp <- boost_tree(mtry = 10) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expr4 <- boost_tree() %>%
    set_mode("censored regression") %>%
    set_engine("mboost")
  expr4_exp <- boost_tree(min_n = 10) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expr5 <- boost_tree() %>%
    set_mode("censored regression") %>%
    set_engine("mboost")
  expr5_exp <- boost_tree(loss_reduction = 10) %>%
    set_mode("censored regression") %>%
    set_engine("mboost")

  expect_equal(update(expr1, tree_depth = 10), expr1_exp)
  expect_equal(update(expr2, trees = 10), expr2_exp)
  expect_equal(update(expr3, mtry = 10), expr3_exp)
  expect_equal(update(expr4, min_n = 10), expr4_exp)
  expect_equal(update(expr5, loss_reduction = 10), expr5_exp)

})
