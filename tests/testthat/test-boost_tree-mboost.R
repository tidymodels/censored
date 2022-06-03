library(testthat)

test_that("model object", {
  lung2 <- lung[-14, ]
  exp_f_fit <- mboost::blackboost(Surv(time, status) ~ age + ph.ecog,
                                  data = lung2,
                                  family = mboost::CoxPH())

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


test_that("survival predictions", {
  pred_time <- c(0, 100, 200, 10000)

  lung2 <- lung[-14, ]
  exp_f_fit <- mboost::blackboost(Surv(time, status) ~ age + ph.ecog,
                                  data = lung2,
                                  family = mboost::CoxPH())
  cox_spec <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2)

  expect_error(predict(f_fit, lung2, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")

  f_pred <- predict(f_fit, lung2, type = "survival", time = pred_time)
  exp_f_pred <- mboost::survFit(exp_f_fit, lung2)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung2))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(dim(.x) == c(4, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.time,
    rep(pred_time, nrow(lung2))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    as.numeric(t(floor_surv_mboost(exp_f_pred, pred_time)))
  )
})

test_that("linear_pred predictions", {
  lung2 <- lung[-14, ]
  exp_f_fit <- mboost::blackboost(Surv(time, status) ~ age + ph.ecog,
                                  data = lung2,
                                  family = mboost::CoxPH())
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
})


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

# ------------------------------------------------------------------------------

test_that("primary arguments", {

  # mtry
  mtry <- boost_tree(mtry = 5) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(mtry)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 mtry = rlang::quo(5),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  mtry_v <- boost_tree(mtry = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(mtry_v)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 mtry = rlang::new_quosure(hardhat::tune()),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  # trees
  trees <- boost_tree(trees = 1000) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(trees)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 mstop = rlang::quo(1000),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  trees_v <- boost_tree(trees = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(trees_v)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 mstop = rlang::new_quosure(hardhat::tune()),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  # tree_depth
  tree_depth <- boost_tree(tree_depth = 3) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(tree_depth)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 maxdepth = rlang::quo(3),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  tree_depth_v <- boost_tree(tree_depth = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(tree_depth_v)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 maxdepth = rlang::new_quosure(hardhat::tune()),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  # min_n
  min_n <- boost_tree(min_n = 10) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(min_n)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 minsplit = rlang::quo(10),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  min_n_v <- boost_tree(min_n = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(min_n_v)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 minsplit = rlang::new_quosure(hardhat::tune()),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  # loss_reduction
  loss_reduction <- boost_tree(loss_reduction = 0.2) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(loss_reduction)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 mincriterion = rlang::quo(0.2),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

  loss_reduction_v <- boost_tree(loss_reduction = tune()) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(translate(loss_reduction_v)$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 mincriterion = rlang::new_quosure(hardhat::tune()),
                 family = rlang::expr(mboost::CoxPH())
               )
  )

})

test_that("updating", {
  expr1 <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  expr1_exp <- boost_tree(tree_depth = 10) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expr2 <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  expr2_exp <- boost_tree(trees = 10) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expr3 <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  expr3_exp <- boost_tree(mtry = 10) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expr4 <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  expr4_exp <- boost_tree(min_n = 10) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expr5 <- boost_tree() %>%
    set_engine("mboost") %>%
    set_mode("censored regression")
  expr5_exp <- boost_tree(loss_reduction = 10) %>%
    set_engine("mboost") %>%
    set_mode("censored regression")

  expect_equal(update(expr1, tree_depth = 10), expr1_exp)
  expect_equal(update(expr2, trees = 10), expr2_exp)
  expect_equal(update(expr3, mtry = 10), expr3_exp)
  expect_equal(update(expr4, min_n = 10), expr4_exp)
  expect_equal(update(expr5, loss_reduction = 10), expr5_exp)
})
