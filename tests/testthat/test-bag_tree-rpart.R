library(testthat)

test_that("model object", {
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


# prediction: time --------------------------------------------------------

test_that("time predictions", {
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
})

test_that("time predictions without surrogate splits for NA", {

  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  f_fit <- fit(mod_spec, Surv(time, status) ~ ph.ecog, data = lung)

  # lung$ph.ecog[14] is NA
  new_data_3 <- lung[13:15,]

  expect_error(
    f_pred <- predict(f_fit, new_data_3, type = "time"),
    NA
  )
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_equal(which(is.na(f_pred$.pred_time)), 2)

})

# prediction: survival ----------------------------------------------------

test_that("survival predictions", {
  set.seed(1234)
  exp_f_fit <- ipred::bagging(Surv(time, status) ~ age + ph.ecog, data = lung)

  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  expect_error(predict(f_fit, lung, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")

  f_pred <- predict(f_fit, lung, type = "survival", time = 100:200)
  exp_f_pred <- purrr::map(predict(exp_f_fit, lung),
                           ~ summary(.x, times = c(100:200))$surv)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(dim(.x) == c(101, 2))))
  )
  expect_true(
    all(purrr::map_lgl(f_pred$.pred, ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    unlist(exp_f_pred)
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.time,
    rep(100:200, nrow(lung))
  )

  # Out of domain prediction
  f_pred <- predict(f_fit, lung, type = "survival", time = 10000)
  exp_f_pred <- purrr::map(predict(exp_f_fit, lung),
                           ~ summary(.x, times = c(max(.x$time)))$surv)

  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    unlist(exp_f_pred)
  )
})

test_that("survival predictions without surrogate splits for NA", {

  mod_spec <- bag_tree(engine = "rpart") %>% set_mode("censored regression")
  f_fit <- fit(mod_spec, Surv(time, status) ~ ph.ecog, data = lung)

  # lung$ph.ecog[14] is NA
  new_data_3 <- lung[13:15,]

  expect_error(
    f_pred <- predict(f_fit, new_data_3, type = "survival",
                      time = c(100, 500, 1000)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_true(!any(is.na(f_pred$.pred[[1]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))
  expect_true(!any(is.na(f_pred$.pred[[3]]$.pred_survival)))

})
