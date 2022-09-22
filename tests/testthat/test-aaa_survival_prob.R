test_that("survfit_summary_typestable() works for survival prob - unstratified", {
  lung_pred <- tidyr::drop_na(lung)
  mod <- coxph(Surv(time, status) ~ ., data = lung)

  # multiple observations
  surv_fit <- survfit(mod, newdata = lung_pred)

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))
  expect_true(all(prob[1,] > prob[2,]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))

  # single observation
  surv_fit <- survfit(mod, newdata = lung_pred[1,])

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
  expect_true(all(prob[1,] > prob[2,]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
})

test_that("survfit_summary_typestable() works for survival prob - stratified", {
  lung_pred <- tidyr::drop_na(lung)
  mod <- coxph(Surv(time, status) ~ age + ph.ecog + strata(sex), data = lung)

  # multiple observations
  surv_fit <- survfit(mod, newdata = lung_pred)

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))
  expect_true(all(prob[1,] > prob[2,]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))

  # single observation
  surv_fit <- survfit(mod, newdata = lung_pred[1,])

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
  expect_true(all(prob[1,] > prob[2,]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
})


test_that("survfit_summary_patch_infinite_time() works", {
  lung_pred <- tidyr::drop_na(lung)
  pred_time <- c(-Inf, 0, Inf, 1022, -Inf)

  mod <- coxph(Surv(time, status) ~ ., data = lung)
  surv_fit <- survfit(mod, newdata = lung_pred)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  surv_fit_summary_patched  <- surv_fit_summary %>%
     survfit_summary_typestable() %>%
     survfit_summary_patch_infinite_time(time = pred_time)

  prob <- surv_fit_summary_patched$surv
  exp_prob <- surv_fit_summary$surv

  expect_equal(prob[c(3,4),], exp_prob)
  expect_equal(
    prob[c(1,2),],
    matrix(1, nrow = 2, ncol = nrow(lung_pred)),
    ignore_attr = "dimnames"
  )
  expect_equal(unname(prob[5,]), rep(0, nrow(lung_pred)))
})

test_that("survfit_summary_restore_time_order() works", {
  lung_pred <- tidyr::drop_na(lung)
  pred_time <- c(300, 100, 200)

  mod <- coxph(Surv(time, status) ~ ., data = lung)
  surv_fit <- survfit(mod, newdata = lung_pred)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  surv_fit_summary_patched  <- surv_fit_summary %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_infinite_time(time = pred_time) %>%
    survfit_summary_restore_time_order(time = pred_time)

  prob <- surv_fit_summary_patched$surv
  exp_prob <- surv_fit_summary$surv

  expect_equal(prob, exp_prob[c(3,1:2),])
})


test_that("survfit_summary_patch_missings() works", {
  # expects that there are missings
  # test "all missings" case one level up


  skip("not yet")
  pred_time <- c(100, 200)

  mod <- coxph(Surv(time, status) ~ ., data = lung)
  surv_fit <- survfit(mod, newdata = lung_pred)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  surv_fit_summary_patched  <- surv_fit_summary %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_missings(time = pred_time)

  prob <- surv_fit_summary_patched$surv
  exp_prob <- surv_fit_summary$surv

  expect_equal(prob, exp_prob[c(3,1:2),])
})


test_that("survfit_summary_to_tibble() works", {
  skip("do we need this?")

  lung_pred <- tidyr::drop_na(lung)
  pred_time <- c(100, 200)
  n_obs <- nrow(lung_pred)

  mod <- coxph(Surv(time, status) ~ ., data = lung)
  surv_fit <- survfit(mod, newdata = lung_pred)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  pred <- surv_fit_summary %>%
    survfit_summary_typestable() %>%
    survfit_summary_to_tibble(time = pred_time, n_obs = n_obs)

})


#
# # can handle all at once
# pred_time_inf <- c(-Inf, 0, Inf, 1022, -Inf)
# prob <- survfit_to_prob(surv_fit, time = pred_time_inf)
# exp_prob <- summary(surv_fit, times = pred_time_inf, extend = TRUE)$surv
# expect_equal(prob[c(2,4),], exp_prob)
# expect_equal(
#   prob[c(1,5),],
#   matrix(1, nrow = 2, ncol = nrow(lung_pred)),
#   ignore_attr = "dimnames"
# )
# expect_equal(unname(prob[3,]), rep(0, nrow(lung_pred)))
