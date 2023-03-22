test_that("survfit_summary_typestable() works for survival prob - unstratified (coxph)", {
  lung_pred <- tidyr::drop_na(lung)
  mod <- coxph(Surv(time, status) ~ ., data = lung)

  # multiple observations
  surv_fit <- survfit(mod, newdata = lung_pred)

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))
  expect_true(all(prob[1, ] > prob[2, ]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))

  # single observation
  surv_fit <- survfit(mod, newdata = lung_pred[1, ])

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
  expect_true(all(prob[1, ] > prob[2, ]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
})

test_that("survfit_summary_typestable() works for survival prob - stratified (coxph)", {
  lung_pred <- tidyr::drop_na(lung)
  mod <- coxph(Surv(time, status) ~ age + ph.ecog + strata(sex), data = lung)

  # multiple observations
  surv_fit <- survfit(mod, newdata = lung_pred)

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))
  expect_true(all(prob[1, ] > prob[2, ]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))

  # single observation
  surv_fit <- survfit(mod, newdata = lung_pred[1, ])

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
  expect_true(all(prob[1, ] > prob[2, ]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
})

test_that("survfit_summary_typestable() works for survival prob - unstratified (coxnet)", {
  skip_if_not_installed("glmnet")

  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)
  lung_pred <- lung_x[1:5, ]

  mod <- suppressWarnings(
    glmnet::glmnet(x = lung_x, y = lung_y, family = "cox")
  )

  # multiple observations
  surv_fit <- survfit(mod, newx = lung_pred, s = 0.1, x = lung_x, y = lung_y)

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))
  expect_true(all(prob[1, ] > prob[2, ]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))

  # single observation
  surv_fit <- survfit(mod, newx = lung_pred[1, , drop = FALSE], s = 0.1, x = lung_x, y = lung_y)

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
  expect_true(all(prob[1, ] > prob[2, ]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
})

test_that("survfit_summary_typestable() works for survival prob - stratified (coxnet)", {
  skip_if_not_installed("glmnet")

  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- glmnet::stratifySurv(Surv(lung2$time, lung2$status), lung2$sex)
  lung_pred <- lung_x[1:5, ]
  lung_pred_strata <- lung2$sex[1:5]

  mod <- suppressWarnings(glmnet::glmnet(x = lung_x, y = lung_y, family = "cox"))

  # multiple observations
  surv_fit <- survfit(
    mod,
    newx = lung_pred,
    newstrata = lung_pred_strata,
    s = 0.1,
    x = lung_x,
    y = lung_y
  )

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))
  expect_true(all(prob[1, ] > prob[2, ]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), nrow(lung_pred)))

  # single observation
  surv_fit <- survfit(
    mod,
    newx = lung_pred[1, ],
    newstrata = lung_pred_strata[1],
    s = 0.1,
    x = lung_x,
    y = lung_y
  )

  pred_time <- c(100, 200)
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
  expect_true(all(prob[1, ] > prob[2, ]))

  pred_time <- 100
  surv_fit_summary <- summary(surv_fit, times = pred_time) %>%
    survfit_summary_typestable()
  prob <- surv_fit_summary$surv
  expect_equal(dim(prob), c(length(pred_time), 1))
})

test_that("survfit_summary_patch_infinite_time() works (coxph)", {
  lung_pred <- tidyr::drop_na(lung)
  pred_time <- c(-Inf, 0, Inf, 1022, -Inf)

  mod <- coxph(Surv(time, status) ~ ., data = lung)
  surv_fit <- survfit(mod, newdata = lung_pred)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  surv_fit_summary_patched <- surv_fit_summary %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_infinite_time(eval_time = pred_time)

  prob <- surv_fit_summary_patched$surv
  exp_prob <- surv_fit_summary$surv

  expect_equal(prob[c(3, 4), ], exp_prob)
  expect_equal(
    prob[c(1, 2), ],
    matrix(1, nrow = 2, ncol = nrow(lung_pred)),
    ignore_attr = "dimnames"
  )
  expect_equal(unname(prob[5, ]), rep(0, nrow(lung_pred)))
})

test_that("survfit_summary_patch_infinite_time() works (coxnet)", {
  skip_if_not_installed("glmnet")

  pred_time <- c(-Inf, 0, Inf, 1022, -Inf)

  lung2 <- lung[-14, ]
  lung_x <- as.matrix(lung2[, c("age", "ph.ecog")])
  lung_y <- Surv(lung2$time, lung2$status)
  lung_pred <- lung_x[1:5, ]

  mod <- suppressWarnings(
    glmnet::glmnet(x = lung_x, y = lung_y, family = "cox")
  )
  surv_fit <- survfit(mod, newx = lung_pred, s = 0.1, x = lung_x, y = lung_y)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  surv_fit_summary_patched <- surv_fit_summary %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_infinite_time(eval_time = pred_time)

  prob <- surv_fit_summary_patched$surv
  exp_prob <- surv_fit_summary$surv

  expect_equal(prob[c(3, 4), ], exp_prob)
  expect_equal(
    prob[c(1, 2), ],
    matrix(1, nrow = 2, ncol = nrow(lung_pred)),
    ignore_attr = "dimnames"
  )
  expect_equal(unname(prob[5, ]), rep(0, nrow(lung_pred)))
})

test_that("survfit_summary_restore_time_order() works", {
  lung_pred <- tidyr::drop_na(lung)
  pred_time <- c(300, 100, 200)

  mod <- coxph(Surv(time, status) ~ ., data = lung)
  surv_fit <- survfit(mod, newdata = lung_pred)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  surv_fit_summary_patched <- surv_fit_summary %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_infinite_time(eval_time = pred_time) %>%
    survfit_summary_restore_time_order(eval_time = pred_time)

  prob <- surv_fit_summary_patched$surv
  exp_prob <- surv_fit_summary$surv

  expect_equal(prob, exp_prob[c(3, 1:2), ])
})

test_that("survfit_summary_patch_missings() works", {
  pred_time <- c(100, 200)
  mod <- coxph(Surv(time, status) ~ age + ph.ecog, data = lung)

  lung_pred <- lung[13:14, ]
  surv_fit <- survfit(mod, newdata = lung_pred)
  surv_fit_summary <- summary(surv_fit, times = pred_time, extend = TRUE)

  surv_fit_summary_patched <- surv_fit_summary %>%
    survfit_summary_typestable() %>%
    survfit_summary_patch_missings(
      eval_time = pred_time,
      index_missing = 2,
      n_obs = 2
    )

  prob <- surv_fit_summary_patched$surv

  expect_equal(ncol(prob), nrow(lung_pred))
  expect_equal(prob[, 2], rep(NA_real_, length(pred_time)))
})

test_that("combine_list_of_survfit_summary() works for survbagg", {
  skip_if_not_installed("ipred")

  lung_pred <- tidyr::drop_na(lung)

  set.seed(1234)
  engine_fit <- ipred::bagging(Surv(time, status) ~ age + ph.ecog, data = lung)
  survfit_list <- predict(engine_fit, newdata = lung_pred)

  pred_time <- c(100, 200)
  survfit_summary_list <- purrr::map(
    survfit_list,
    summary,
    times = pred_time,
    extend = TRUE
  )

  survfit_summary <- combine_list_of_survfit_summary(
    survfit_summary_list,
    eval_time = pred_time
  )

  expect_equal(
    dim(survfit_summary$surv),
    c(length(pred_time), nrow(lung_pred))
  )
  expect_equal(
    dim(survfit_summary$cumhaz),
    c(length(pred_time), nrow(lung_pred))
  )
})

test_that("combine_list_of_survfit_summary() works for ctree", {
  skip_if_not_installed("partykit")

  lung_pred <- tidyr::drop_na(lung)

  set.seed(1234)
  engine_fit <- partykit::ctree(Surv(time, status) ~ age + ph.ecog, data = lung)
  survfit_list <- predict(engine_fit, newdata = lung_pred, type = "prob")

  pred_time <- c(100, 200)
  survfit_summary_list <- purrr::map(
    survfit_list,
    summary,
    times = pred_time,
    extend = TRUE
  )

  survfit_summary <- combine_list_of_survfit_summary(
    survfit_summary_list,
    eval_time = pred_time
  )

  expect_equal(
    dim(survfit_summary$surv),
    c(length(pred_time), nrow(lung_pred))
  )
  expect_equal(
    dim(survfit_summary$cumhaz),
    c(length(pred_time), nrow(lung_pred))
  )
})

test_that("combine_list_of_survfit_summary() works for cforest", {
  skip_if_not_installed("partykit")

  lung_pred <- tidyr::drop_na(lung)

  set.seed(1234)
  engine_fit <- partykit::cforest(Surv(time, status) ~ age + ph.ecog, data = lung)
  survfit_list <- predict(engine_fit, newdata = lung_pred, type = "prob")

  pred_time <- c(100, 200)
  survfit_summary_list <- purrr::map(
    survfit_list,
    summary,
    times = pred_time,
    extend = TRUE
  )

  survfit_summary <- combine_list_of_survfit_summary(
    survfit_summary_list,
    eval_time = pred_time
  )

  expect_equal(
    dim(survfit_summary$surv),
    c(length(pred_time), nrow(lung_pred))
  )
  expect_equal(
    dim(survfit_summary$cumhaz),
    c(length(pred_time), nrow(lung_pred))
  )
})
