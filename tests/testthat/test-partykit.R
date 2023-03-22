test_that("survival_prob_partykit() works for ctree", {
  set.seed(1234)
  # use only ph.ecog to rule out surrogate splits
  mod <- partykit::ctree(Surv(time, status) ~ ph.ecog, data = lung)

  # time: combination of order, out-of-range, infinite
  pred_time <- c(-Inf, 0, 100, Inf, 1022, 3000)

  # multiple observations
  # (with 1 missing but partykit takes care of that)
  lung_pred <- lung[13:15, ]
  surv_fit <- predict(mod, newdata = lung_pred, type = "prob")
  surv_fit_summary <- purrr::map(
    surv_fit,
    summary,
    times = pred_time,
    extend = TRUE
  ) %>%
    combine_list_of_survfit_summary(eval_time = pred_time)

  prob <- survival_prob_partykit(mod, new_data = lung_pred, eval_time = pred_time) %>%
    tidyr::unnest(cols = .pred)
  exp_prob <- surv_fit_summary$surv

  expect_equal(
    prob$.eval_time,
    rep(pred_time, nrow(lung_pred))
  )
  expect_equal(
    prob %>% dplyr::filter(is.infinite(.eval_time)) %>% dplyr::pull(.pred_survival),
    rep(c(1, 0), nrow(lung_pred))
  )
  expect_equal(
    prob %>% dplyr::filter(is.finite(.eval_time)) %>% dplyr::pull(.pred_survival),
    as.vector(exp_prob)
  )

  # single observation
  lung_pred <- lung[13, ]
  surv_fit <- predict(mod, newdata = lung_pred, type = "prob")
  surv_fit_summary <- purrr::map(
    surv_fit,
    summary,
    times = pred_time,
    extend = TRUE
  ) %>%
    combine_list_of_survfit_summary(eval_time = pred_time)

  prob <- survival_prob_partykit(mod, new_data = lung_pred, eval_time = pred_time) %>%
    tidyr::unnest(cols = .pred)
  exp_prob <- surv_fit_summary$surv

  expect_equal(
    prob %>% dplyr::filter(is.infinite(.eval_time)) %>% dplyr::pull(.pred_survival),
    c(1, 0)
  )
  expect_equal(
    prob %>% dplyr::filter(is.finite(.eval_time)) %>% dplyr::pull(.pred_survival),
    as.vector(exp_prob)
  )

  # all observations with missings
  lung_pred <- lung[c(14, 14), ]

  prob <- survival_prob_partykit(mod, new_data = lung_pred, eval_time = pred_time) %>%
    tidyr::unnest(cols = .pred)

  expect_true(all(!is.na(prob$.pred_survival)))
})

test_that("survival_prob_partykit() works for cforest", {
  # partykit::cforest takes care of missing values via some form of randomness
  # hence set the seed before predicting on data with missings

  mod <- partykit::cforest(Surv(time, status) ~ age + ph.ecog, data = lung)

  # time: combination of order, out-of-range, infinite
  pred_time <- c(-Inf, 0, 100, Inf, 1022, 3000)

  # multiple observations (with 1 missing)
  lung_pred <- lung[13:15, ]
  set.seed(1234)
  surv_fit <- predict(mod, newdata = lung_pred, type = "prob")
  surv_fit_summary <- purrr::map(
    surv_fit,
    summary,
    times = pred_time,
    extend = TRUE
  ) %>%
    combine_list_of_survfit_summary(eval_time = pred_time)

  set.seed(1234)
  prob <- survival_prob_partykit(mod, new_data = lung_pred, eval_time = pred_time) %>%
    tidyr::unnest(cols = .pred)
  exp_prob <- surv_fit_summary$surv

  expect_equal(
    prob$.eval_time,
    rep(pred_time, nrow(lung_pred))
  )
  expect_equal(
    prob %>% dplyr::filter(is.infinite(.eval_time)) %>% dplyr::pull(.pred_survival),
    rep(c(1, 0), nrow(lung_pred))
  )
  expect_equal(
    prob %>% dplyr::filter(is.finite(.eval_time)) %>% dplyr::pull(.pred_survival),
    as.vector(exp_prob)
  )

  # single observation
  lung_pred <- lung[13, ]
  set.seed(1234)
  surv_fit <- predict(mod, newdata = lung_pred, type = "prob")
  surv_fit_summary <- purrr::map(
    surv_fit,
    summary,
    times = pred_time,
    extend = TRUE
  ) %>%
    combine_list_of_survfit_summary(eval_time = pred_time)

  set.seed(1234)
  prob <- survival_prob_partykit(mod, new_data = lung_pred, eval_time = pred_time) %>%
    tidyr::unnest(cols = .pred)
  exp_prob <- surv_fit_summary$surv

  expect_equal(
    prob %>% dplyr::filter(is.infinite(.eval_time)) %>% dplyr::pull(.pred_survival),
    c(1, 0)
  )
  expect_equal(
    prob %>% dplyr::filter(is.finite(.eval_time)) %>% dplyr::pull(.pred_survival),
    as.vector(exp_prob)
  )

  # all observations with missings
  lung_pred <- lung[c(14, 14), ]

  prob <- survival_prob_partykit(mod, new_data = lung_pred, eval_time = pred_time) %>%
    tidyr::unnest(cols = .pred)

  expect_true(all(!is.na(prob$.pred_survival)))
})
