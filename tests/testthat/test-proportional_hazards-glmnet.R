library(testthat)
library(survival)
library(glmnet)
library(rlang)

# ------------------------------------------------------------------------------

test_that("model object", {
  lung2 <- lung[-14, ]
  cox_spec <- proportional_hazards(penalty = 0.123) %>% set_engine("glmnet")
  exp_f_fit <- glmnet(x = as.matrix(lung2[, c(4, 6)]),
                      y = Surv(lung2$time, lung2$status),
                      family = "cox")

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)

  # Removing call element
  expect_equal(f_fit$fit[-11], exp_f_fit[-11])
})

# ------------------------------------------------------------------------------

test_that("linear_pred predictions without strata", {
  lung2 <- lung[-14, ]
  cox_spec <- proportional_hazards(penalty = 0.123) %>% set_engine("glmnet")
  exp_f_fit <- glmnet(x = as.matrix(lung2[, c(4, 6)]),
                      y = Surv(lung2$time, lung2$status),
                      family = "cox")
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2), NA)

  # predict
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01)
  exp_f_pred <- -unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))

  # single observation
  expect_error(f_pred_1 <- predict(f_fit, lung2[1,], type = "linear_pred"), NA)
  expect_equal(nrow(f_pred_1), 1)

  # predict without the sign flip
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01, increasing = FALSE)
  exp_f_pred <- unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))


  # multi_predict
  new_data_3 <- lung2[1:3, ]
  f_pred_unnested_01 <-
    predict(f_fit, new_data_3, type = "linear_pred", penalty = 0.1) %>%
    dplyr::mutate(penalty = 0.1, .row = seq_len(nrow(new_data_3)))
  f_pred_unnested_005 <-
    predict(f_fit, new_data_3, type = "linear_pred", penalty = 0.05) %>%
    dplyr::mutate(penalty = 0.05, .row = seq_len(nrow(new_data_3)))
  exp_pred_multi_unnested <-
    dplyr::bind_rows(
      f_pred_unnested_005,
      f_pred_unnested_01
    ) %>%
    dplyr::arrange(.row, penalty) %>%
    dplyr::select(penalty, .pred_linear_pred) %>%
    dplyr::mutate(.pred_linear_pred = -.pred_linear_pred)

  pred_multi <- multi_predict(f_fit, new_data_3, type = "linear_pred",
                              penalty = c(0.05, 0.1))
  expect_s3_class(pred_multi, "tbl_df")
  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(pred_multi$.pred,
                       ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(pred_multi$.pred,
                       ~ all(names(.x) == c("penalty", ".pred_linear_pred"))))
  )
  expect_equal(
    pred_multi %>% tidyr::unnest(cols = .pred),
    exp_pred_multi_unnested
  )

})

test_that("linear_pred predictions with strata", {

  # TODO find a better example?

  lung2 <- lung[-14, ]
  cox_spec <- proportional_hazards(penalty = 0.123) %>% set_engine("glmnet")
  exp_f_fit <- suppressWarnings(
    glmnet(x = as.matrix(lung2[, c(4, 6)]),
           y = stratifySurv(Surv(lung2$time, lung2$status), lung2$sex),
           family = "cox")
  )
  expect_error(
    suppressWarnings(
      f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog + strata(sex),
                 data = lung2)
    ),
    NA
  )

  # predict
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01)
  exp_f_pred <- -unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))

  # single observation
  expect_error(f_pred_1 <- predict(f_fit, lung2[1,], type = "linear_pred"), NA)
  expect_equal(nrow(f_pred_1), 1)

  # predict without the sign flip
  f_pred <- predict(f_fit, lung2, type = "linear_pred", penalty = 0.01, increasing = FALSE)
  exp_f_pred <- unname(predict(exp_f_fit, newx = as.matrix(lung2[, c(4, 6)]), s = 0.01))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equivalent(f_pred$.pred_linear_pred, unname(exp_f_pred))
  expect_equal(nrow(f_pred), nrow(lung2))


  # multi_predict
  new_data_3 <- lung2[1:3, ]
  f_pred_unnested_01 <-
    predict(f_fit, new_data_3, type = "linear_pred", penalty = 0.1) %>%
    dplyr::mutate(penalty = 0.1, .row = seq_len(nrow(new_data_3)))
  f_pred_unnested_005 <-
    predict(f_fit, new_data_3, type = "linear_pred", penalty = 0.05) %>%
    dplyr::mutate(penalty = 0.05, .row = seq_len(nrow(new_data_3)))
  exp_pred_multi_unnested <-
    dplyr::bind_rows(
      f_pred_unnested_005,
      f_pred_unnested_01
    ) %>%
    dplyr::arrange(.row, penalty) %>%
    dplyr::select(penalty, .pred_linear_pred) %>%
    dplyr::mutate(.pred_linear_pred = -.pred_linear_pred)

  pred_multi <- multi_predict(f_fit, new_data_3, type = "linear_pred",
                              penalty = c(0.05, 0.1))
  expect_s3_class(pred_multi, "tbl_df")
  expect_equal(names(pred_multi), ".pred")
  expect_equal(nrow(pred_multi), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(pred_multi$.pred,
                       ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(purrr::map_lgl(pred_multi$.pred,
                       ~ all(names(.x) == c("penalty", ".pred_linear_pred"))))
  )
  expect_equal(
    pred_multi %>% tidyr::unnest(cols = .pred),
    exp_pred_multi_unnested
  )

})



# ------------------------------------------------------------------------------

test_that("api errors", {
  expect_error(
    proportional_hazards() %>% set_engine("lda"),
    regexp = "Engine 'lda' is not supported"
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

test_that("survival probabilities without strata", {

  # load the `lung` dataset
  data(cancer, package = "survival")
  # remove row with missing value
  lung2 <- lung[-14, ]
  new_data_3 <- lung2[1:3, ]

  cox_spec <- proportional_hazards(penalty = 0.123) %>%
    set_mode("censored regression") %>%
    set_engine("glmnet")

  set.seed(14)
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung2),
    NA
  )

  # predict
  expect_error(
    pred_1 <- predict(f_fit, new_data = lung2[1, ], type = "survival",
                      time = c(100, 200)),
    NA
  )

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

  # single observation
  expect_error(
    f_pred_1 <- predict(f_fit, lung2[1,], type = "survival", time = c(100, 200)),
    NA
  )
  expect_equal(nrow(f_pred_1), 1)

  # multi_predict
  f_pred_unnested_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(penalty = 0.1, .row = rep(1:3, each = 2))
  f_pred_unnested_005 <-
    predict(f_fit, new_data = new_data_3, type = "survival",
            time = c(100, 200), penalty = 0.05) %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(penalty = 0.05,
                  .row = rep(1:3, each = 2))
  exp_pred_multi_unnested <-
    dplyr::bind_rows(f_pred_unnested_005, f_pred_unnested_01) %>%
    dplyr::arrange(.row, .time, penalty) %>%
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
    exp_pred_multi_unnested
  )

})


test_that("survival probabilities with strata", {

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

  # predict
  f_pred <- predict(f_fit, new_data = new_data_3,
                    type = "survival", time = c(10, 20), penalty = 0.1)

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
  # single observation
  expect_error(
    f_pred_1 <- predict(f_fit, bladder[1, ], type = "survival", time = c(10, 20)),
    NA
  )
  expect_equal(nrow(f_pred_1), 1)

  # multi_predict
  f_pred_unnested_01 <- f_pred %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(penalty = 0.1, .row = rep(1:3, each = 2))
  f_pred_unnested_005 <-
    predict(f_fit, new_data = new_data_3, type = "survival",
            time = c(10, 20), penalty = 0.05) %>%
    tidyr::unnest(cols = .pred) %>%
    dplyr::mutate(penalty = 0.05,
                  .row = rep(1:3, each = 2))
  exp_pred_multi_unnested <-
    dplyr::bind_rows(f_pred_unnested_005, f_pred_unnested_01) %>%
    dplyr::arrange(.row, .time, penalty) %>%
    dplyr::select(penalty, .time, .pred_survival)

  pred_multi <- multi_predict(f_fit, new_data = new_data_3,
                              type = "survival", time = c(10, 20),
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
    exp_pred_multi_unnested
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


# ------------------------------------------------------------------------------

test_that("predictions with strata and dot in formula", {
  # For R <= 3.6 only , glmnet models below give a warning for lack of convergence.
  skip_if(R.version$major == "3")

  cox_spec <- proportional_hazards(penalty = 0.001) %>% set_engine("glmnet")
  lung2 <- lung[, c("time", "status", "ph.ecog", "age", "sex")]
  lung2$sex <- factor(lung2$sex)
  lung2 <- lung2[complete.cases(lung2),]
  # formula method
  expect_warning(
    expect_error(
      f_fit <- fit(cox_spec, Surv(time, status) ~ . - sex + strata(sex), data = lung2),
      NA
    ),
    "algorithm did not converge"
  )
  expect_warning(
    expect_error(
      f_fit_2 <- fit(cox_spec, Surv(time, status) ~ ph.ecog  + age + strata(sex), data = lung2),
      NA
    ),
    "algorithm did not converge"
  )
  expect_warning(
    expect_error({
      predict(f_fit, lung2, type = "linear_pred")
      predict(f_fit, lung2, type = "survival", time = c(100, 300))
    },
    NA),
    "to new 6 after EncodeVars()"
  )
  expect_equal(
    predict(f_fit, lung2, type = "linear_pred"),
    predict(f_fit_2, lung2, type = "linear_pred")
  )
  expect_warning(
    expect_equal(
      predict(f_fit, lung2, type = "survival", time = c(100, 300)),
      predict(f_fit_2, lung2, type = "survival", time = c(100, 300))
    ),
    "to new 6 after EncodeVars()"
  )
})

