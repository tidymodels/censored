library(testthat)

# survival has some issues where missing predictor value get ommited despite
# na.action = na.exclude. See https://github.com/therneau/survival/issues/137

test_that("model object", {

  exp_f_fit <- coxph(Surv(time, status) ~ age + sex, data = lung, x = TRUE)

  # formula method
  cox_spec <- proportional_hazards() %>% set_engine("survival")
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung), NA)

  # Removing `model` element from f_fit and `call` from both
  expect_equal(f_fit$fit[-c(16, 21)], exp_f_fit[-20], ignore_formula_env = TRUE)
})

# prediction: time --------------------------------------------------------

test_that("time predictions without strata", {
  cox_spec <- proportional_hazards() %>% set_engine("survival")
  exp_f_fit <- coxph(Surv(time, status) ~ age + sex, data = lung, x = TRUE)

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung), NA)
  f_pred <- predict(f_fit, lung, type = "time")
  tabs <- summary(survfit(exp_f_fit, lung, na.action = na.pass))$table
  colnames(tabs) <- gsub("[[:punct:]]", "", colnames(tabs))
  exp_f_pred <- unname(tabs[, "rmean"])

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(f_pred$.pred_time, exp_f_pred)
  expect_equal(nrow(f_pred), nrow(lung))

  # single observation
  expect_error(f_pred_1 <- predict(f_fit, lung[1,], type = "time"), NA)
  expect_equal(nrow(f_pred_1), 1)
})

test_that("time predictions with strata", {
  cox_spec <- proportional_hazards() %>% set_engine("survival")
  exp_f_fit <- coxph(Surv(time, status) ~ age + sex + strata(inst),
                     data = lung, x = TRUE)

  # formula method
  expect_error(f_fit <- cox_spec %>%
                 fit(Surv(time, status) ~ age + sex + strata(inst),
                     data = lung),
               NA)
  new_data_3 <- lung[1:3,]
  f_pred <- predict(f_fit, new_data_3, type = "time")
  tabs <- summary(survfit(exp_f_fit, new_data_3, na.action = na.pass))$table
  colnames(tabs) <- gsub("[[:punct:]]", "", colnames(tabs))
  exp_f_pred <- unname(tabs[, "rmean"])

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_time"))
  expect_equal(f_pred$.pred_time, exp_f_pred)
  expect_equal(nrow(f_pred), nrow(new_data_3))

  # single observation
  expect_error(f_pred_1 <- predict(f_fit, lung[1,], type = "time"), NA)
  expect_equal(nrow(f_pred_1), 1)

  # prediction without strata info should fail
  new_data_0 <- data.frame(age = c(50, 60), sex = 1)
  expect_error(
    predict(f_fit, new_data = new_data_0, type = "time"),
    "provide the strata"
  )
})

test_that("time predictions with NA", {

  cox_spec <- proportional_hazards() %>% set_engine("survival")
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + strata(ph.ecog),
                 data = lung),
    NA
  )

  # survfit.coxph() is not type-stable,
  # thus test against single or multiple survival curves
  # lung$ph.ecog[14] is NA
  na_x_data_x <- lung[c(13:15, 14),]
  na_x_data_1 <- lung[c(13, 14, 14),]
  na_x_data_0 <- lung[c(14, 14),]
  na_1_data_x <- lung[13:15,]
  na_1_data_1 <- lung[13:14,]
  na_1_data_0 <- lung[14,]

  # survival time
  expect_error(
    f_pred <- predict(f_fit, na_x_data_x, type = "time"),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_x))
  expect_equal(which(is.na(f_pred$.pred_time)), c(2, 4))

  expect_error(
    f_pred <- predict(f_fit, na_x_data_1, type = "time"),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_1))
  expect_equal(which(is.na(f_pred$.pred_time)), c(2, 3))

  expect_error(
    f_pred <- predict(f_fit, na_x_data_0, type = "time"),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_0))
  expect_equal(which(is.na(f_pred$.pred_time)), 1:2)

  expect_error(
    f_pred <- predict(f_fit, na_1_data_x, type = "time"),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_x))
  expect_equal(which(is.na(f_pred$.pred_time)), 2)

  expect_error(
    f_pred <- predict(f_fit, na_1_data_1, type = "time"),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_1))
  expect_equal(which(is.na(f_pred$.pred_time)), 2)

  expect_error(
    f_pred <- predict(f_fit, na_1_data_0, type = "time"),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_0))
  expect_true(is.na(f_pred$.pred_time))

})


# prediction: survival ----------------------------------------------------

test_that("survival predictions without strata", {
  # due to pec:
  skip_if_not_installed("Matrix", minimum_version = "1.4.2")

  cox_spec <- proportional_hazards() %>% set_engine("survival")
  exp_f_fit <- coxph(Surv(time, status) ~ age + sex, data = lung, x = TRUE)

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung), NA)
  expect_error(predict(f_fit, lung, type = "survival"),
               "When using 'type' values of 'survival' or 'hazard' are given")
  # Test at observed event times since we use the step function and pec does not
  f_pred <- predict(f_fit, lung, type = "survival", time = c(306, 455))
  exp_f_pred <- pec::predictSurvProb(exp_f_fit, lung, times = c(306, 455))

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    as.numeric(t(exp_f_pred))
  )

  # single observation
  expect_error(
    f_pred_1 <- predict(f_fit, lung[1,], type = "survival", time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred_1), 1)
})

test_that("survival predictions with strata", {
  cox_spec <- proportional_hazards() %>% set_engine("survival")

  set.seed(14)
  f_fit <- fit(cox_spec,
               Surv(stop, event) ~ rx + size + number + strata(enum),
               data = bladder)
  exp_f_fit <- coxph(Surv(stop, event) ~ rx + size + number + strata(enum),
                     data = bladder, x = TRUE)

  new_data_3 <- bladder[1:3, ]
  f_pred <- predict(f_fit, new_data = new_data_3,
                    type = "survival", time = c(10, 20))
  # reference value from pec::predictSurvProb()
  exp_f_pred <- structure(c(0.635719137259774, 0.933929695867806, 0.967237940301564,
                            0.534997251349036, 0.725922785669273, 0.904152770723571),
                          .Dim = 3:2)

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(2, 2))))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~ all(names(.x) == c(".time", ".pred_survival"))))
  )
  expect_equal(
    tidyr::unnest(f_pred, cols = c(.pred))$.pred_survival,
    as.numeric(t(exp_f_pred))
  )

  # single observation
  expect_error(
    f_pred_1 <- predict(f_fit, bladder[1, ], type = "survival", time = c(10, 20)),
    NA
  )
  expect_equal(nrow(f_pred_1), 1)

  # prediction without strata info should fail
  new_data_s <- new_data_3 %>% dplyr::select(-enum)
  expect_error(
    predict(f_fit, new_data = new_data_s, type = "survival", time = 20),
    "provide the strata"
  )
})

test_that("survival prediction with NA", {

  cox_spec <- proportional_hazards() %>% set_engine("survival")
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ age + strata(ph.ecog),
                 data = lung),
    NA
  )

  # survfit.coxph() is not type-stable,
  # thus test against single or multiple survival curves
  # lung$ph.ecog[14] is NA
  na_x_data_x <- lung[c(13:15, 14),]
  na_x_data_1 <- lung[c(13, 14, 14),]
  na_x_data_0 <- lung[c(14, 14),]
  na_1_data_x <- lung[13:15,]
  na_1_data_1 <- lung[13:14,]
  na_1_data_0 <- lung[14,]

  # survival probabilities
  expect_error(
    f_pred <- predict(f_fit, na_x_data_x, type = "survival", time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_x))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[4]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_x_data_1, type = "survival", time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_1))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[3]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_x_data_0, type = "survival", time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_x_data_0))
  expect_true(all(is.na(f_pred$.pred[[1]]$.pred_survival)))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_x, type = "survival", time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_x))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_1, type = "survival", time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_1))
  expect_true(all(is.na(f_pred$.pred[[2]]$.pred_survival)))

  expect_error(
    f_pred <- predict(f_fit, na_1_data_0, type = "survival", time = c(306, 455)),
    NA
  )
  expect_equal(nrow(f_pred), nrow(na_1_data_0))
  expect_true(all(is.na(f_pred$.pred[[1]]$.pred_survival)))

})

# prediction: linear_pred -------------------------------------------------

test_that("linear_pred predictions without strata", {
  cox_spec <- proportional_hazards() %>% set_engine("survival")
  exp_f_fit <- coxph(Surv(time, status) ~ age + sex, data = lung, x = TRUE)

  # formula method
  expect_error(f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung), NA)
  f_pred <- predict(f_fit, lung, type = "linear_pred")
  exp_f_pred <- -unname(predict(exp_f_fit, newdata = lung, reference = "zero"))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, exp_f_pred)
  expect_equal(nrow(f_pred), nrow(lung))

  # single observation
  expect_error(f_pred_1 <- predict(f_fit, lung[1,], type = "linear_pred"), NA)
  expect_equal(nrow(f_pred_1), 1)

  # don't flip the sign
  f_pred <- predict(f_fit, lung, type = "linear_pred", increasing = FALSE)
  exp_f_pred <- unname(predict(exp_f_fit, newdata = lung, reference = "zero"))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, exp_f_pred)
  expect_equal(nrow(f_pred), nrow(lung))
})

test_that("linear_pred predictions with strata", {
  cox_spec <- proportional_hazards() %>% set_engine("survival")
  exp_f_fit <- coxph(Surv(time, status) ~ age + sex + strata(inst),
                     data = lung, x = TRUE)

  # formula method
  expect_error(
    f_fit <- cox_spec %>%
                 fit(Surv(time, status) ~ age + sex + strata(inst), data = lung),
    NA
    )
  f_pred <- predict(f_fit, lung, type = "linear_pred")
  exp_f_pred <- -unname(predict(exp_f_fit, newdata = lung, reference = "zero"))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, exp_f_pred)
  expect_equal(nrow(f_pred), nrow(lung))

  # single observation
  expect_error(f_pred_1 <- predict(f_fit, lung[1,], type = "linear_pred"), NA)
  expect_equal(nrow(f_pred_1), 1)

  # don't flip the sign
  f_pred <- predict(f_fit, lung, type = "linear_pred", increasing = FALSE)
  exp_f_pred <- unname(predict(exp_f_fit, newdata = lung, reference = "zero"))

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, exp_f_pred)
  expect_equal(nrow(f_pred), nrow(lung))
})


# ------------------------------------------------------------------------------

test_that("predictions with strata and dot in formula", {
  lung2 <- lung[, c("time", "status", "age", "sex")]
  lung2$sex <- factor(lung2$sex)

  cox_spec <- proportional_hazards() %>% set_engine("survival")

  # formula method
  expect_error(
    f_fit <- fit(cox_spec, Surv(time, status) ~ . + strata(sex), data = lung2),
    NA
  )
  expect_error(
    f_fit_2 <- fit(cox_spec, Surv(time, status) ~ age + strata(sex), data = lung2),
    NA
  )
  expect_error({
    predict(f_fit, lung2, type = "time")
    predict(f_fit, lung2, type = "linear_pred")
    predict(f_fit, lung2, type = "survival", time = c(100, 300))
  },
  NA)
  expect_equal(
    predict(f_fit, lung2, type = "time"),
    predict(f_fit_2, lung2, type = "time")
  )
  expect_equal(
    predict(f_fit, lung2, type = "linear_pred"),
    predict(f_fit_2, lung2, type = "linear_pred")
  )
  expect_equal(
    predict(f_fit, lung2, type = "survival", time = c(100, 300)),
    predict(f_fit_2, lung2, type = "survival", time = c(100, 300))
  )
})

test_that("confidence intervals", {
  cox_spec <- proportional_hazards() %>% set_engine("survival")

  # without strata
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung, type = "survival", time = c(306, 455),
                    interval = "confidence")

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(lung))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(2, 4))))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~ all(names(.x) == c(".time", ".pred_survival",
                             ".pred_lower",
                             ".pred_upper"))))
  )

  # with strata
  set.seed(14)
  f_fit <- fit(cox_spec,
               Surv(stop, event) ~ rx + size + number + strata(enum),
               data = bladder)
  new_data_3 <- bladder[1:3, ]
  f_pred <- predict(f_fit, new_data = new_data_3,  type = "survival",
                   time = c(10, 20), interval = "confidence")

  expect_s3_class(f_pred, "tbl_df")
  expect_equal(names(f_pred), ".pred")
  expect_equal(nrow(f_pred), nrow(new_data_3))
  expect_true(
    all(purrr::map_lgl(f_pred$.pred,
                       ~ all(dim(.x) == c(2, 4))))
  )
  expect_true(
    all(
      purrr::map_lgl(
        f_pred$.pred,
        ~ all(names(.x) == c(".time", ".pred_survival", ".pred_lower",
                             ".pred_upper"))))
  )
})

test_that("get_missings_coxph() can identify missings without strata", {

  cox_spec <- proportional_hazards() %>% set_engine("survival")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  # lung$ph.ecog[14] is NA
  na_x_data_x <- lung[c(13:15, 14),]
  na_x_data_1 <- lung[c(13, 14, 14),]
  na_x_data_0 <- lung[c(14, 14),]
  na_1_data_x <- lung[13:15,]
  na_1_data_1 <- lung[13:14,]
  na_1_data_0 <- lung[14,]
  na_0_data_x <- lung[2:4,]

  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_x) %>% unclass() %>% unname(),
    c(2,4)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_1) %>% unclass() %>% unname(),
    c(2,3)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_0) %>% unclass() %>% unname(),
    c(1,2)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_x) %>% unclass() %>% unname(),
    c(2)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_1) %>% unclass() %>% unname(),
    2
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_0) %>% unclass() %>% unname(),
    1
  )
  expect_true(is.null(get_missings_coxph(f_fit$fit, na_0_data_x)))
})

test_that("get_missings_coxph() can identify missings with single strata term", {

  # missing in predictor
  cox_spec <- proportional_hazards() %>% set_engine("survival")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + ph.ecog + strata(sex),
               data = lung)

  # lung$ph.ecog[14] is NA
  na_x_data_x <- lung[c(13:15, 14),]
  na_x_data_1 <- lung[c(13, 14, 14),]
  na_x_data_0 <- lung[c(14, 14),]
  na_1_data_x <- lung[13:15,]
  na_1_data_1 <- lung[13:14,]
  na_1_data_0 <- lung[14,]
  na_0_data_x <- lung[2:4,]

  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_x) %>% unclass() %>% unname(),
    c(2,4)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_1) %>% unclass() %>% unname(),
    c(2,3)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_0) %>% unclass() %>% unname(),
    c(1,2)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_x) %>% unclass() %>% unname(),
    c(2)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_1) %>% unclass() %>% unname(),
    2
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_0) %>% unclass() %>% unname(),
    1
  )
  expect_true(is.null(get_missings_coxph(f_fit$fit, na_0_data_x)))

  # missing in strata
  cox_spec <- proportional_hazards() %>% set_engine("survival")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + strata(ph.ecog), data = lung)

  # lung$ph.ecog[14] is NA
  na_x_data_x <- lung[c(13:15, 14),]
  na_x_data_1 <- lung[c(13, 14, 14),]
  na_x_data_0 <- lung[c(14, 14),]
  na_1_data_x <- lung[13:15,]
  na_1_data_1 <- lung[13:14,]
  na_1_data_0 <- lung[14,]
  na_0_data_x <- lung[2:4,]

  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_x) %>% unclass() %>% unname(),
    c(2,4)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_1) %>% unclass() %>% unname(),
    c(2,3)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_0) %>% unclass() %>% unname(),
    c(1,2)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_x) %>% unclass() %>% unname(),
    c(2)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_1) %>% unclass() %>% unname(),
    2
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_0) %>% unclass() %>% unname(),
    1
  )
  expect_true(is.null(get_missings_coxph(f_fit$fit, na_0_data_x)))

})

test_that("get_missings_coxph() can identify missings with two strata terms", {

  # missing in strata
  cox_spec <- proportional_hazards() %>% set_engine("survival")
  f_fit <- fit(cox_spec, Surv(time, status) ~ age + strata(ph.ecog) + strata(sex),
               data = lung)

  # lung$ph.ecog[14] is NA
  na_x_data_x <- lung[c(13:15, 14),]
  na_x_data_1 <- lung[c(13, 14, 14),]
  na_x_data_0 <- lung[c(14, 14),]
  na_1_data_x <- lung[13:15,]
  na_1_data_1 <- lung[13:14,]
  na_1_data_0 <- lung[14,]
  na_0_data_x <- lung[2:4,]

  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_x) %>% unclass() %>% unname(),
    c(2,4)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_1) %>% unclass() %>% unname(),
    c(2,3)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_x_data_0) %>% unclass() %>% unname(),
    c(1,2)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_x) %>% unclass() %>% unname(),
    c(2)
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_1) %>% unclass() %>% unname(),
    2
  )
  expect_equal(
    get_missings_coxph(f_fit$fit, na_1_data_0) %>% unclass() %>% unname(),
    1
  )
  expect_true(is.null(get_missings_coxph(f_fit$fit, na_0_data_x)))
})
