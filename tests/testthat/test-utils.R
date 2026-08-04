test_that("get_strata() extracts a single strata term", {
  strata <- get_strata(Surv(time, status) ~ age + strata(sex), lung[1:7, ])

  expect_equal(levels(strata), c("sex=1", "sex=2"))
  expect_equal(as.character(strata), paste0("sex=", lung$sex[1:7]))
})

test_that("get_strata() combines multiple strata terms to match survreg's scales", {
  form <- Surv(time, status) ~ age + strata(sex) + strata(ph.ecog)
  engine_fit <- survival::survreg(form, data = lung)

  strata <- get_strata(form, lung)

  # get_survreg_scale() looks the scales up by these labels
  expect_equal(levels(strata), names(engine_fit$scale))
})

test_that("get_strata() combines the variables of a single strata term", {
  form <- Surv(time, status) ~ age + strata(sex, ph.ecog)

  strata <- get_strata(form, lung[1:7, ])

  expect_equal(
    as.character(strata)[1:3],
    c("sex=1, ph.ecog=1", "sex=1, ph.ecog=0", "sex=1, ph.ecog=0")
  )
})

test_that("get_strata() passes on or omits missing strata values", {
  form <- Surv(time, status) ~ age + strata(sex)
  new_data <- lung[1:7, ]
  new_data$sex[2] <- NA

  passed <- get_strata(form, new_data, na.action = stats::na.pass)
  omitted <- get_strata(form, new_data)

  expect_length(passed, 7)
  expect_equal(which(is.na(passed)), 2L)
  expect_length(omitted, 6)
})

test_that("get_strata() works with the terms of a fit and new data without the outcome", {
  lung_fct <- lung |>
    dplyr::mutate(sex = factor(sex, labels = c("male", "female")))
  engine_fit <- survival::survreg(
    Surv(time, status) ~ age + strata(sex),
    data = lung_fct
  )

  # A factor strata variable is labelled by its levels alone, and this
  # `new_data` has neither the outcome columns nor the "male" stratum.
  # survreg() uses `xlevels` only for the predictors.
  new_data <- lung_fct[lung_fct$sex == "female", c("age", "sex")][1:3, ]

  strata <- get_strata(engine_fit$terms, new_data, na.action = stats::na.pass)

  expect_equal(as.character(strata), rep("female", 3))
})
