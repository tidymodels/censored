library(testthat)

# registration ------------------------------------------------------------

test_that("engine is registered and translate() works", {
  engines <- parsnip::show_engines("survival_reg")
  censored_engines <- engines$engine[engines$mode == "censored regression"]
  expect_in("survival", censored_engines)

  spec <- survival_reg(dist = "weibull") |>
    set_engine("survival") |>
    set_mode("censored regression")

  translated <- translate(spec)
  expect_equal(translated$method$fit$func[["fun"]], "survreg")
  expect_equal(rlang::eval_tidy(translated$method$fit$args$dist), "weibull")
})

# fit ---------------------------------------------------------------------

test_that("model object", {
  set.seed(1234)
  exp_f_fit <- survival::survreg(
    Surv(time, status) ~ age + ph.ecog,
    data = lung,
    model = TRUE
  )

  mod_spec <- survival_reg() |>
    set_engine("survival") |>
    set_mode("censored regression")
  set.seed(1234)
  f_fit <- fit(mod_spec, Surv(time, status) ~ age + ph.ecog, data = lung)

  # remove `call` from comparison
  f_fit$fit$call <- NULL
  exp_f_fit$call <- NULL

  expect_equal(
    f_fit$fit,
    exp_f_fit,
    ignore_formula_env = TRUE
  )
})


# prediction: time --------------------------------------------------------

test_that("survival time prediction", {
  res <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung)

  exp_pred <- predict(res$fit, head(lung))
  exp_pred <- tibble::tibble(.pred_time = unname(exp_pred))
  expect_equal(exp_pred, predict(res, head(lung)))

  # single observation
  f_pred_1 <- predict(res, lung[1, ], type = "time")
  expect_identical(nrow(f_pred_1), 1L)
})

# prediction: survival ----------------------------------------------------

test_that("survival probability prediction", {
  rms_surv <- readRDS(test_path("data", "rms_surv.rds"))
  res <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung)

  # snapshot test this here instead of parsnip because
  # there are not engines in parnsip
  expect_snapshot(error = TRUE, {
    predict(res, head(lung), type = "survival")
  })

  exp_pred <- predict(
    res,
    head(lung),
    type = "survival",
    eval_time = c(0, 500, 1000)
  )
  exp_pred_vert <- exp_pred |>
    dplyr::mutate(.patient = dplyr::row_number()) |>
    tidyr::unnest(cols = .pred)

  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(
    names(exp_pred_vert),
    c(".eval_time", ".pred_survival", ".patient")
  )

  # using rms for expected results
  expect_equal(
    exp_pred$.pred[[1]]$.pred_survival,
    rms_surv,
    tolerance = 0.001
  )

  # single observation
  f_pred_1 <- predict(
    res,
    lung[1, ],
    type = "survival",
    eval_time = c(100, 500)
  )
  expect_identical(nrow(f_pred_1), 1L)
})

test_that("can predict for out-of-domain timepoints", {
  eval_time_obs_max_and_ood <- c(1022, 2000)
  obs_without_NA <- lung[2, ]

  mod <- survival_reg() |>
    set_mode("censored regression") |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ ., data = lung)

  expect_no_error(
    preds <- predict(
      mod,
      obs_without_NA,
      type = "survival",
      eval_time = eval_time_obs_max_and_ood
    )
  )
  expect_no_error(
    preds <- predict(
      mod,
      obs_without_NA,
      type = "hazard",
      eval_time = eval_time_obs_max_and_ood
    )
  )
})

# prediction: linear_pred -------------------------------------------------

test_that("linear predictor", {
  f_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung)
  f_pred <- predict(f_fit, lung[1:5, ], type = "linear_pred")

  exp_fit <- survreg(Surv(time, status) ~ age + sex, data = lung)
  exp_pred <- predict(exp_fit, lung[1:5, ], type = "linear")

  expect_s3_class(f_pred, "tbl_df")
  expect_true(all(names(f_pred) == ".pred_linear_pred"))
  expect_equal(f_pred$.pred_linear_pred, unname(exp_pred))
  expect_equal(nrow(f_pred), 5)

  # single observation
  f_pred_1 <- predict(f_fit, lung[1, ], type = "linear_pred")
  expect_identical(nrow(f_pred_1), 1L)
})


# prediction: quantile ----------------------------------------------------

test_that("prediction of survival time quantile", {
  res <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung)

  exp_quant <- predict(res$fit, head(lung), p = (2:4) / 5, type = "quantile")
  obs_quant <- predict(
    res,
    head(lung),
    type = "quantile",
    quantile_levels = (2:4) / 5
  )

  expect_s3_class(obs_quant, "tbl_df")
  expect_equal(names(obs_quant), ".pred_quantile")
  expect_equal(nrow(obs_quant), 6)
  expect_s3_class(
    obs_quant$.pred_quantile,
    c("quantile_pred", "vctrs_vctr", "list")
  )

  for (.row in 1:nrow(obs_quant)) {
    expect_equal(
      unclass(obs_quant$.pred_quantile[.row])[[1]] |>
        as.vector(),
      exp_quant[.row, ]
    )
  }

  # single observation
  f_pred_1 <- predict(res, lung[1, ], type = "quantile")
  expect_identical(nrow(f_pred_1), 1L)

  # single quantile
  f_pred_2 <- predict(
    res,
    lung[1:2, ],
    type = "quantile",
    quantile_levels = 0.5
  )
  expect_identical(nrow(f_pred_2), 2L)
})


# prediction: hazard ------------------------------------------------------

test_that("survival hazard prediction", {
  rms_haz <- readRDS(test_path("data", "rms_haz.rds"))
  res <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung)

  # snapshot test this here instead of parsnip because
  # there are not engines in parnsip
  expect_snapshot(error = TRUE, {
    predict(res, head(lung), type = "hazard")
  })

  exp_pred <- predict(
    res,
    head(lung),
    type = "hazard",
    eval_time = c(0, 500, 1000)
  )
  exp_pred_vert <- exp_pred |>
    dplyr::mutate(.patient = dplyr::row_number()) |>
    tidyr::unnest(cols = .pred)

  expect_true(all(names(exp_pred) == ".pred"))
  expect_equal(
    names(exp_pred_vert),
    c(".eval_time", ".pred_hazard", ".patient")
  )

  # using rms for expected results
  expect_equal(
    exp_pred$.pred[[1]]$.pred_hazard[-1],
    rms_haz[-1],
    tolerance = 0.001
  )

  # single observation
  f_pred_1 <- predict(res, lung[1, ], type = "hazard", eval_time = c(100, 500))
  expect_identical(nrow(f_pred_1), 1L)
})

# prediction with strata --------------------------------------------------

test_that("survival probabilities with strata match a hand computation", {
  f_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + strata(sex), data = lung)
  engine_fit <- extract_fit_engine(f_fit)

  # strata give one scale per stratum, exercising the multi-scale branch
  expect_named(engine_fit$scale, c("sex=1", "sex=2"))

  new_data <- lung[c(1, 7, 20), ]
  # both strata represented
  expect_equal(new_data$sex, c(1, 2, 1))

  eval_time <- c(100, 500)
  pred <- predict(f_fit, new_data, type = "survival", eval_time = eval_time)
  lp <- predict(engine_fit, new_data, type = "lp")

  # match each row to its stratum's scale by hand
  expect_equal(
    pred$.pred[[1]]$.pred_survival,
    1 -
      survival::psurvreg(
        eval_time,
        lp[1],
        distribution = engine_fit$dist,
        scale = engine_fit$scale[["sex=1"]]
      )
  )
  expect_equal(
    pred$.pred[[2]]$.pred_survival,
    1 -
      survival::psurvreg(
        eval_time,
        lp[2],
        distribution = engine_fit$dist,
        scale = engine_fit$scale[["sex=2"]]
      )
  )
  expect_equal(
    pred$.pred[[3]]$.pred_survival,
    1 -
      survival::psurvreg(
        eval_time,
        lp[3],
        distribution = engine_fit$dist,
        scale = engine_fit$scale[["sex=1"]]
      )
  )
})

test_that("NA in the strata variable gives NA predictions with rows preserved", {
  f_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + strata(sex), data = lung)

  eval_time <- c(100, 500)
  na_pred <- rep(NA_real_, length(eval_time))

  # A missing strata value must predict NA regardless of which strata the
  # other rows belong to (see https://github.com/tidymodels/censored/issues/383).
  na_and_sex1 <- lung[c(1, 2), ]
  na_and_sex1$sex[1] <- NA
  na_and_sex2 <- lung[c(1, 7), ]
  na_and_sex2$sex[1] <- NA

  pred_1 <- predict(
    f_fit,
    na_and_sex1,
    type = "survival",
    eval_time = eval_time
  )
  pred_2 <- predict(
    f_fit,
    na_and_sex2,
    type = "survival",
    eval_time = eval_time
  )

  expect_equal(nrow(pred_1), 2)
  expect_equal(nrow(pred_2), 2)
  expect_equal(pred_1$.pred[[1]]$.pred_survival, na_pred)
  expect_equal(pred_2$.pred[[1]]$.pred_survival, na_pred)
})

test_that("hazard predictions with strata match a hand computation", {
  f_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + strata(sex), data = lung)
  engine_fit <- extract_fit_engine(f_fit)

  new_data <- lung[c(1, 7, 20), ]
  eval_time <- c(100, 500)
  pred <- predict(f_fit, new_data, type = "hazard", eval_time = eval_time)
  lp <- predict(engine_fit, new_data, type = "lp")

  hand_hazard <- function(lp_i, scale_i) {
    survival::dsurvreg(
      eval_time,
      lp_i,
      scale_i,
      distribution = engine_fit$dist
    ) /
      (1 -
        survival::psurvreg(
          eval_time,
          lp_i,
          distribution = engine_fit$dist,
          scale = scale_i
        ))
  }

  expect_equal(
    pred$.pred[[1]]$.pred_hazard,
    hand_hazard(lp[1], engine_fit$scale[["sex=1"]])
  )
  expect_equal(
    pred$.pred[[2]]$.pred_hazard,
    hand_hazard(lp[2], engine_fit$scale[["sex=2"]])
  )
  expect_equal(
    pred$.pred[[3]]$.pred_hazard,
    hand_hazard(lp[3], engine_fit$scale[["sex=1"]])
  )
})

test_that("survival_prob_survreg() and hazard_survreg() work on stratified fits", {
  f_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + strata(sex), data = lung)

  new_data <- lung[c(1, 7, 20), ]
  eval_time <- c(100, 500)

  expect_equal(
    survival_prob_survreg(f_fit, new_data, eval_time = eval_time),
    predict(f_fit, new_data, type = "survival", eval_time = eval_time)
  )
  expect_equal(
    hazard_survreg(f_fit, new_data, eval_time = eval_time),
    predict(f_fit, new_data, type = "hazard", eval_time = eval_time)
  )
})

# missing data -------------------------------------------------------------

test_that("missing predictors don't drop rows", {
  f_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + ph.ecog, data = lung)

  new_data <- lung[13:15, ] # row 2 has ph.ecog = NA
  eval_time <- c(100, 300)
  na_pred <- rep(NA_real_, length(eval_time))

  f_pred_time <- predict(f_fit, new_data, type = "time")
  expect_equal(nrow(f_pred_time), 3)
  expect_equal(which(is.na(f_pred_time$.pred_time)), 2L)
  expect_all_true(is.finite(f_pred_time$.pred_time[c(1, 3)]))

  f_pred_lp <- predict(f_fit, new_data, type = "linear_pred")
  expect_equal(nrow(f_pred_lp), 3)
  expect_equal(which(is.na(f_pred_lp$.pred_linear_pred)), 2L)
  expect_all_true(is.finite(f_pred_lp$.pred_linear_pred[c(1, 3)]))

  f_pred_surv <- predict(
    f_fit,
    new_data,
    type = "survival",
    eval_time = eval_time
  )
  expect_equal(nrow(f_pred_surv), 3)
  expect_equal(f_pred_surv$.pred[[2]]$.eval_time, eval_time)
  expect_equal(f_pred_surv$.pred[[2]]$.pred_survival, na_pred)
  expect_all_true(is.finite(f_pred_surv$.pred[[1]]$.pred_survival))
  expect_all_true(is.finite(f_pred_surv$.pred[[3]]$.pred_survival))

  f_pred_hazard <- predict(
    f_fit,
    new_data,
    type = "hazard",
    eval_time = eval_time
  )
  expect_equal(nrow(f_pred_hazard), 3)
  expect_equal(f_pred_hazard$.pred[[2]]$.pred_hazard, na_pred)
  expect_all_true(is.finite(f_pred_hazard$.pred[[1]]$.pred_hazard))
  expect_all_true(is.finite(f_pred_hazard$.pred[[3]]$.pred_hazard))

  f_pred_quantile <- predict(
    f_fit,
    new_data,
    type = "quantile",
    quantile_levels = c(0.2, 0.5, 0.8)
  )
  expect_equal(nrow(f_pred_quantile), 3)
  expect_equal(
    as.vector(unclass(f_pred_quantile$.pred_quantile[2])[[1]]),
    rep(NA_real_, 3)
  )
  expect_all_true(
    is.finite(as.vector(unclass(f_pred_quantile$.pred_quantile[1])[[1]]))
  )
  expect_all_true(
    is.finite(as.vector(unclass(f_pred_quantile$.pred_quantile[3])[[1]]))
  )
})

# fit via matrix interface ------------------------------------------------

test_that("`fix_xy()` works", {
  lung_x <- as.matrix(lung[, c("age", "ph.ecog")])
  lung_y <- Surv(lung$time, lung$status)
  lung_pred <- lung[1:5, ]

  spec <- survival_reg() |>
    set_engine("survival") |>
    set_mode("censored regression")
  f_fit <- fit(spec, Surv(time, status) ~ age + ph.ecog, data = lung)
  xy_fit <- fit_xy(spec, x = lung_x, y = lung_y)

  elements_to_ignore <- c("call", "terms", "model")
  f_ignore <- which(names(f_fit$fit) %in% elements_to_ignore)
  xy_ignore <- which(names(xy_fit$fit) %in% elements_to_ignore)
  expect_equal(f_fit$fit[-f_ignore], xy_fit$fit[-xy_ignore])

  f_pred_time <- predict(f_fit, new_data = lung_pred, type = "time")
  xy_pred_time <- predict(xy_fit, new_data = lung_pred, type = "time")
  expect_equal(f_pred_time, xy_pred_time)

  f_pred_survival <- predict(
    f_fit,
    new_data = lung_pred,
    type = "survival",
    eval_time = c(100, 200)
  )
  xy_pred_survival <- predict(
    xy_fit,
    new_data = lung_pred,
    type = "survival",
    eval_time = c(100, 200)
  )
  expect_equal(f_pred_survival, xy_pred_survival)

  f_pred_lp <- predict(f_fit, new_data = lung_pred, type = "linear_pred")
  xy_pred_lp <- predict(xy_fit, new_data = lung_pred, type = "linear_pred")
  expect_equal(f_pred_lp, xy_pred_lp)

  f_pred_quantile <- predict(
    f_fit,
    new_data = lung_pred,
    type = "quantile",
    quantile_levels = c(0.2, 0.8)
  )
  xy_pred_quantile <- predict(
    xy_fit,
    new_data = lung_pred,
    type = "quantile",
    quantile_levels = c(0.2, 0.8)
  )
  expect_equal(f_pred_quantile, xy_pred_quantile)

  f_pred_hazard <- predict(
    f_fit,
    new_data = lung_pred,
    type = "hazard",
    eval_time = c(100, 200)
  )
  xy_pred_hazard <- predict(
    xy_fit,
    new_data = lung_pred,
    type = "hazard",
    eval_time = c(100, 200)
  )
  expect_equal(f_pred_hazard, xy_pred_hazard)
})


# deprecation of time arg -------------------------------------------------

test_that("deprecation of `time` arg for type 'survival'", {
  f_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung)
  exp_pred <- predict(
    f_fit,
    head(lung),
    type = "survival",
    eval_time = c(0, 500, 1000)
  )

  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(
    predict(f_fit, head(lung), type = "survival", time = c(0, 500, 1000)),
    class = "defunctError"
  )

  rlang::local_options(lifecycle_verbosity = "quiet")
  pred <- predict(f_fit, head(lung), type = "survival", time = c(0, 500, 1000))
  expect_identical(pred, exp_pred)
})

test_that("deprecation of `time` arg for type 'hazard'", {
  f_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung)
  exp_pred <- predict(
    f_fit,
    head(lung),
    type = "hazard",
    eval_time = c(0, 500, 1000)
  )

  rlang::local_options(lifecycle_verbosity = "error")
  expect_error(
    predict(f_fit, head(lung), type = "hazard", time = c(0, 500, 1000)),
    class = "defunctError"
  )

  rlang::local_options(lifecycle_verbosity = "quiet")
  pred <- predict(f_fit, head(lung), type = "hazard", time = c(0, 500, 1000))
  expect_identical(pred, exp_pred)
})

test_that("survival_prob_survreg() warns about deprecated `time` argument", {
  mod <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age, data = lung)
  new_data <- lung[1:2, ]

  expect_snapshot(
    pred_deprecated <- survival_prob_survreg(
      mod,
      new_data = new_data,
      time = 100
    )
  )
  expect_equal(
    pred_deprecated,
    survival_prob_survreg(mod, new_data = new_data, eval_time = 100)
  )
})

# input checks ------------------------------------------------------------

test_that("survival_prob_survreg() errors informatively on bad input", {
  raw_fit <- survival::survreg(Surv(time, status) ~ age, data = lung)
  wrong_engine <- structure(
    list(fit = structure(list(), class = "coxph")),
    class = "model_fit"
  )

  expect_snapshot(
    error = TRUE,
    survival_prob_survreg(raw_fit, new_data = lung[1:3, ], eval_time = 100)
  )
  expect_snapshot(
    error = TRUE,
    survival_prob_survreg(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
  )
})

test_that("hazard_survreg() errors informatively on bad input", {
  raw_fit <- survival::survreg(Surv(time, status) ~ age, data = lung)
  wrong_engine <- structure(
    list(fit = structure(list(), class = "coxph")),
    class = "model_fit"
  )

  expect_snapshot(
    error = TRUE,
    hazard_survreg(raw_fit, new_data = lung[1:3, ], eval_time = 100)
  )
  expect_snapshot(
    error = TRUE,
    hazard_survreg(wrong_engine, new_data = lung[1:3, ], eval_time = 100)
  )
})

test_that("survival_prob_survreg() fails gracefully for eval_time values it can't handle", {
  mod <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age, data = lung)

  expect_snapshot(
    error = TRUE,
    survival_prob_survreg(mod, new_data = lung[1:2, ], eval_time = c(100, -Inf))
  )
  expect_snapshot(
    error = TRUE,
    survival_prob_survreg(mod, new_data = lung[1:2, ], eval_time = c(100, -50))
  )
})

test_that("survival_prob_survreg() accepts eval_time values that it can handle", {
  mod <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age, data = lung)
  new_data <- lung[1:2, ]

  expect_no_error(
    survival_prob_survreg(mod, new_data = new_data, eval_time = numeric(0))
  )
  expect_no_error(
    survival_prob_survreg(mod, new_data = new_data, eval_time = c(100, NA))
  )
  expect_no_error(
    survival_prob_survreg(mod, new_data = new_data, eval_time = c(100, Inf))
  )
  expect_no_error(
    survival_prob_survreg(
      mod,
      new_data = new_data,
      eval_time = c(100, 100, 200)
    )
  )
})

test_that("hazard_survreg() fails gracefully for eval_time values it can't handle", {
  mod <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age, data = lung)

  expect_snapshot(
    error = TRUE,
    hazard_survreg(mod, new_data = lung[1:2, ], eval_time = c(100, -Inf))
  )
  expect_snapshot(
    error = TRUE,
    hazard_survreg(mod, new_data = lung[1:2, ], eval_time = c(100, -50))
  )
})

test_that("hazard_survreg() accepts eval_time values that it can handle", {
  mod <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age, data = lung)
  new_data <- lung[1:2, ]

  expect_no_error(
    hazard_survreg(mod, new_data = new_data, eval_time = numeric(0))
  )
  expect_no_error(
    hazard_survreg(mod, new_data = new_data, eval_time = c(100, NA))
  )
  expect_no_error(
    hazard_survreg(mod, new_data = new_data, eval_time = c(100, Inf))
  )
  expect_no_error(
    hazard_survreg(mod, new_data = new_data, eval_time = c(100, 100, 200))
  )
})

# tuning ------------------------------------------------------------------

test_that("tuning parameters are inherited", {
  spec <- survival_reg(dist = tune()) |>
    set_engine("survival") |>
    set_mode("censored regression")

  params <- hardhat::extract_parameter_set_dials(spec)
  expect_setequal(params$name, "dist")
})

# case weights ------------------------------------------------------------

test_that("can handle case weights", {
  # survreg requires strictly positive weights
  set.seed(1)
  wts <- importance_weights(runif(nrow(lung)))

  wt_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung, case_weights = wts)
  unwt_fit <- survival_reg() |>
    set_engine("survival") |>
    fit(Surv(time, status) ~ age + sex, data = lung)

  expect_equal(unname(wt_fit$fit$weights), as.numeric(wts))

  # weighted predictions differ from the unweighted fit for every type
  expect_unequal(
    predict(wt_fit, lung, type = "time"),
    predict(unwt_fit, lung, type = "time")
  )
  expect_unequal(
    predict(wt_fit, lung, type = "survival", eval_time = c(100, 500)),
    predict(unwt_fit, lung, type = "survival", eval_time = c(100, 500))
  )
  expect_unequal(
    predict(wt_fit, lung, type = "hazard", eval_time = c(100, 500)),
    predict(unwt_fit, lung, type = "hazard", eval_time = c(100, 500))
  )
  expect_unequal(
    predict(wt_fit, lung, type = "linear_pred"),
    predict(unwt_fit, lung, type = "linear_pred")
  )
  expect_unequal(
    predict(wt_fit, lung, type = "quantile"),
    predict(unwt_fit, lung, type = "quantile")
  )
})
