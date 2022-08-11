library(testthat)

test_that("primary arguments", {
  basic <- survival_reg()
  basic_flexsurv <- translate(basic %>% set_engine("flexsurv"))

  expect_equal(basic_flexsurv$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg())
               )
  )

  normal <- survival_reg(dist = "lnorm")
  normal_flexsurv <- translate(normal %>% set_engine("flexsurv"))
  expect_equal(normal_flexsurv$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 dist = rlang::quo("lnorm")
               )
  )

  dist_v <- survival_reg(dist = tune())
  dist_v_flexsurv <- translate(dist_v %>% set_engine("flexsurv"))
  expect_equal(dist_v_flexsurv$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 dist = rlang::new_quosure(hardhat::tune())
               )
  )
})

test_that("engine arguments", {
  fs_cl <- survival_reg()
  expect_equal(translate(fs_cl %>% set_engine("flexsurv", cl = .99))$method$fit$args,
               list(
                 formula = rlang::expr(missing_arg()),
                 data = rlang::expr(missing_arg()),
                 weights = rlang::expr(missing_arg()),
                 cl = rlang::quo(.99)
               )
  )
})


test_that("updating", {
  expr1     <- survival_reg() %>% set_engine("flexsurv", cl = tune())
  expr1_exp <- survival_reg(dist = "lnorm") %>% set_engine("flexsurv", cl = .99)
  expect_equal(update(expr1, dist = "lnorm", cl = 0.99), expr1_exp)

  param_tibb <- tibble::tibble(dist = "weibull")
  param_list <- as.list(param_tibb)

  expr1_updated <- update(expr1, param_tibb)
  expect_equal(expr1_updated$args$dist, "weibull")

  expr1_updated_lst <- update(expr1, param_list)
  expect_equal(expr1_updated_lst$args$dist, "weibull")
})

test_that("bad input", {
  expect_error(survival_reg(mode = ", classification"))
  expect_error(translate(survival_reg() %>% set_engine("wat")))
  expect_error(translate(survival_reg() %>% set_engine(NULL)))
})
