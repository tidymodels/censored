# this tests primary and engine args here rather than in parsnip because
# it requires the engines to be loaded

test_that("arguments", {
  basic <- proportional_hazards()
  penalty <- proportional_hazards(penalty = 1)
  mixture <- proportional_hazards(penalty = 1, mixture = 0.128)
  mixture_v <- proportional_hazards(penalty = 1, mixture = tune())

  expect_snapshot(translate_args(basic))
  expect_snapshot(translate_args(penalty %>% set_engine("glmnet")))
  expect_snapshot(translate_args(penalty %>% set_engine("glmnet", path_values = 4:2)))
  expect_snapshot(translate_args(mixture %>% set_engine("glmnet")))
  expect_snapshot(translate_args(mixture_v %>% set_engine("glmnet")))
})

test_that("check_args() works", {
  skip_if_not_installed("parsnip", "1.2.1.9001")

  # Here for completeness, no checking is done
  expect_true(TRUE)
})
