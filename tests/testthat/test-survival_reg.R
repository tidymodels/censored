# this tests primary and engine args here rather than in parsnip because
# it requires the engines to be loaded

test_that("arguments", {
  basic <- survival_reg()
  normal <- survival_reg(dist = "lnorm")
  dist_v <- survival_reg(dist = tune())

  expect_snapshot(translate_args(basic |> set_engine("flexsurv")))
  expect_snapshot(translate_args(basic |> set_engine("flexsurv", cl = .99)))
  expect_snapshot(translate_args(normal |> set_engine("flexsurv")))
  expect_snapshot(translate_args(dist_v |> set_engine("flexsurv")))
})

test_that("check_args() works", {
  skip_if_not_installed("parsnip", "1.2.1.9001")

  # Here for completeness, no checking is done
  expect_true(TRUE)
})
