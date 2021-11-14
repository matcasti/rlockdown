test_that("check if npm is installed", {
  expect_true(object = is_installed("npm"))
})

test_that("missing package specification shows error", {
  expect_error(object = is_installed())
})
