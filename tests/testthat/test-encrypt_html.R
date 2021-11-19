test_that("missing html", {
  html_file <- "test.html"

  expect_error(
    object = protect_html(password = 1234)
  )
})


test_that("missing password", {
  html_file <- "test.html"

  expect_error(
    object = protect_html(html = html_file)
  )
})

