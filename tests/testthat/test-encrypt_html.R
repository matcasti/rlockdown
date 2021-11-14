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


test_that("generates encrypted html", {
  html_file <- "test.html"
  html_output <- "test_output.html"

  protect_html(html = html_file,
               password = 1234,
               output = html_output,
               title = "My title",
               instructions = "My instructions")

  expect_true(
    object = file.exists(html_output)
  )

  file.remove(html_output)
})

