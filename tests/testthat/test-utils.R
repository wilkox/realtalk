library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("format_datetime() returns a formatted timestamp", {
  # Test that format_datetime returns a character string
  expect_type(format_datetime(), "character")
  
  # Test that the format is as expected (YYYY-MM-DD HH:MM:SS TZ)
  expect_match(format_datetime(), "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} [A-Z]+$")
})

test_that("do_later_now() executes without error", {

  expect_no_error(do_later_now())
  
})

test_that("openai_api_key() works", {
  testthat::skip_on_ci()
  
  expect_no_error({ key <- openai_api_key() })
  expect_type(key, "character")
})
