library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("format_datetime() returns a formatted timestamp", {
  # Test that format_datetime returns a character string
  expect_type(format_datetime(), "character")
  
  # Test that the format is as expected (YYYY-MM-DD HH:MM:SS TZ)
  expect_match(format_datetime(), "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} [A-Z]+$")
})

test_that("do_later_now() executes without error", {
  # Basic test that the function runs without error
  expect_no_error(do_later_now())
  
  # Test that it actually runs 'later' functions
  test_result <- NULL
  later::later(function() { test_result <<- TRUE })
  do_later_now()
  expect_true(test_result)
})

test_that("openai_api_key() returns a non-empty API key", {
  # In a real environment with actual API key set
  api_key <- openai_api_key(verbose = FALSE)
  expect_type(api_key, "character")
  expect_true(nchar(api_key) > 0)
  
  # Test that it sets and retrieves from options
  expect_equal(getOption("realtalk.OPENAI_API_KEY"), api_key)
  
  # Test that subsequent calls use the option
  cached_key <- "test_cached_key_for_testing_only"
  old_key <- getOption("realtalk.OPENAI_API_KEY")
  withr::with_options(
    list(realtalk.OPENAI_API_KEY = cached_key),
    {
      expect_equal(openai_api_key(verbose = FALSE), cached_key)
    }
  )
  
  # Reset the option to the original value
  options(realtalk.OPENAI_API_KEY = old_key)
  
  # Test that it errors when the environment variable is not set
  # But only if we can safely unset it for testing
  if (!identical(Sys.getenv("CI"), "true")) {
    withr::with_envvar(
      c("OPENAI_API_KEY" = NA), 
      {
        expect_error(openai_api_key(verbose = FALSE), "Cannot find environmental variable")
      }
    )
  }
})