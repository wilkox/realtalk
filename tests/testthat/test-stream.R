library(testthat)

test_that("streams create, start, and stop without error", {

  expect_no_error({ stream <- Stream$new() })
  expect_false({ stream$is_ready() })
  expect_no_error({ stream$start_streaming() })
  expect_true({ stream$is_ready() })
  expect_equal(stream$eventlog$as_tibble()$type[1], "session.created")
  expect_no_error({ stream$stop_streaming() })
  expect_false({ stream$is_ready() })
})
