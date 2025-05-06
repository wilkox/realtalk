library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("streams create, start, and stop", {

  expect_no_error({ stream <- Stream$new() })

  # wait_for_response() should time out waiting for a response, as the stream
  # has not yet been initalised
  expect_error({ stream$wait_for_response(timeout = 1) })

  # The stream is not ready yet
  expect_false({ stream$is_ready() })

  # There should be no events in the stream
  expect_no_error({ conversation <- stream$conversation() })
  expect_equal(nrow(conversation), 0)

  expect_no_error({ stream$start_streaming() })
  expect_true({ stream$is_ready() })
  expect_equal(stream$eventlog$as_tibble()$type[1], "session.created")
  expect_no_error({ stream$stop_streaming() })
  expect_false({ stream$is_ready() })

})

# Set up a stream to use for further testing
stream <- Stream$new()
stream$start_streaming()

test_that("stream$log() works", {
  expect_no_error({ stream$log("This is a test log message") })
})

test_that("stream$eventlog holds an EventLog object", {
  expect_s3_class(stream$eventlog, "EventLog")
})

test_that("sending text messages from user and system roles works", {
  expect_no_error({ stream$send_text("This is a test message from the user", role = "user") })
  expect_no_error({ stream$send_text("This is a test message from the system", role = "system") })
  expect_no_error({ stream$send_text(
    "This is a test message from the system",
    role = "system",
    trigger_response = TRUE
  ) })
  expect_no_error({ stream$wait_for_response() })
})

test_that("set_status_message() works", {
  expect_no_error({ stream$set_status_message("This is a test status message") })
})

test_that("stream$conversation() returns a relevant tibble", {
  expect_no_error({ conversation <- stream$conversation() })
  expect_s3_class(conversation, "tbl")
  expect_true(nrow(conversation) > 0)
})

test_that("stream$transcript() works", {
  expect_no_error({ stream$transcript() })
})

# Shut down test stream
stream$stop_streaming()
