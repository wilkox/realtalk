library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("streams create, start, and stop", {
  testthat::skip_on_ci()

  expect_no_error({ stream <- Stream$new() })

  # wait_for_response() should time out waiting for a response, as the stream
  # has not yet been initalised
  expect_error({ stream$wait_for_response(timeout = 1) })

  # The stream is not ready yet
  expect_false({ stream$is_ready() })

  # There should be no events in the stream
  expect_no_error({ conversation <- stream$conversation() })
  expect_equal(nrow(conversation), 0)

  # Start streaming
  expect_no_error({ stream$start_streaming() })
  expect_true({ stream$is_ready() })
  expect_equal(stream$eventlog$as_tibble()$type[1], "session.created")

  # Stop streaming
  expect_no_error({ stream$stop_streaming() })
  expect_false({ stream$is_ready() })

})

test_that("stream methods work correctly", {
  testthat::skip_on_ci()
  
  # Setup a stream for all tests
  stream <- Stream$new()
  stream$start_streaming()
  
  # Test logging
  expect_no_error({ stream$log("This is a test log message") })
  
  # Test eventlog
  expect_s3_class(stream$eventlog, "EventLog")
  
  # Test sending text messages
  expect_no_error({ stream$send_text("This is a test message from the user", role = "user") })
  expect_no_error({ stream$send_text("This is a test message from the system", role = "system") })
  expect_no_error({ 
    stream$send_text(
      "This is a test message from the system",
      role = "system",
      trigger_response = TRUE
    )
  })
  expect_no_error({ stream$wait_for_response() })
  
  # Test status message
  expect_no_error({ stream$set_status_message("This is a test status message") })
  
  # Test conversation
  expect_no_error({ conversation <- stream$conversation() })
  expect_s3_class(conversation, "tbl")
  expect_true(nrow(conversation) > 0)
  
  # Test transcript
  expect_no_error({ stream$transcript() })
  
  # Clean up
  stream$stop_streaming()
})
