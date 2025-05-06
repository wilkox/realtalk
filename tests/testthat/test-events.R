library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("Events can be created, added to EventLogs, return tibbles, and print", {

  # Creating an event
  expect_no_error({ event <- Event$new(data = list(type = "test_event")) })
  expect_equal(class(event), c("Event", "R6"))

  # Adding to an EventLog
  eventlog <- EventLog$new()
  expect_no_error(eventlog$add(event))

  # Returning a tibble
  expect_no_error({ event_tbl <- event$as_tibble() })
  expect_equal(nrow(event_tbl), 1)
  expect_s3_class(event_tbl, "tbl")

  # Printing event
  expect_no_error({ output <- capture.output(event$print()) })
  expect_true(length(output) > 0)
  expect_true(any(grepl("test_event", output)))

})
