library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("EventLogs can be created, add events, and access events", {

  # Creating an EventLog
  expect_no_error({ eventlog <- EventLog$new() })

  # Adding an Event
  event <- Event$new(data = list(type = "test_event"))
  expect_no_error({ eventlog$add(event) })

  # Returning a tibble of events
  expect_no_error({ el_tbl <- eventlog$as_tibble() })
  expect_equal(nrow(el_tbl), 1)
  expect_s3_class(el_tbl, "tbl")

  # Printing events - capture output instead of printing to terminal
  output <- capture.output(eventlog$print())
  expect_true(length(output) > 0)
  expect_true(any(grepl("test_event", output)))

})

test_that("EventLogs can be compacted", {

  eventlog <- EventLog$new()
  event <- Event$new(data = list(type = "test_event"))
  eventlog$add(event)
  
  expect_no_error({ eventlog$compact() })

})
