library(testthat)

test_that("events and eventlogs can be created", {

  event <- Event$new(data = list(type = "testevent"))
  expect_equal(class(event), c("Event", "R6"))
  eventlog <- EventLog$new()
  expect_equal(class(eventlog), c("EventLog", "R6"))
  expect_no_error(eventlog$add(event))
  expect_equal(eventlog$as_tibble() |> nrow(), 1)

})
