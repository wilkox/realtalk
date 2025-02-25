library(testthat)

test_that("a stream can open and close", {
  stream <- Stream$new(verbose = FALSE)
  expect_equal(stream$ready_state(), 1)
  stream$close()
  expect_equal(stream$ready_state(), 3)
})

test_that("stream events are logged", {

  stream <- Stream$new(verbose = FALSE)
  # Wait until the remote server sends a 'session.created' event
  then <- lubridate::now()
  while(stream$eventlog$as_tibble() |> nrow() == 0) {
    do_later_now()
    Sys.sleep(1)
    if (lubridate::interval(then, lubridate::now()) / lubridate::dseconds(1) > 30) {
      cli::cli_abort("Timeout waiting for session.created event")
    }
  }
  expect_equal(stream$eventlog$as_tibble()$type, "session.created")
  stream$close()

})

test_that("sending text and retrieving the stream transcript work", {
  stream <- Stream$new()
  expect_no_error(stream$send_text("What is the capital of France?"))
  expect_no_error(stream$send_text("What is the capital of Xinjiang?"))
  expect_no_error(invisible(capture.output(capture.output(stream$transcript()), type = "message")))
})
