library(testthat)

test_that("start_stream() establishes and closes a connection", {
  
  ws <- start_stream(verbose = FALSE)
  Sys.sleep(5)
  expect_true(inherits(ws, "WebSocket"))
  ready_state <- ws$readyState() |> as.vector()
  expect_equal(ready_state, 1)
  
  ws$close()
  Sys.sleep(5)
  ready_state <- ws$readyState() |> as.vector()
  expect_equal(ready_state, 3)
})
