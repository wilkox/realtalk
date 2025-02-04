library(testthat)

test_that("streams open and close", {
  
  # Create a new stream
  stream <- Stream$new(verbose = FALSE)
  expect_equal(stream$ready_state(), 1)
  
  stream$close()
  expect_equal(stream$ready_state(), 3)
})
