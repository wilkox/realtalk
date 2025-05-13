library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("ManagedProcess can be created", {
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  
  # Create a real background process with callr
  process <- callr::r_bg(function() {
    Sys.sleep(5) # Long-running process
    "test complete"
  })
  
  expect_no_error({
    managed_process <- ManagedProcess$new(
      process = process,
      name = "test_process",
      logger = logger
    )
  })
  
  # Test fields initialization
  expect_equal(class(managed_process)[1], "ManagedProcess")
  expect_true(managed_process$get_status()$status == "running")
  
  # Clean up
  process$kill()
})

test_that("ManagedProcess can check process status", {
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  
  # Create a real background process that exits quickly
  live_process <- callr::r_bg(function() {
    Sys.sleep(5) # Long-running process
    return(0)
  })
  
  managed_process <- ManagedProcess$new(
    process = live_process,
    name = "test_process",
    logger = logger
  )
  
  # Test check method returns status information
  expect_no_error({ status <- managed_process$check() })
  expect_true(status$status == "running")
  
  # Create a process that exits quickly
  dead_process <- callr::r_bg(function() {
    return(1) # Exit with status 1
  })
  
  # Wait for process to finish
  Sys.sleep(1)
  
  managed_dead_process <- ManagedProcess$new(
    process = dead_process,
    name = "test_dead_process",
    logger = logger
  )
  
  # Test check method detects dead process
  expect_no_error({ status <- managed_dead_process$check() })
  expect_true(status$status == "dead")
  
  # Clean up
  live_process$kill()
})

test_that("ManagedProcess has output retrieval methods", {
  # Skip on CI where process management can be less reliable
  skip_on_ci()
  
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  
  # Create a process
  process <- callr::r_bg(function() {
    # This function will run in a separate process
    # We'll test the structure without relying on actual output
    Sys.sleep(5)
    return(0)
  })
  
  managed_process <- ManagedProcess$new(
    process = process,
    name = "test_process",
    logger = logger
  )
  
  # Test the structure of the output methods
  expect_no_error({ stdout <- managed_process$get_stdout() })
  expect_type(stdout, "character")
  
  expect_no_error({ stderr <- managed_process$get_stderr() })
  expect_type(stderr, "character")
  
  # Test combined output
  expect_no_error({ combined <- managed_process$get_combined_output() })
  expect_true("stdout" %in% names(combined))
  expect_true("stderr" %in% names(combined))
  expect_true("status" %in% names(combined))
  
  # Clean up
  process$kill()
})

test_that("ManagedProcess can restart a process", {
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  
  # Create a process that exits quickly
  process <- callr::r_bg(function() {
    return(1) # Exit with status 1
  })
  
  # Wait for process to finish
  Sys.sleep(1)
  
  managed_process <- ManagedProcess$new(
    process = process,
    name = "test_dead_process",
    logger = logger
  )
  
  # Check that it's dead
  status <- managed_process$check()
  expect_true(status$status == "dead")
  
  # Create restart function that returns a new process
  restart_function <- function() {
    return(callr::r_bg(function() {
      Sys.sleep(5) # Long-running process
      return(0)
    }))
  }
  
  # Test restart method
  expect_no_error({ result <- managed_process$restart(restart_function) })
  expect_true(result)
  
  # Verify process was restarted
  expect_no_error({ status <- managed_process$check() })
  expect_true(status$status == "running")
  expect_equal(managed_process$get_status()$restarts, 1)
  
  # Clean up
  managed_process$get_status()$process$kill()
})

test_that("ManagedProcess initializes with buffer settings", {
  # Skip on CI where process management can be less reliable
  skip_on_ci()
  
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  
  # Create a process 
  process <- callr::r_bg(function() {
    Sys.sleep(10)
    return(0)
  })
  
  # Test initialization with a specific buffer size
  expect_no_error({
    managed_process <- ManagedProcess$new(
      process = process,
      name = "test_process",
      logger = logger,
      max_buffer_size = 20 # Small buffer for testing
    )
  })
  
  # Just verify that output buffers are initialized
  expect_type(managed_process$get_stdout(), "character")
  expect_type(managed_process$get_stderr(), "character")
  
  # Clean up
  process$kill()
})