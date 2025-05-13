library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("Stream initialization creates Logger and ProcessManager", {
  testthat::skip_on_ci()
  
  expect_no_error({
    stream <- Stream$new()
  })
  
  # Check that logger and process_manager fields are created
  expect_true(!is.null(stream$logger))
  expect_true(!is.null(stream$process_manager))
  expect_equal(class(stream$logger)[1], "Logger")
  expect_equal(class(stream$process_manager)[1], "ProcessManager")
})

test_that("Stream can report error history", {
  testthat::skip_on_ci()
  
  stream <- Stream$new()
  
  # Test error_history method for all processes
  expect_no_error({ 
    error_info <- stream$error_history() 
  })
  
  # Should initially return an empty list or similar structure
  expect_true(is.list(error_info))
  
  # We shouldn't try to get error info for a non-existent process
  # as that would trigger an error
})

test_that("Stream can report process status", {
  testthat::skip_on_ci()
  
  stream <- Stream$new()
  
  # Test process_status method before starting
  expect_no_error({
    status <- stream$process_status()
  })
  
  # Should return a list or similar structure of process statuses
  expect_true(is.list(status))
  
  # Start streaming 
  stream$start_streaming()
  
  # Now should have active processes
  status <- stream$process_status()
  expect_true(length(status) > 0)
  
  # At least one process should be running
  running_count <- sum(sapply(status, function(s) s$status == "running"))
  expect_true(running_count > 0)
  
  # Clean up
  stream$stop_streaming()
})

test_that("Stream's main loop includes health checks", {
  testthat::skip_on_ci()
  
  stream <- Stream$new()
  
  # Start streaming, this should register processes with the process manager
  stream$start_streaming()
  
  # Check that processes are being monitored
  expect_true(length(stream$process_status()) > 0)
  
  # Clean up
  stream$stop_streaming()
})

test_that("Stream handles process failure gracefully", {
  testthat::skip_on_ci()
  
  # Create a stream
  stream <- Stream$new()
  
  # Start streaming
  stream$start_streaming()
  
  # Check that we have a main_loop process running
  initial_status <- stream$process_status()
  expect_true("main_loop" %in% names(initial_status))
  expect_equal(initial_status$main_loop$status, "running")
  
  # Register a deliberate "dummy" process that will terminate quickly
  # This simulates a failing background process
  dummy_process <- callr::r_bg(function() {
    # Exit immediately with error status 1
    quit(status = 1)
  })
  
  # Wait briefly for the process to terminate
  Sys.sleep(0.5)
  
  # Register the terminated process with the process_manager
  stream$process_manager$register_process(dummy_process, "test_dummy")
  
  # Query the process status and check that the failure is detected
  status <- stream$process_status()
  expect_true("test_dummy" %in% names(status))
  expect_equal(status$test_dummy$status, "dead")
  
  # Check that we can get error history for the failed process
  error_history <- stream$error_history("test_dummy")
  expect_true(is.list(error_history))
  expect_true("status" %in% names(error_history))
  expect_equal(error_history$status$status, "dead")
  
  # Clean up
  stream$stop_streaming()
})

test_that("Stream can attempt recovery for failed processes", {
  testthat::skip_on_ci()
  
  stream <- Stream$new()
  
  # Start streaming
  stream$start_streaming()
  
  # Register a process that will terminate
  dummy_process <- callr::r_bg(function() {
    # Exit with error status 1
    quit(status = 1)
  })
  
  # Wait for the process to terminate
  Sys.sleep(0.5)
  
  # Register the terminated process
  managed_process <- stream$process_manager$register_process(dummy_process, "test_recovery")
  
  # Verify the process is dead
  status <- stream$process_status()
  expect_equal(status$test_recovery$status, "dead")
  
  # Define a recovery function
  recovery_function <- function() {
    # Return a new process that stays alive
    return(callr::r_bg(function() {
      Sys.sleep(10)
      return(0)
    }))
  }
  
  # Attempt recovery
  result <- stream$process_manager$restart_process("test_recovery", recovery_function)
  
  # Verify the restart was successful
  expect_true(result)
  
  # Check the process status after recovery
  status_after_recovery <- stream$process_status()
  expect_equal(status_after_recovery$test_recovery$status, "running")
  expect_equal(status_after_recovery$test_recovery$restarts, 1)
  
  # Check the error history and restart count
  error_history <- stream$error_history("test_recovery")
  expect_equal(error_history$status$restarts, 1)
  
  # Clean up - make sure to kill the recovered process
  status_after_recovery$test_recovery$process$kill()
  stream$stop_streaming()
})