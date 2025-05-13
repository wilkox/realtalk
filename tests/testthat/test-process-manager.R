library(testthat)
withr::local_options(cli.default_handler = function(...) { NULL })

test_that("ProcessManager can be created", {
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  
  expect_no_error({
    process_manager <- ProcessManager$new(logger = logger)
  })
  
  # Test initialization
  expect_equal(class(process_manager)[1], "ProcessManager")
})

test_that("ProcessManager can register processes", {
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  process_manager <- ProcessManager$new(logger = logger)
  
  # Create a background process to register
  process <- callr::r_bg(function() {
    Sys.sleep(5) # Long-running process
    return(0)
  })
  
  # Register the process
  expect_no_error({
    managed_process <- process_manager$register_process(
      process = process,
      name = "test_process"
    )
  })
  
  # Check that the process was registered correctly
  expect_no_error({ processes <- process_manager$check_processes() })
  expect_true("test_process" %in% names(processes))
  expect_true(processes$test_process$status == "running")
  
  # Clean up
  process$kill()
})

test_that("ProcessManager can check multiple processes", {
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  process_manager <- ProcessManager$new(logger = logger)
  
  # Create two processes - one that runs, one that exits
  live_process <- callr::r_bg(function() {
    Sys.sleep(5) # Long-running process
    return(0)
  })
  
  dead_process <- callr::r_bg(function() {
    return(1) # Exit immediately with status 1
  })
  
  # Wait for the dead process to finish
  Sys.sleep(1)
  
  # Register both processes
  process_manager$register_process(live_process, "live_process")
  process_manager$register_process(dead_process, "dead_process")
  
  # Check all processes
  expect_no_error({ statuses <- process_manager$check_processes() })
  expect_true(statuses$live_process$status == "running")
  expect_true(statuses$dead_process$status == "dead")
  
  # Check individual process
  expect_no_error({ live_status <- process_manager$check_process("live_process") })
  expect_true(live_status$status == "running")
  
  # Clean up
  live_process$kill()
})

test_that("ProcessManager can get error history", {
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  process_manager <- ProcessManager$new(logger = logger)
  
  # Create a process that outputs errors and then exits
  process <- callr::r_bg(function() {
    message("This is an error message")
    warning("This is a warning")
    stop("Deliberate error for testing")
    return(1)
  })
  
  # Wait for the process to finish
  Sys.sleep(1)
  
  # Register the process
  process_manager$register_process(process, "error_process")
  
  # Allow time for output collection
  Sys.sleep(1)
  
  # Get error history for the process
  expect_no_error({ error_history <- process_manager$get_error_history("error_process") })
  expect_true(any(grepl("error", error_history$stderr, ignore.case = TRUE)) || 
              any(grepl("warning", error_history$stderr, ignore.case = TRUE)))
  
  # Get error history for all processes
  expect_no_error({ all_errors <- process_manager$get_error_history() })
  expect_true("error_process" %in% names(all_errors))
})

test_that("ProcessManager can restart a process", {
  # Create a simple logger for testing
  logger <- Logger$new(log_path = fs::file_temp(pattern = "logger", ext = "log"))
  process_manager <- ProcessManager$new(logger = logger)
  
  # Create a process that exits quickly
  process <- callr::r_bg(function() {
    message("About to exit")
    return(1) # Exit with status 1
  })
  
  # Wait for the process to finish
  Sys.sleep(1)
  
  # Register the process
  process_manager$register_process(process, "dead_process")
  
  # Verify process is dead
  status <- process_manager$check_process("dead_process")
  expect_true(status$status == "dead")
  
  # Create restart function
  restart_function <- function() {
    return(callr::r_bg(function() {
      Sys.sleep(5) # Long-running process
      return(0)
    }))
  }
  
  # Restart the process
  expect_no_error({ 
    result <- process_manager$restart_process("dead_process", restart_function) 
  })
  expect_true(result)
  
  # Verify process was restarted
  status <- process_manager$check_process("dead_process")
  expect_true(status$status == "running")
  
  # Clean up
  process_manager$check_process("dead_process")$process$kill()
})