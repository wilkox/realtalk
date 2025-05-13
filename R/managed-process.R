#' R6 Class for Managing a Background Process
#'
#' @description
#' The ManagedProcess class wraps a background process (typically created with callr::r_bg)
#' and provides integrated monitoring, output capture, and status tracking. It automatically
#' buffers stdout and stderr from the process with timestamps, monitors process health,
#' and maintains complete process information even after termination.
#'
#' @export
ManagedProcess <- R6::R6Class("ManagedProcess",
  public = list(
    #' @description
    #' Create a new ManagedProcess
    #'
    #' @param process A callr process object (typically from callr::r_bg)
    #' @param name A unique identifier string for the process
    #' @param logger A Logger instance for logging events
    #' @param max_buffer_size Optional safety limit for buffer size (default 10000)
    #'
    #' @return A new ManagedProcess object
    initialize = function(process, name, logger, max_buffer_size = 10000) {
      # Validate inputs
      checkmate::assert_class(process, classes = "process")
      checkmate::assert_string(name, min.chars = 1)
      checkmate::assert_class(logger, classes = "Logger")
      checkmate::assert_count(max_buffer_size, positive = TRUE)
      
      # Store references
      private$process <- process
      private$process_name <- name
      private$logger <- logger
      private$max_buffer_size <- max_buffer_size
      
      # Initialize status tracking
      private$started_at <- Sys.time()
      private$last_checked <- Sys.time()
      private$status <- "running"
      private$restarts <- 0
      private$exit_status <- NULL
      
      # Initialize output buffers with timestamp
      private$stdout_buffer <- character(0)
      private$stderr_buffer <- character(0)
      private$buffer_start_time <- Sys.time()
      
      # Log initialization
      logger$info(
        component = name,
        message = "Managed process initialized",
        context = list(
          started_at = format(private$started_at)
        )
      )
      
      # Start buffer collection
      self$check()
    },
    
    #' @description
    #' Check if the process is still alive and update status
    #'
    #' @return A list containing status information
    check = function() {
      # Update last checked timestamp
      private$last_checked <- Sys.time()
      
      # Collect any new output
      if (!is.null(private$process)) {
        tryCatch({
          # Collect stdout if available
          stdout <- private$process$read_output()
          if (!is.null(stdout) && nchar(stdout) > 0) {
            private$append_stdout(stdout)
          }
          
          # Collect stderr if available
          stderr <- private$process$read_error()
          if (!is.null(stderr) && nchar(stderr) > 0) {
            private$append_stderr(stderr)
          }
        }, error = function(e) {
          private$logger$warning(
            component = private$process_name,
            message = "Error collecting process output",
            context = list(error = e$message)
          )
        })
      }
      
      # Check process status
      status_changed <- FALSE
      
      if (is.null(private$process)) {
        # Process was never initialized properly
        if (private$status != "dead") {
          private$status <- "dead"
          private$exit_status <- "process_null"
          status_changed <- TRUE
        }
      } else if (!private$process$is_alive()) {
        # Process has died
        if (private$status != "dead") {
          private$status <- "dead"
          status_changed <- TRUE
          
          # Get exit status
          tryCatch({
            private$exit_status <- private$process$get_exit_status()
          }, error = function(e) {
            private$exit_status <- "unknown"
            private$logger$warning(
              component = private$process_name,
              message = "Could not get exit status",
              context = list(error = e$message)
            )
          })
          
          # Log process termination
          private$logger$error(
            component = private$process_name,
            message = "Process died unexpectedly",
            context = list(
              uptime = format(difftime(Sys.time(), private$started_at)),
              exit_status = private$exit_status
            )
          )
        }
      } else {
        # Process is running normally
        if (private$status != "running") {
          private$status <- "running"
          status_changed <- TRUE
          
          private$logger$info(
            component = private$process_name,
            message = "Process is running",
            context = list(
              uptime = format(difftime(Sys.time(), private$started_at))
            )
          )
        }
      }
      
      # Return current status
      return(self$get_status())
    },
    
    #' @description
    #' Get detailed status information about the process
    #'
    #' @return A list with status details
    get_status = function() {
      list(
        process = private$process,
        name = private$process_name,
        status = private$status,
        started_at = private$started_at,
        last_checked = private$last_checked,
        uptime = difftime(Sys.time(), private$started_at),
        restarts = private$restarts,
        exit_status = private$exit_status
      )
    },
    
    #' @description
    #' Attempt to restart a dead process
    #'
    #' @param restart_function A function that returns a new process object
    #' @return TRUE if restart succeeded, FALSE otherwise
    restart = function(restart_function) {
      if (private$status != "dead") {
        private$logger$warning(
          component = private$process_name,
          message = "Attempted to restart a process that is not dead",
          context = list(current_status = private$status)
        )
        return(FALSE)
      }
      
      private$logger$info(
        component = private$process_name,
        message = "Attempting to restart process",
        context = list(
          previous_exit_status = private$exit_status,
          previous_restarts = private$restarts
        )
      )
      
      # Attempt restart
      tryCatch({
        # Call the provided restart function to get a new process
        new_process <- restart_function()
        
        # Update internal state
        private$process <- new_process
        private$started_at <- Sys.time()
        private$last_checked <- Sys.time()
        private$status <- "running"
        private$restarts <- private$restarts + 1
        private$exit_status <- NULL
        
        private$logger$info(
          component = private$process_name,
          message = "Process restarted successfully",
          context = list(restarts = private$restarts)
        )
        
        return(TRUE)
      }, error = function(e) {
        private$logger$error(
          component = private$process_name,
          message = "Failed to restart process",
          context = list(error = e$message)
        )
        return(FALSE)
      })
    },
    
    #' @description
    #' Get all stdout content with timestamps
    #'
    #' @return A character vector of timestamped stdout lines
    get_stdout = function() {
      return(private$stdout_buffer)
    },
    
    #' @description
    #' Get all stderr content with timestamps
    #'
    #' @return A character vector of timestamped stderr lines
    get_stderr = function() {
      return(private$stderr_buffer)
    },
    
    #' @description
    #' Get all process information and outputs
    #'
    #' @return A list containing status information and all output
    get_combined_output = function() {
      list(
        status = self$get_status(),
        stdout = private$stdout_buffer,
        stderr = private$stderr_buffer
      )
    }
  ),
  
  private = list(
    # The actual callr process object
    process = NULL,
    
    # Process identification
    process_name = NULL,
    
    # Status tracking
    started_at = NULL,
    status = NULL,
    last_checked = NULL,
    restarts = 0,
    exit_status = NULL,
    
    # Output buffering
    stdout_buffer = NULL,
    stderr_buffer = NULL,
    buffer_start_time = NULL,
    max_buffer_size = NULL,
    
    # Logger reference
    logger = NULL,
    
    # Add content to stdout buffer with timestamp
    append_stdout = function(content) {
      # Skip if empty
      if (is.null(content) || nchar(content) == 0) {
        return()
      }
      
      # Split multiline content
      lines <- strsplit(content, split = "\\n")[[1]]
      
      # Add timestamp to each line
      timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
      timestamped_lines <- paste0(timestamp, " ", lines)
      
      # Append to buffer
      private$stdout_buffer <- c(private$stdout_buffer, timestamped_lines)
      
      # Check buffer size
      private$check_buffer_size()
    },
    
    # Add content to stderr buffer with timestamp
    append_stderr = function(content) {
      # Skip if empty
      if (is.null(content) || nchar(content) == 0) {
        return()
      }
      
      # Split multiline content
      lines <- strsplit(content, split = "\\n")[[1]]
      
      # Add timestamp to each line
      timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
      timestamped_lines <- paste0(timestamp, " ", lines)
      
      # Append to buffer
      private$stderr_buffer <- c(private$stderr_buffer, timestamped_lines)
      
      # Check buffer size
      private$check_buffer_size()
    },
    
    # Safety check to prevent excessive memory usage
    check_buffer_size = function() {
      # Check stdout buffer
      if (length(private$stdout_buffer) > private$max_buffer_size) {
        # Keep the newest entries
        private$stdout_buffer <- tail(private$stdout_buffer, private$max_buffer_size)
        private$logger$warning(
          component = private$process_name,
          message = "Stdout buffer exceeded maximum size, truncating",
          context = list(max_size = private$max_buffer_size)
        )
      }
      
      # Check stderr buffer
      if (length(private$stderr_buffer) > private$max_buffer_size) {
        # Keep the newest entries
        private$stderr_buffer <- tail(private$stderr_buffer, private$max_buffer_size)
        private$logger$warning(
          component = private$process_name,
          message = "Stderr buffer exceeded maximum size, truncating",
          context = list(max_size = private$max_buffer_size)
        )
      }
    }
  )
)