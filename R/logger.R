#' R6 Class for Unified Logging
#'
#' @description
#' The Logger class provides a unified logging system with standardized formats, 
#' severity levels, and thread safety. It supports logging at INFO, WARNING, and 
#' ERROR levels with component tagging and structured context data.
#'
#' @export
Logger <- R6::R6Class("Logger",
  public = list(
    #' @description
    #' Create a new Logger
    #'
    #' @param log_path Path to the log file
    #'
    #' @return A new Logger object
    initialize = function(log_path) {
      # Validate inputs
      checkmate::assert_string(log_path, min.chars = 1)
      
      # Store log path
      private$log_path <- log_path
      
      # Create log file if it doesn't exist
      if (!fs::file_exists(private$log_path)) {
        tryCatch({
          fs::file_create(private$log_path)
        }, error = function(e) {
          cli::cli_abort(c(
            "Failed to create log file at {private$log_path}",
            "i" = "Error: {e$message}",
            "i" = "Ensure the directory exists and is writable"
          ))
        })
      }
      
      # Log initialization
      self$info(
        component = "Logger",
        message = "Logger initialized",
        context = list(log_path = private$log_path)
      )
    },
    
    #' @description
    #' Log an informational message
    #'
    #' @param component The component generating the log entry
    #' @param message The message to log
    #' @param context Optional list of contextual data
    #'
    #' @return Invisibly returns NULL
    info = function(component, message, context = list()) {
      self$log("INFO", component, message, context)
    },
    
    #' @description
    #' Log a warning message
    #'
    #' @param component The component generating the log entry
    #' @param message The message to log
    #' @param context Optional list of contextual data
    #'
    #' @return Invisibly returns NULL
    warning = function(component, message, context = list()) {
      self$log("WARNING", component, message, context)
    },
    
    #' @description
    #' Log an error message
    #'
    #' @param component The component generating the log entry
    #' @param message The message to log
    #' @param context Optional list of contextual data
    #'
    #' @return Invisibly returns NULL
    error = function(component, message, context = list()) {
      self$log("ERROR", component, message, context)
    },
    
    #' @description
    #' Direct logging implementation
    #'
    #' @param level The log level (INFO, WARNING, ERROR)
    #' @param component The component generating the log entry
    #' @param message The message to log
    #' @param context Optional list of contextual data
    #'
    #' @return Invisibly returns NULL
    log = function(level, component, message, context = list()) {
      # Validate inputs
      checkmate::assert_choice(level, choices = c("INFO", "WARNING", "ERROR"))
      checkmate::assert_string(component, min.chars = 1)
      checkmate::assert_string(message, min.chars = 1)
      checkmate::assert_list(context, null.ok = TRUE)
      
      # Format timestamp
      timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
      
      # Format context JSON if provided
      context_str <- ""
      if (length(context) > 0) {
        tryCatch({
          context_str <- jsonlite::toJSON(context, auto_unbox = TRUE)
        }, error = function(e) {
          context_str <- paste0('{"error":"Failed to serialize context: ', e$message, '"}')
        })
      }
      
      # Build log entry
      log_entry <- paste0(
        timestamp, " [", level, "] [", component, "] ", message,
        if (nchar(context_str) > 0) paste0(" ", context_str) else ""
      )
      
      # Write to log file with thread safety
      private$write_to_log(log_entry)
      
      invisible(NULL)
    },
    
    #' @description
    #' Retrieve filtered log entries
    #'
    #' @param level Optional level to filter by (INFO, WARNING, ERROR)
    #' @param component Optional component name to filter by
    #' @param pattern Optional text pattern to search for
    #' @param n Optional limit on number of entries to return
    #'
    #' @return A character vector of log entries
    get_logs = function(level = NULL, component = NULL, pattern = NULL, n = NULL) {
      # Validate inputs
      if (!is.null(level)) {
        checkmate::assert_choice(level, choices = c("INFO", "WARNING", "ERROR"))
      }
      if (!is.null(component)) {
        checkmate::assert_string(component, min.chars = 1)
      }
      if (!is.null(pattern)) {
        checkmate::assert_string(pattern, min.chars = 1)
      }
      if (!is.null(n)) {
        checkmate::assert_count(n, positive = TRUE)
      }
      
      # Get lock for thread safety
      lock_path <- fs::path(private$log_path, ext = "lock")
      lock <- filelock::lock(lock_path, timeout = 5000)
      
      if (is.null(lock)) {
        cli::cli_warn("Could not acquire lock to read log file. Returning empty result.")
        return(character(0))
      }
      
      # Read log file
      tryCatch({
        # Check if file exists and has content
        if (!fs::file_exists(private$log_path) || fs::file_size(private$log_path) == 0) {
          filelock::unlock(lock)
          return(character(0))
        }
        
        # Read all log entries
        log_entries <- readLines(private$log_path)
        
        # Apply filters
        if (!is.null(level)) {
          level_pattern <- paste0("\\[", level, "\\]")
          log_entries <- log_entries[grepl(level_pattern, log_entries, perl = TRUE)]
        }
        
        if (!is.null(component)) {
          component_pattern <- paste0("\\[", component, "\\]")
          log_entries <- log_entries[grepl(component_pattern, log_entries, perl = TRUE)]
        }
        
        if (!is.null(pattern)) {
          log_entries <- log_entries[grepl(pattern, log_entries, perl = TRUE)]
        }
        
        # Apply limit
        if (!is.null(n) && length(log_entries) > n) {
          log_entries <- tail(log_entries, n)
        }
        
        return(log_entries)
      }, 
      error = function(e) {
        cli::cli_warn("Error reading log file: {e$message}")
        return(character(0))
      },
      finally = {
        # Always release the lock
        filelock::unlock(lock)
      })
    }
  ),
  
  private = list(
    # Path to the log file
    log_path = NULL,
    
    # Thread-safe write to log file
    write_to_log = function(entry) {
      # Get lock with a reasonable timeout
      lock_path <- fs::path(private$log_path, ext = "lock")
      lock <- filelock::lock(lock_path, timeout = 5000)
      
      if (is.null(lock)) {
        # Could not acquire lock, use warning to avoid infinite recursion
        cli::cli_warn("Could not acquire lock to write to log file. Log entry may be lost.")
        return(invisible(NULL))
      }
      
      # Write to log file with lock
      tryCatch({
        # Open in append mode
        con <- file(private$log_path, "at")
        writeLines(entry, con)
        close(con)
      },
      error = function(e) {
        # Use warning to avoid infinite recursion
        cli::cli_warn("Error writing to log file: {e$message}")
      },
      finally = {
        # Always release the lock
        filelock::unlock(lock)
      })
      
      invisible(NULL)
    }
  )
)