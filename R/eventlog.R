#' R6 Class Representing a Log of Events in a Stream
#'
#' @export
EventLog <- R6::R6Class("EventLog",

  public = list(

    #' @description
    #' Create an EventLog
    initialize = function() {

      # Binary log file for individual serialized events
      private$log_path <- fs::file_temp(pattern = "events_log", ext = "bin")
      private$log_lock <- fs::file_temp(pattern = "events_log_lockfile")
      
      # Create empty log file if it doesn't exist
      if (!fs::file_exists(private$log_path)) {
        fs::file_create(private$log_path)
      }
    },

    #' @description
    #' Add an Event
    #' @param event An Event object
    add = function(event) {

      # Check that the object to be added is an event
      checkmate::assertR6(event, classes = "Event")
      
      # Wait for control of the log file
      lock_elapsed <- 0
      lock_timeout <- 30
      while (fs::file_exists(private$log_lock)) {
        lock_elapsed <- lock_elapsed + 0.1
        if (lock_elapsed >= lock_timeout) {
          cli::cli_abort("Timed out waiting for log file to unlock after {lock_elapsed} seconds")
        }
        Sys.sleep(0.1)
      }
      
      # Lock the log file
      fs::file_touch(private$log_lock)
      
      # Open the log file in append binary mode
      con <- file(private$log_path, "ab")
      
      # Write length marker (for easier reading later)
      # Serialize to a raw vector first
      serialized_event <- serialize(event, NULL)
      # Write the length of the serialized data
      writeBin(length(serialized_event), con, size = 4)
      # Write the serialized data itself
      writeBin(serialized_event, con)
      
      # Close the connection
      close(con)
      
      # Release lock
      fs::file_delete(private$log_lock)
      
      invisible(self)
    },

    #' @description
    #' Return the eventlog as a tibble
    as_tibble = function() {

      # Wait for control of the log file
      lock_elapsed <- 0
      lock_timeout <- 30
      while (fs::file_exists(private$log_lock)) {
        lock_elapsed <- lock_elapsed + 0.1
        if (lock_elapsed >= lock_timeout) {
          cli::cli_abort("Timed out waiting for log file to unlock after {lock_elapsed} seconds")
        }
        Sys.sleep(0.1)
      }
      
      # Lock the log file
      fs::file_touch(private$log_lock)
      
      # Check if file exists and has content
      events <- list()
      message("Log path is:")
      message(private$log_path)
      
      if (fs::file_exists(private$log_path) && file.size(private$log_path) > 0) {
        # Open connection to log file
        con <- file(private$log_path, "rb")
        
        # Read events until we hit EOF
        while (TRUE) {
          # Try to read length marker
          len <- tryCatch(
            readBin(con, integer(), n = 1, size = 4),
            error = function(e) NULL
          )
          
          # Break if we've reached EOF
          if (is.null(len) || length(len) == 0) break
          
          # Read the serialized event
          serialized_data <- readBin(con, raw(), n = len)
          
          # Deserialize the event
          event <- unserialize(serialized_data)
          
          # Add to our list
          events <- c(events, list(event))
        }
        
        # Close the connection
        close(con)
      }
      
      # Release lock
      fs::file_delete(private$log_lock)
      
      # Convert events to tibbles and combine
      if (length(events) > 0) {
        event_tibbles <- lapply(events, function(event) { event$as_tibble() })
        return(dplyr::bind_rows(event_tibbles))
      } else {
        return(tibble::tibble())
      }
    },

    #' @description
    #' Print the eventlog
    print = function() {
      print(self$as_tibble())
    },
    
    #' @description
    #' Compact the log file to optimize storage and reading
    compact = function() {
      # Wait for control of the log file
      lock_elapsed <- 0
      lock_timeout <- 30
      while (fs::file_exists(private$log_lock)) {
        lock_elapsed <- lock_elapsed + 0.1
        if (lock_elapsed >= lock_timeout) {
          cli::cli_abort("Timed out waiting for log file to unlock after {lock_elapsed} seconds")
        }
        Sys.sleep(0.1)
      }
      
      # Lock the log file
      fs::file_touch(private$log_lock)
      
      # Read all events
      events <- list()
      
      if (fs::file_exists(private$log_path) && file.size(private$log_path) > 0) {
        con <- file(private$log_path, "rb")
        
        while (TRUE) {
          # Try to read length marker
          len <- tryCatch(
            readBin(con, integer(), n = 1, size = 4),
            error = function(e) NULL
          )
          
          # Break if we've reached EOF
          if (is.null(len) || length(len) == 0) break
          
          # Read the serialized event
          serialized_data <- readBin(con, raw(), n = len)
          
          # Deserialize the event
          event <- unserialize(serialized_data)
          
          # Add to our list
          events <- c(events, list(event))
        }
        
        close(con)
      }
      
      # Create a temporary file
      temp_path <- fs::file_temp(pattern = "events_log_compact", ext = "bin")
      
      # Write all events to the new file in optimized format
      if (length(events) > 0) {
        con <- file(temp_path, "wb")
        
        for (event in events) {
          # Serialize to a raw vector
          serialized_event <- serialize(event, NULL)
          # Write length
          writeBin(length(serialized_event), con, size = 4)
          # Write data
          writeBin(serialized_event, con)
        }
        
        close(con)
        
        # Replace original with compacted version
        file.copy(temp_path, private$log_path, overwrite = TRUE)
        fs::file_delete(temp_path)
      }
      
      # Release lock
      fs::file_delete(private$log_lock)
      
      invisible(self)
    }
  ),

  private = list(
    # Path to the log file
    log_path = NULL,
    
    # Path to the log file lock
    log_lock = NULL
  )
)
