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
      
      # Acquire lock with backoff
      private$acquire_lock()
      
      # Open the log file in append binary mode
      tryCatch({
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
      }, 
      error = function(e) {
        # Release lock even if an error occurs
        private$release_lock()
        stop(e)
      },
      finally = {
        # Release lock in all cases
        private$release_lock()
      })
      
      invisible(self)
    },

    #' @description
    #' Return the eventlog as a tibble
    as_tibble = function() {
      # Acquire lock with backoff
      private$acquire_lock()
      
      # Initialize empty result
      events <- list()
      
      # Safely read events
      tryCatch({
        # Check if file exists and has content
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
      },
      error = function(e) {
        # Release lock even if an error occurs
        private$release_lock()
        stop(e)
      },
      finally = {
        # Release lock in all cases
        private$release_lock()
      })
      
      # Convert events to tibbles and combine
      if (length(events) > 0) {
        event_tibbles <- lapply(events, function(event) { event$as_tibble() })
        return(dplyr::bind_rows(event_tibbles))
      } else {
        return(tibble::tibble(
          created_at = as.Date(character(0)),
          type = character(0),
          data = list()
        ))
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
      # Acquire lock with backoff
      private$acquire_lock()
      
      # Initialize empty result
      events <- list()
      
      # Safely compact events
      tryCatch({
        # Read all events
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
      },
      error = function(e) {
        # Release lock even if an error occurs
        private$release_lock()
        stop(e)
      },
      finally = {
        # Release lock in all cases
        private$release_lock()
      })
      
      invisible(self)
    }
  ),

  private = list(
    # Path to the log file
    log_path = NULL,
    
    # Path to the log file lock
    log_lock = NULL,
    
    # Acquire lock with exponential backoff
    acquire_lock = function() {
      # Initialize variables for exponential backoff
      attempts <- 0
      max_attempts <- 15  # Maximum number of retry attempts
      base_wait_time <- 0.01  # Start with 10ms
      max_wait_time <- 2  # Maximum wait time in seconds
      
      while (TRUE) {
        # Check if lock file exists
        if (!fs::file_exists(private$log_lock)) {
          # Try to create the lock file
          tryCatch({
            fs::file_touch(private$log_lock)
            # Successfully acquired lock
            return(TRUE)
          }, error = function(e) {
            # Failed to create lock file, will retry
          })
        }
        
        # Increment attempts
        attempts <- attempts + 1
        
        # Check if we've reached max attempts
        if (attempts > max_attempts) {
          cli::cli_abort("Failed to acquire lock after {attempts} attempts")
        }
        
        # Calculate wait time with exponential backoff and jitter
        wait_time <- min(base_wait_time * 2^(attempts - 1), max_wait_time)
        # Add random jitter (±20%)
        jitter <- runif(1, 0.8, 1.2)
        wait_time <- wait_time * jitter
        
        # Wait before retrying
        Sys.sleep(wait_time)
      }
    },
    
    # Release lock
    release_lock = function() {
      if (fs::file_exists(private$log_lock)) {
        tryCatch(
          fs::file_delete(private$log_lock),
          error = function(e) {
            # If we can't delete the lock file, log a warning
            cli::cli_warn("Failed to release lock: {e$message}")
          }
        )
      }
    }
  )
)
