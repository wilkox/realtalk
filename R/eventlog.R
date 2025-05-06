#' R6 Class Representing a Log of Events in a Stream
#'
#' @description
#' The EventLog class provides a persistent, file-backed storage system for Event objects
#' in a Stream. It handles serialization, locking, and retrieval of events. The class
#' uses a binary file to store serialized events with length markers for efficient access.
#'
#' @export
EventLog <- R6::R6Class("EventLog",

  public = list(

    #' @description
    #' Create a new EventLog
    #' 
    #' Initializes a new file-backed EventLog with a temporary file to store events and
    #' a lock file to manage concurrent access.
    #' 
    #' @return A new EventLog object
    initialize = function() {
      # Binary log file for individual serialized events
      private$log_path <- fs::file_temp(pattern = "events_log", ext = "bin")
      
      # Create empty log file if it doesn't exist
      if (!fs::file_exists(private$log_path)) {
        fs::file_create(private$log_path)
      }
    },

    #' @description
    #' Add an Event to the EventLog
    #' @param event An Event object to add to the log
    #' @return Invisibly returns the EventLog object (for method chaining)
    add = function(event) {
      # Check that the object to be added is an event
      checkmate::assertR6(event, classes = "Event")
      
      # Acquire lock
      lck <- private$acquire_lock()
      
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
        private$release_lock(lck)
        stop(e)
      },
      finally = {
        # Release lock in all cases
        private$release_lock(lck)
      })
      
      invisible(self)
    },

    #' @description
    #' Return the eventlog as a tibble
    #' @return A tibble with columns for created_at, type, and data for each Event
    as_tibble = function() {
      # Acquire lock
      lck <- private$acquire_lock()
      
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
        private$release_lock(lck)
        stop(e)
      },
      finally = {
        # Release lock in all cases
        private$release_lock(lck)
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
    #' @return Invisibly returns the printed tibble
    print = function() {
      print(self$as_tibble())
    },
    
    #' @description
    #' Compact the log file to optimize storage and reading
    #' 
    #' This method reads all events from the log file and writes them back
    #' to a new file in a more compact format. This can improve performance
    #' after many events have been added and removed.
    #' 
    #' @return Invisibly returns the EventLog object (for method chaining)
    compact = function() {
      # Acquire lock
      lck <- private$acquire_lock()
      
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
        private$release_lock(lck)
        stop(e)
      },
      finally = {
        # Release lock in all cases
        private$release_lock(lck)
      })
      
      invisible(self)
    }
  ),

  private = list(
    # Path to the log file
    log_path = NULL,
    
    # Acquire lock using filelock package
    acquire_lock = function() {
      # Use filelock to acquire a lock with timeout
      lck <- filelock::lock(fs::path(private$log_path, ext = "lock"), timeout = 60000)
      if (is.null(lck)) {
        cli::cli_abort("Failed to acquire lock on event log file")
      }
      return(lck)
    },
    
    # Release lock using filelock package
    release_lock = function(lck) {
      # Only attempt to unlock if we have a valid lock
      if (!is.null(lck)) {
        filelock::unlock(lck)
      }
    }
  )
)
