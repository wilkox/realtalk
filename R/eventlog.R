#' R6 Class Representing a Log of Events in a Stream
#'
#' @export
EventLog <- R6::R6Class("EventLog",

  public = list(

    #' @description
    #' Create an EventLog
    initialize = function() {

      # Initialise file containing serialised events list and lock file
      private$events_path <- fs::file_temp(pattern = "events_list", ext = "rds")
      private$events_lock <- fs::file_temp(pattern = "events_list_lockfile")

      # Initialise events list and serialise to file
      events <- list()
      saveRDS(events, private$events_path)

    },

    #' @description
    #' Add an Event
    #' @param event An Event object
    add = function(event) {

      # Check that the object to be added is an event
      checkmate::assertR6(event, classes = "Event")

      # Wait for control of the events file
      lock_elapsed <- 0
      lock_timeout <- 10
      while (fs::file_exists(private$events_lock)) {
        lock_elapsed <- lock_elapsed + 1
        if (lock_elapsed >= lock_timeout) {
          cli::cli_abort("Timed out waiting for event file to unlock after {lock_elapsed} seconds")
        }
        Sys.sleep(1)
      }

      # Lock the events file and serialise new events list
      fs::file_touch(private$events_lock)
      events <- readRDS(private$events_path)
      events <- c(events, list(event))
      saveRDS(events, private$events_path)
      fs::file_delete(private$events_lock)
    },

    #' @description
    #' Return the eventlog as a tibble
    as_tibble = function() {

      # Wait for control of the events file
      lock_elapsed <- 0
      lock_timeout <- 10
      while (fs::file_exists(private$events_lock)) {
        lock_elapsed <- lock_elapsed + 1
        if (lock_elapsed >= lock_timeout) {
          cli::cli_abort("Timed out waiting for event file to unlock after {lock_elapsed} seconds")
        }
        Sys.sleep(1)
      }

      # Lock the events file and read the events list
      fs::file_touch(private$events_lock)
      events <- readRDS(private$events_path)
      fs::file_delete(private$events_lock)

      # Bind the events into a tibble and return
      events <- lapply(events, function(event) { event$as_tibble() })
      dplyr::bind_rows(events)
    },

    #' @description
    #' Print the eventlog
    print = function() {
      print(self$as_tibble())
    }
  ),

  private = list(

    # path of a serialised list of events
    events_path = NULL,

    # path to a lockfile for events_path
    events_lock = NULL

  )
)
