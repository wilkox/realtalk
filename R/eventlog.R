#' R6 Class Representing a Log of Events in a Stream
#'
#' @export
EventLog <- R6::R6Class("EventLog",

  public = list(

    #' @field events A list of Event objects
    events = NULL,

    #' @description
    #' Create an EventLog
    initialize = function() {
      self$events <- list()
    },

    #' @description
    #' Add an Event
    #' @param event An Event object
    add = function(event) {
      checkmate::assertR6(event, classes = "Event")
      self$events <- c(self$events, event)
    },

    #' @description
    #' Return the eventlog as a tibble
    as_tibble = function() {
      events <- lapply(self$events, function(event) { event$as_tibble() })
      dplyr::bind_rows(events)
    },

    #' @description
    #' Print the eventlog
    print = function() {
      print(self$as_tibble())
    }
  )
)
