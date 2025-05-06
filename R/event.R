#' R6 Class Representing a Stream Event
#'
#' @description
#' The Event class encapsulates events that occur during a streaming session with
#' the OpenAI Realtime API. Events can represent actions like messages being sent
#' or received, audio being processed, or changes to the stream state. Each Event
#' contains the raw event data and a timestamp for when it was created.
#'
#' @export
Event <- R6::R6Class("Event",

  public = list(

    #' @field data The raw event data from the API, usually a list containing event type and payload
    data = NULL,

    #' @field created_at Timestamp for when the Event was created
    created_at = NULL,

    #' @description 
    #' Create a new Event object
    #' @param data A list containing the event data, must include a 'type' field
    #' that identifies the kind of event (e.g., "response.created", "response.done")
    initialize = function(data) {
      self$data <- data
      self$created_at <- lubridate::now()
    },

    #' @description
    #' Convert the event to a tibble format
    #' @return A tibble with three columns:
    #'   - created_at: The timestamp when the event was created
    #'   - type: The event type (extracted from data$type)
    #'   - data: The complete event data as a list column
    as_tibble = function() {
      tibble::tibble(
        created_at = self$created_at,
        type = self$data$type,
        data = list(self$data)
      )
    },

    #' @description
    #' Print the event details to the console
    #' @return Invisibly returns NULL
    print = function() {
      cli::cli_h1("Event created at {self$created_at}")
      print(self$data)
      invisible(NULL)
    }
  )
)
