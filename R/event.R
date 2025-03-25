#' R6 Class Representing a Stream Event
#'
#' @export
Event <- R6::R6Class("Event",

  public = list(

    #' @field data The raw event data
    data = NULL,

    #' @field created_at Timestamp for when the Event was created
    created_at = NULL,

    #' @description 
    #' Create an Event
    #' @param data The event data
    initialize = function(data) {
      self$data <- data
      self$created_at <- lubridate::now()
    },

    #' @description
    #' Return the event as a tibble
    #' @return a tibble
    as_tibble = function() {
      tibble::tibble(
        created_at = self$created_at,
        type = self$data$type,
        data = list(self$data)
      )
    },

    #' @description
    #' Print the event
    print = function() {
      cli::cli_h1("Event created at {self$created_at}")
      print(self$data)
    }
  )
)
