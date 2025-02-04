#' R6 Class Representing A Stream
#'
#' @description Represents a streaming session with the OpenAI Realtime API
Stream <- R6::R6Class("Stream",

  public = list(

    #' @field websocket The websocket::WebSocket object
    websocket = NULL,

    #' @field verbose Should messages be printed?
    verbose = NULL,

    #' @description
    #' Open a streaming connection to OpenAI Realtime API via WebSockets
    #' @param api_key Your long-term OpenAI API key. Defaults to
    #' `Sys.getenv("OPENAI_API_KEY")`.
    #' @param model A character string specifying the model. Defaults to
    #' `lemur::openai_api_key()`.
    #' @param voice A character string specifying the voice to use. Defaults to
    #' "verse".
    #' @param verbose Print messages? Defaults to TRUE. Errors will always be
    #' printed.
    #' @return A new Stream object
    initialize = function(
      api_key = lemur::openai_api_key(verbose = verbose),
      model = "gpt-4o-realtime-preview-2024-12-17",
      voice = "verse",
      verbose = TRUE
    ) {

      self$verbose <- verbose

      # Create a new WebSocket client
      url <- paste0("wss://api.openai.com/v1/realtime?model=", model)
      headers <- list(
        "Authorization" = paste0("Bearer ", api_key),
        "OpenAI-Beta" = "realtime=v1"
      )
      self$websocket <- websocket::WebSocket$new(url, header = headers, autoConnect = FALSE)
      
      # Define event callbacks
      self$websocket$onOpen(function(event) {
        if (verbose) cli::cli_alert_success("Connected to server.")
      })
      
      self$websocket$onMessage(function(event) { self$receive_event(event) })
      
      self$websocket$onError(function(event) {
        cli::cli_alert_danger("WebSocket error: {event$message}")
      })
      
      self$websocket$onClose(function(event) {
        if (verbose) cli::cli_alert_success("Connection closed.")
      })
      
      # Connect to the websocket server
      self$websocket$connect()
      do_later_now()

    },

    #' @description
    #' Send a text message to the stream
    #'
    #' @param text A character string to send.
    #' @param response_modalities The modalit(y|ies) that the model should respond
    #' in. At least one of "text" and "audio". Defaults to text only.
    send_text = function(text, response_modalities = c("text")) {

      # Send the text
      payload <- list(
        type = jsonlite::unbox("conversation.item.create"),
        item = list(
          type = jsonlite::unbox("message"),
          role = jsonlite::unbox("user"),
          content = list(list(
            type = jsonlite::unbox("input_text"),
            text = jsonlite::unbox(text)
          ))
        )
      )
      self$websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))

      # Trigger a response
      payload <- list(
        type = jsonlite::unbox("response.create"),
        response = list(modalities = response_modalities)
      )
      self$websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))
    },

    #' @description
    #' Receive and process an event from the stream
    #' @param event The WebSockets event
    receive_event = function(event) {
      data <- jsonlite::fromJSON(event$data)
      if (self$verbose) cli::cli_alert_info("Received event:\n{jsonlite::toJSON(data, pretty = TRUE)}")
    },

    #' @description
    #' Close the stream
    close = function() {
      self$websocket$close()
      do_later_now()
    },

    #' @description
    #' Report the current ready state of the stream
    #' @return An integer representing the state of the connection: 0L =
    #' Connecting, 1L = Open, 2L = Closing, 3L = Closed.
    ready_state = function() {
      do_later_now()
      self$websocket$readyState() |> as.integer()
    }
  )
)
