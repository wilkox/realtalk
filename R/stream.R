#' R6 Class Representing A Stream
#'
#' @description Represents a streaming session with the OpenAI Realtime API
Stream <- R6::R6Class("Stream",

  public = list(

    #' @field websocket The websocket::WebSocket object
    websocket = NULL,

    #' @field verbose Should messages be printed?
    verbose = NULL,

    #' @field eventlog A log of all sent and received events
    eventlog = NULL,

    #' @description
    #' A transcript of the text and audio conversation so far
    transcript = function() {

      events <- self$eventlog$as_tibble()

      # Select printable events
      events <- dplyr::filter(events, type %in% c(
        "conversation.item.created",
        "response.text.done",
        "response.audio_transcript.done"
      ))

      # Remove conversation.item.created events that were still 'in_progress'
      events <- dplyr::filter(events, purrr::pmap_lgl(events, function(type, data, ...) {
        if (type == "conversation.item.created") {
          return(data$item$status == "completed")
        } else {
          return(TRUE)
        }
      }))

      # Extract the relevant text
      events$text <- purrr::pmap_chr(events, function(type, data, ...) {
        if (type == "conversation.item.created") {
          return(data$item$content$text)
        }
        if (type == "response.text.done") {
          return(data$text)
        }
        if (type == "response.audio_transcript.done") {
          return(data$transcript)
        }

        return(glue::glue("I don't know how to extract this type of text! Type is {type}"))
      })

      # Print a neat transcript
      cli::cli_h1("Transcript")

      # Set up styling functions
      style_user <- function(text) cli::col_cyan(text)
      style_other <- function(text) cli::col_green(text)
  
      # Calculate console width
      console_width <- getOption("width", 80)
      msg_width <- floor(console_width * 0.7)
  
      # Display messages
      for (i in seq_len(nrow(events))) {
        sender <- events$type[i]
        message <- events$text[i]
        
        # Determine if this is a user message
        is_user <- sender == "conversation.item.created"
        
        # Format and display the message
        lines <- strwrap(message, width = msg_width)
        
        for (line in lines) {
          if (is_user) {
            # Right-aligned user message
            spaces <- console_width - nchar(line)
            spaces <- max(2, spaces)  # Ensure at least some spacing
            padding <- strrep(" ", spaces)
            styled_text <- style_user(line)
            # Print padded text and styled text separately
            cat(padding, styled_text, "\n", sep = "")
          } else {
            # Left-aligned other message with a small indent
            cat(style_other(line), "\n", sep = "")
          }
        }
        
        # Add spacing between messages
        cat("\n")
      }

    },

    #' @description
    #' Open a streaming connection to OpenAI Realtime API via WebSockets
    #' @param api_key Your long-term OpenAI API key. Defaults to
    #' `Sys.getenv("OPENAI_API_KEY")`.
    #' @param model A character string specifying the model. Defaults to
    #' `lemur::openai_api_key()`.
    #' @param voice A character string specifying the voice to use. Defaults to
    #' "verse".
    #' @param verbose Print messages? Defaults to FALSE. Errors will always be
    #' printed.
    #' @return A new Stream object
    #'
    initialize = function(
      api_key = lemur::openai_api_key(verbose = verbose),
      model = "gpt-4o-realtime-preview-2024-12-17",
      voice = "verse",
      verbose = FALSE
    ) {

      self$verbose <- verbose
      self$eventlog <- EventLog$new()

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
      start_time <- Sys.time()
      timeout <- 10  # 10 seconds timeout
      while (self$websocket$readyState() == 0L && difftime(Sys.time(), start_time, units = "secs") < timeout) {
        Sys.sleep(0.1)
        do_later_now()
      }
      if (self$websocket$readyState() != 1L) {
        cli::cli_abort("Failed to establish connection after {timeout} seconds")
      }
      do_later_now()

    },

    #' Send an audio message to the stream
    #'
    #' @param audio The base64-encoded audio
    #'
    send_audio = function(audio) {

      if (self$websocket$readyState() != 1L) {
        cli::cli_abort("Stream is not in a ready state")
      }

      # Send the audio, there is no need to commit or trigger a response as
      # this happens automatically in VAD mode
      payload <- list(
        type = jsonlite::unbox("input_audio_buffer.append"),
        audio = jsonlite::unbox(audio)
      )
      self$websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))

    },

    #' Send a text message to the stream
    #'
    #' @param text A character string to send.
    #' @param response_modalities The modalit(y|ies) that the model should respond
    #' in. Either "text" or c("audio", "text"). Defaults to text only.
    #' @param role The role, either "user" (default) or "system".
    #'
    send_text = function(text, response_modalities = c("text"), role = "user") {

      if (self$websocket$readyState() != 1L) {
        cli::cli_abort("Stream is not in a ready state")
      }

      # Send the text
      payload <- list(
        type = jsonlite::unbox("conversation.item.create"),
        item = list(
          type = jsonlite::unbox("message"),
          role = jsonlite::unbox(role),
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
      Event$new(data) |> self$eventlog$add()
    },

    #' @description
    #' Close the stream
    close = function() {
      do_later_now() # Flush any pending activity before closing to prevent a warning
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
