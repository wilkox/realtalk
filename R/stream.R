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
        "conversation.item.created", # Text in
        "response.text.done", # Text out
        "conversation.item.input_audio_transcription.completed", # Audio in
                                                                 # (transcript)
        "response.audio_transcript.done" # Audio out (transcript)
      ))

      # conversation.item.created events must have:
      # - status "completed"
      # - "text" content
      # - "text" content must not be empty
      events <- dplyr::filter(events, purrr::pmap_lgl(events, function(type, data, ...) {
        if (type == "conversation.item.created") {
          if (! data$item$status == "completed") return(FALSE)
          if (! "text" %in% names(data$item$content)) return(FALSE)
          if (stringr::str_length(data$item$content$text) == 0) return(FALSE)
          return(TRUE)
        } else {
          return(TRUE)
        }
      }))

      # Determine the role, medium, and content
      events$message <- purrr::pmap(events, function(type, data, ...) {
        if (type == "conversation.item.created") {
          return(list(
            role = ifelse(
              "roll" %in% names(data$item),
              data$item$roll,
              "system"
            ),
            medium = "text",
            content = data$item$content$text
          ))
        }
        if (type == "response.text.done") {
          return(list(
            role = "assistant",
            medium = "text",
            content = data$text
          ))
        }
        if (type == "conversation.item.input_audio_transcription.completed") {
          return(list(
            role = "user",
            medium = "audio",
            content = data$transcript
          ))
        }
        if (type == "response.audio_transcript.done") {
          return(list(
            role = "assistant",
            medium = "audio",
            content = data$transcript
          ))
        }

        return(glue::glue("I don't know how to extract this type of text! Type is {type}"))
      })

      # Tidy up
      transcript <- events$message |>
        purrr::map(tibble::as_tibble) |>
        dplyr::bind_rows()

      # Print a neat transcript
      cli::cli_h1("Transcript")
      cli::cli_alert_info("Messages may appear out of order due to delays in transcription")

      # Display messages
      for (i in seq_len(nrow(transcript))) {

        message <- transcript[i, ]

        # Select styling function
        if (message$role == "system") {
          style_f <- function(text) cli::col_white(text)
        } else if (message$role == "user") {
          style_f <- function(text) cli::col_red(text)
        } else if (message$role == "assistant") {
          style_f <- function(text) cli::col_cyan(text)
        } else {
          cli::abort("Don't know how to style role {role}")
        }

        cli::cli_h2(style_f(paste0(
          stringr::str_to_title(message$role),
          " [",
          stringr::str_to_title(message$medium),
          "]"
        )))

        cli::cli_text(style_f(message$content))
      }

    },

    #' @description
    #' Open a streaming connection to OpenAI Realtime API via WebSockets
    #' @param api_key Your long-term OpenAI API key. Defaults to
    #' `Sys.getenv("OPENAI_API_KEY")`.
    #' @param model A character string specifying the model. Defaults to
    #' `lemur::openai_api_key()`.
    #' @param voice A character string specifying the voice to use. Defaults to
    #' "ballad".
    #' @param verbose Print messages? Defaults to FALSE. Errors will always be
    #' printed.
    #' @return A new Stream object
    #'
    initialize = function(
      api_key = lemur::openai_api_key(verbose = verbose),
      model = "gpt-4o-realtime-preview-2024-12-17",
      voice = "ballad",
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

      # Send a session.update event with the voice and triggering audio
      # transcription
      payload <- list(
        type = jsonlite::unbox("session.update"),
        session = list(
          voice = jsonlite::unbox(voice),
          input_audio_transcription = list(
            model = jsonlite::unbox("whisper-1")
          )
        )
      )
      self$websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))

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
