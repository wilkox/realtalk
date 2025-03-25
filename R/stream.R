#' R6 Class Representing A Stream
#'
#' @description Represents a streaming session with the OpenAI Realtime API
Stream <- R6::R6Class("Stream",

  public = list(

    #' @field websocket The websocket::WebSocket object
    websocket = NULL,

    #' @field bg_process The callr background R process in which the websocket
    #' and the main streaming loop are instantiated
    bg_process = NULL,

    #' @field bg_close_path The path for a temporary file used to signal the
    #' background process to close
    bg_close_path = NULL,

    #' @field eventlog The EventLog for a completed stream. Will be NULL until
    #' stop_streaming() has been called.
    eventlog = NULL,

    #' @description
    #' A transcript of the text and audio conversation so far
    transcript = function() {

      # Capture events
      if (is.null(self$eventlog)) {
        cli::cli_abort("No event log found. Has the stream been run?")
      }
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
    #'
    #' Open a WebSocket streaming connection to OpenAI Realtime API and begin
    #' bidirectional audio streaming. The streaming loop will be encapsulated
    #' in a callr subprocess, which is stored in the bg_process slot.
    #'
    #' @param api_key Your long-term OpenAI API key. Defaults to
    #' `Sys.getenv("OPENAI_API_KEY")`.
    #' @param model A character string specifying the model. Defaults to
    #' `lemur::openai_api_key()`.
    #' @param voice A character string specifying the voice to use. Defaults to
    #' "ballad".
    #' @return No return
    #'
    start_streaming = function(
      api_key = lemur::openai_api_key(verbose = FALSE),
      model = "gpt-4o-realtime-preview-2024-12-17",
      voice = "ballad"
    ) {

      # Set up the background process
      self$bg_process <- callr::r_bg(
        function(api_key, model, voice, bg_close_path) {

          # Allow passthrough of cli messages
          options(cli.message_class = "callr_message")

          # Initialise the event log
          eventlog <- realtalk::EventLog$new()

          # Create a new WebSocket client
          url <- paste0("wss://api.openai.com/v1/realtime?model=", model)
          headers <- list(
            "Authorization" = paste0("Bearer ", api_key),
            "OpenAI-Beta" = "realtime=v1"
          )
          websocket <- websocket::WebSocket$new(url, header = headers, autoConnect = FALSE)
          
          # Define event callbacks
          websocket$onOpen(function(event) {
            cli::cli_alert_success("Connected to server.")
          })
          
          websocket$onMessage(function(event) {
            data <- jsonlite::fromJSON(event$data)
            event <- realtalk::Event$new(data) 
            eventlog$add(event)
          })
          
          websocket$onError(function(event) {
            cli::cli_alert_danger("WebSocket error: {event$message}")
          })
          
          websocket$onClose(function(event) {
            cli::cli_alert_success("Connection closed.")
          })
          
          # Connect to the websocket server
          websocket$connect()
          realtalk::do_later_now()
          start_time <- Sys.time()
          timeout <- 10  # 10 seconds timeout
          while (websocket$readyState() == 0L && difftime(Sys.time(), start_time, units = "secs") < timeout) {
            Sys.sleep(0.1)
            realtalk::do_later_now()
          }
          if (websocket$readyState() != 1L) {
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
          websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))

          # Flush the future queue
          realtalk::do_later_now()

          # Initialise audio buffers
          audio_out_tempdir <- fs::path_temp("audio_out")
          fs::dir_create(audio_out_tempdir)
          audio_in_buffer_file <- fs::file_temp(pattern = "audio_out_buffer", ext = "wav")

          # Background function to catch and play streamed audio out
          audio_out_outfile <- fs::file_temp()
          audio_out_errfile <- fs::file_temp()
          audio_out_bg <- callr::r_bg(
            function(audio_out_tempdir) {
              cli::cli_alert_info("Background audio out loop initiated")
              cli::cli_alert_info("Directory is {audio_out_tempdir}")

              # Audio out loop
              while (TRUE) {

                audio_files_in_buffer <- fs::dir_ls(audio_out_tempdir)
                cli::cli_alert_info("There are {length(audio_files_in_buffer)} files in the audio in buffer")
                if (length(audio_files_in_buffer) > 0) {
                  cli::cli_alert_info("Reading, concatenating, and playing audio files...")
                  purrr::map(audio_files_in_buffer, readLines) |>
                    paste0() |>
                    realtalk::play_audio_chunk()
                  cli::cli_alert_info("Deleting played files...")
                  fs::file_delete(audio_files_in_buffer)
                }
                
                Sys.sleep(0.2)
              }
            }, 
            args = list(audio_out_tempdir = audio_out_tempdir),
            stdout = audio_out_outfile,
            stderr = audio_out_errfile
          )

          # Initiate sox streaming of audio in to buffer file
          sox_bg_process <- callr::r_bg(function(audio_in_buffer_file) {
            sox_args <- c(
              "-q", # Quiet mode
              "-t", "coreaudio", "default",
              "-b", "16", # 16-bit depth
              "-r", "24000", # 24 kHz sample rate
              "-c", "1", # 1 channel (mono)
              audio_in_buffer_file
            )
            system2("sox", sox_args, stderr = FALSE)
          }, args = list(audio_in_buffer_file))

          # Wait for sox to begin working
          sox_timer <- 0
          sox_timeout <- 10
          sox_polling_interval <- 0.1
          while (TRUE) {
            if (fs::file_exists(audio_in_buffer_file)) {
              if (fs::file_size(audio_in_buffer_file) > 0) break
            }
            cli::cli_alert_info("Waiting for sox...")
            sox_timer <- sox_timer + sox_polling_interval
            if (sox_timer >= sox_timeout) {
              cli::cli_abort("Timed out waiting for sox after {sox_timer} seconds")
            }
            Sys.sleep(sox_polling_interval)
          }
          cli::cli_alert_info("sox is recording")

          # Main streaming loop
          j <- 0
          audio_in_last_processed_byte <- 0
          eventlog_streamed <- eventlog$as_tibble()
          while (TRUE) {

            # Check if shutdown signal has been sent
            if (fs::file_exists(bg_close_path)) {
              cli::cli_alert_info("Shutdown signal received")
              return(eventlog)
            }

            cli::cli_h1("Main loop iteration {j}")
            realtalk::do_later_now()

            # Check if audio in buffer has grown
            audio_in_buffer_size <- fs::file_size(audio_in_buffer_file)
            cli::cli_alert_info("Audio in buffer size is {audio_in_buffer_size}")

            # If audio in buffer has grown, stream the delta
            if (audio_in_buffer_size > audio_in_last_processed_byte) {

              # Open connection to the file and read the new binary data
              connection <- file(audio_in_buffer_file, "rb") # "rb" reads in binary mode
              seek(connection, audio_in_last_processed_byte)
              new_audio_bin <- readBin(
                connection,
                "raw",
                audio_in_buffer_size - audio_in_last_processed_byte
              )
              close(connection)

              # Encode the new audio data as base64
              new_audio_b64 <- base64enc::base64encode(new_audio_bin)

              # Send to server
              if (websocket$readyState() != 1L) {
                cli::cli_abort("Stream is not in a ready state")
              }

              # Send the audio, there is no need to commit or trigger a response as
              # this happens automatically in VAD mode
              payload <- list(
                type = jsonlite::unbox("input_audio_buffer.append"),
                audio = jsonlite::unbox(new_audio_b64)
              )
              websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))
              cli::cli_alert_info("{length(new_audio_bin)} bits streamed from audio in buffer")

              # Update the last processed position
              audio_in_last_processed_byte <- audio_in_buffer_size
            }

            # Check event log for updates
            eventlog_current <- eventlog$as_tibble()
            cli::cli_alert_info("There are {nrow(eventlog_current)} total events")
            eventlog_new <- dplyr::anti_join(eventlog_current, eventlog_streamed, by = dplyr::join_by(created_at, type, data))

            # Write new audio out events to the buffer
            new_audio <- eventlog_new |>
              dplyr::filter(type == "response.audio.delta") 
            cli::cli_alert_info("There are {nrow(new_audio)} new audio out events")
            if (nrow(new_audio) > 0) {
              new_audio <- new_audio |>
                dplyr::pull(data) |>
                dplyr::bind_rows() |>
                dplyr::pull(delta) |>
                paste0(collapse = "")
              if (stringr::str_length(new_audio) > 0) {
                audio_buffer_file <- fs::file_temp(pattern = lubridate::now() |> as.character(), tmp_dir = audio_out_tempdir)
                writeLines(new_audio, audio_buffer_file)
              }
            }

            # Update the eventlog
            eventlog_streamed <- eventlog_current

            j <- j + 1
            Sys.sleep(1)
          } # End of main streaming loop

        }, args = list(
          api_key = api_key,
          model = model,
          voice = voice,
          bg_close_path = self$bg_close_path
        )
      ) # End of callr::r_bg call for main loop
    },

    #' @description
    #'
    #' Ends live audio streaming
    #'
    #' @return No return
    stop_streaming = function() {

      # Touch the background close file
      fs::file_touch(self$bg_close_path)

      # Wait for the background process to shut down
      shutdown_timer <- 0
      shutdown_timeout <- 10
      shutdown_polling_interval <- 1
      while (self$bg_process$is_alive()) {
        cli::cli_alert_info("Shutting down stream...")
        shutdown_timer <- shutdown_timer + shutdown_polling_interval
        if (shutdown_timer >= shutdown_timeout) {
          cli::cli_abort("Timed out waiting for shutdown after {shutdown_timer} seconds")
        }
        Sys.sleep(shutdown_polling_interval)
      }

      # Retrieve the event log
      self$eventlog <- self$bg_process$get_result()

    },

    #' @description
    #'
    #' Set up the Stream object
    #'
    initialize = function() {

      # Set the background close file path
      self$bg_close_path <- fs::file_temp(pattern = "background_close_signal")

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
    #' Close the stream
    close = function() {
      realtalk::do_later_now() # Flush any pending activity before closing to prevent a warning
      self$websocket$close()
      realtalk::do_later_now()
    },

    #' @description
    #' Report the current ready state of the stream
    #' @return An integer representing the state of the connection: 0L =
    #' Connecting, 1L = Open, 2L = Closing, 3L = Closed.
    ready_state = function() {
      realtalk::do_later_now()
      self$websocket$readyState() |> as.integer()
    }
  )
)
