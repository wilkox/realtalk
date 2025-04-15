#' R6 Class Representing A Stream
#'
#' @description Represents a streaming session with the OpenAI Realtime API
#'
#' @export
Stream <- R6::R6Class("Stream",

  public = list(

    #' @field bg_process The callr background R process in which the websocket
    #' and streaming loops are instantiated
    bg_process = NULL,

    #' @field bg_close_path The path for a temporary file used to signal the
    #' background process to close
    bg_close_path = NULL,

    #' @field text_out_path The path for a temporary file containing the text
    #' output buffer from the assistant. This allows for reading of the text
    #' out stream in real time, as the event log is not generated until the
    #' stream is completed.
    text_out_path = NULL,

    #' @field audio_out_buffer_path The path for temporary file containing the
    #' audio out buffer. New audio response deltas are appended to this buffer.
    audio_out_buffer_path = NULL,

    #' @field eventlog The EventLog for the stream
    eventlog = NULL,

    #' @field log Function to write to the local log (as distinct from the
    #' stream EventLog). This is a field holding a static function, not a
    #' method, to allow the function to be passed into sub-processes.
    #'
    #' @seealso [log_path]
    log = NULL,

    #' @field log_path Path to the local log (as distinct from the stream
    #' EventLog).
    #'
    #' @seealso [log]
    log_path = NULL,

    #' @description
    #' 
    #' Return a tibble of all conversation items (i.e. text and audio messages)
    #' in the stream
    conversation = function() {

      # Capture events
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
              "role" %in% names(data$item),
              data$item$role,
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

      # Tidy up and return
      conversation <- events$message |>
        purrr::map(tibble::as_tibble) |>
        dplyr::bind_rows()
      
      return(conversation)
    },

    #' @description
    #'
    #' Return a formatted, plain-text transcript of the audio streams only
    audio_transcript = function() {
      self$conversation() |>
        dplyr::filter(medium == "audio") |>
        dplyr::mutate(role = ifelse(role == "assistant", "expert system", role)) |>
        dplyr::mutate(role = ifelse(role == "user", "doctor", role)) |>
        dplyr::mutate(item = paste0(stringr::str_to_title(role), ": ", content)) |>
        dplyr::pull(item) |>
        paste0(collapse = "\n\n")
    },

    #' @description
    #' 
    #' Print a formatted transcript of the completed stream
    transcript = function() {

      # Print a neat transcript
      cli::cli_h1("Transcript")
      cli::cli_alert_info("Messages may appear out of order due to delays in transcription")

      transcript <- self$conversation()

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
    #' Return all text messages received from the stream in real time. Useful
    #' for polling for text output while the stream is still running.
    #'
    #' @return A list of text messages received
    text_received = function() {
      return(readRDS(self$text_out_path))
    },

    #' @description
    #'
    #' Open a WebSocket streaming connection to OpenAI Realtime API and begin
    #' bidirectional text and audio streaming. The streaming loops are
    #' encapsulated in a callr subprocess, which is stored in the bg_process
    #' slot.
    #'
    #' @param api_key Your long-term OpenAI API key. Defaults to
    #' `Sys.getenv("OPENAI_API_KEY")`.
    #' @param model A character string specifying the model. Defaults to
    #' `lemur::openai_api_key()`.
    #' @param voice A character string specifying the voice to use. Defaults to
    #' "ballad".
    #' 
    #' @return No return
    #'
    start_streaming = function(
      api_key = lemur::openai_api_key(verbose = FALSE),
      model = "gpt-4o-realtime-preview-2024-12-17",
      voice = "ballad"
    ) {

      # Set signal file for stream ready
      stream_ready_path <- fs::file_temp(pattern = "stream_ready")
      self$log(glue::glue("stream_ready_path set to {stream_ready_path}"))

      # Set up the background process
      self$log("start_streaming: setting up background process for main loop")
      self$bg_process <- callr::r_bg(
        function(
          api_key,
          model,
          voice,
          bg_close_path,
          text_in_path,
          text_in_lock,
          text_out_path,
          stream_ready_path,
          audio_out_buffer_path,
          eventlog,
          log
        ) {

          log("bp_process: initalised")

          # Prioritise the main loop
          current_process <- ps::ps_handle()
          ps::ps_set_nice(current_process, value = 5L)
          log("bp_process: nice set to 5")

          # Allow passthrough of cli messages
          options(cli.message_class = "callr_message")

          # Initialise the WebSocket client
          url <- paste0("wss://api.openai.com/v1/realtime?model=", model)
          headers <- list(
            "Authorization" = paste0("Bearer ", api_key),
            "OpenAI-Beta" = "realtime=v1"
          )
          websocket <- websocket::WebSocket$new(url, header = headers, autoConnect = FALSE)
          log(glue::glue("WebSocket URL set to {url}"))
          
          # Define callback event for WebSocket open 
          websocket$onOpen(function(event) {
            cli::cli_alert_success("Websocket connected successfully")
          })
          
          # Define callback event for WebSocket receiving a message
          websocket$onMessage(function(event) {
            data <- jsonlite::fromJSON(event$data)
            event <- realtalk::Event$new(data) 
            eventlog$add(event)

            # Stream audio deltas directly into the audio out buffer as b64
            if (data$type == "response.audio.delta") {
              audio_out_buffer_con <- file(audio_out_buffer_path, "at")
              writeLines(data$delta, audio_out_buffer_con)
              close(audio_out_buffer_con)
              log("Event monitor: new audio out delta written to buffer")
            }
          })
          
          # Define callback event for WebSocket throwing an error
          websocket$onError(function(event) {
            cli::cli_alert_danger("WebSocket error: {event$message}")
          })
          
          # Define callback event for WebSocket closing
          websocket$onClose(function(event) {
            cli::cli_alert_success("WebSocket connection closed")
          })
          
          # Connect to the WebSocket server
          cli::cli_alert_info("Opening WebSocket connection")
          log("Opening WebSocket connection")
          websocket$connect()
          realtalk::do_later_now()
          start_time <- Sys.time()
          timeout <- 10  # 10 seconds timeout
          while (websocket$readyState() == 0L && difftime(Sys.time(), start_time, units = "secs") < timeout) {
            Sys.sleep(0.1)
            realtalk::do_later_now()
          }
          if (websocket$readyState() != 1L) {
            cli::cli_abort("Failed to establish WebSocket connection after {timeout} seconds")
            log("Failed to establish WebSocket connection after {timeout} seconds")
          }
          log("WebSocket connection established")

          # Send a session.update event to set:
          # - voice
          # - transcription of input audio
          cli::cli_alert_info("Setting realtime session parameters")
          log(glue::glue("Setting realtime session parameters: voice = {voice}"))
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

          # Initialise audio I/O buffers
          audio_in_buffer_file <- fs::file_temp(pattern = "audio_in_buffer", ext = "wav")
          log(glue::glue("audio_in_buffer_file is {audio_in_buffer_file}"))

          # Background function to catch and play streamed audio out
          cli::cli_alert_info("Initialising background audio out loop")
          log("Initialising background audio out loop")
          audio_out_bg <- callr::r_bg(
            function(audio_out_buffer_path, eventlog, log) {
              cli::cli_alert_info("Background audio out loop initiated")
              log("audio_out_bg: loop initiated")

              # Set priority of audio out loop to high
              current_process <- ps::ps_handle()
              ps::ps_set_nice(current_process, value = 5L)

              # Audio out loop
              current_line <- 0
              while (TRUE) {

                # Read and play new audio from buffer file
                audio_out_buffer <- readLines(audio_out_buffer_path)
                if (length(audio_out_buffer) > current_line) {
                  log(glue::glue("audio_out_bg: new audio detected in buffer (current nice = {ps::ps_get_nice()})"))
                  new_audio_b64 <- audio_out_buffer[seq(current_line + 1, length(audio_out_buffer))] |>
                    paste0(collapse = "")
                  current_line <- length(audio_out_buffer)

                  # Play new audio
                  realtalk::play_audio_chunk(new_audio_b64)
                } else {

                  # Wait for more audio to appear in the buffer, with the
                  # polling interval depending on whether the stream is
                  # reporting ongoing an ongoing audio response
                  last_audio_event <- eventlog$as_tibble() |>
                    dplyr::filter(type %in% c(
                      "response.audio.delta",
                      "response.audio.done"
                    )) |>
                    dplyr::pull(type) |>
                    tail(1)
                  if (length(last_audio_event) == 0) {
                    active_streaming <- FALSE
                  } else {
                    active_streaming <- last_audio_event == "response.audio.delta"
                  }
                  Sys.sleep(ifelse(active_streaming, 0.5, 0.05))
                }
              } # End of audio out loop
            }, 
            args = list(
              audio_out_buffer_path = audio_out_buffer_path,
              eventlog = eventlog,
              log = log
            ),
            stderr = fs::file_temp(), # These redirects are needed as keepalives for
            stdout = fs::file_temp()  # the background process
          )

          # Initiate sox streaming of audio in to buffer file
          cli::cli_alert_info("Initialising background audio in loop")
          log("Initialising background audio in loop")
          sox_bg <- callr::r_bg(
            function(audio_in_buffer_file, log) {
              sox_args <- c(
                "-q", # Quiet mode
                "-t", "coreaudio", "default",
                "-b", "16", # 16-bit depth
                "-r", "24000", # 24 kHz sample rate
                "-c", "1", # 1 channel (mono)
                audio_in_buffer_file
              )
              system2("sox", sox_args, stderr = FALSE)
            }, 
            args = list(audio_in_buffer_file, log = log),
            stderr = fs::file_temp(), # These redirects are needed as keepalives for
            stdout = fs::file_temp()  # the background process
          )

          # Wait for sox to begin working
          sox_timer <- 0
          sox_timeout <- 10
          sox_polling_interval <- 0.1
          while (TRUE) {
            if (fs::file_exists(audio_in_buffer_file)) {
              if (fs::file_size(audio_in_buffer_file) > 0) break
            }
            cli::cli_alert_info("Waiting for sox...")
            log("Waiting for audio in buffer file to be created by sox")
            sox_timer <- sox_timer + sox_polling_interval
            if (sox_timer >= sox_timeout) {
              cli::cli_abort("Timed out waiting for sox after {sox_timer} seconds")
              log(glue::glue("Timed out waiting for sox after {sox_timer} seconds"))
            }
            Sys.sleep(sox_polling_interval)
          }
          cli::cli_alert_success("Confirmed sox is recording")
          log("Audio in buffer file has been created by sox")

          # Main streaming loop
          main_loop_i <- 0
          fs::file_touch(stream_ready_path)
          audio_in_last_processed_byte <- 0
          eventlog_processed <- eventlog$as_tibble()[0, ]
          log("Commencing main streaming loop")
          while (TRUE) {

            cli::cli_h1("Main loop iteration #{main_loop_i}: {realtalk::timestamp()}")
            log(glue::glue("Main loop: iteration #{main_loop_i}"))
            realtalk::do_later_now()

            cli::cli_h2("Performing checks")
            log("Main loop: performing checks")

            # Check if shutdown signal has been sent
            if (fs::file_exists(bg_close_path)) {
              cli::cli_alert_info("Shutdown signal received")
              log("Main loop: shutdown signal received")
              break
            } else {
              cli::cli_alert_success("No shutdown signal received")
              log("Main loop: no shutdown signal received")
            }

            # Check that the audio I/O background processes are running
            if (! audio_out_bg$is_alive()) {
              cli::cli_alert_danger("audio_out_bg has died, aborting and returning audio_out_bg for analysis")
              log("Main loop: audio_out_bg has died, aborting")
              return(audio_out_bg)
            } else {
              cli::cli_alert_success("audio_out_bg is running")
              log("Main loop: audio_out_bg is running")
            }
            if (! sox_bg$is_alive()) {
              cli::cli_alert_danger("sox_bg has died, aborting and returning sox_bg for analysis")
              log("Main loop: sox_bg has died, aborting")
              return(sox_bg)
            } else {
              cli::cli_alert_success("sox_bg is running")
              log("Main loop: sox_bg is running")
            }

            # Check that stream is in a ready state
            if (websocket$readyState() != 1L) {

              cli::cli_alert_danger("WebSocket is not in a ready state, aborting and returning eventlog for analysis")
              log("Main loop: WebSocket is not in a ready state, aborting")
              log(glue::glue("Main loop: WebSocket state is {websocket$readyState()}"))
              eventlog_dump <- fs::file_temp(pattern = "eventlog", ext = "rds")
              log(glue::glue("Main loop: dumping event log to {eventlog_dump} for analysis"))
              saveRDS(eventlog, eventlog_dump)
              return(eventlog)
            } else {
              cli::cli_alert_success("WebSocket is in a ready state")
              log("Main loop: WebSocket is in a ready state")
            }

            cli::cli_h2("Checking event log")
            log("Main loop: checking event log")

            # Check event log for updates
            eventlog_current <- eventlog$as_tibble()

            # If there are no events left (because the stream is still
            # initialising), wait for some events to accumulate
            if (nrow(eventlog_current) == 0) {
              first_event_elapsed <- 0
              first_event_timeout <- 10
              while (nrow(eventlog_current) == 0) {
                log("Main loop: waiting for first event to appear in event log")
                eventlog_current <- eventlog$as_tibble()
                first_event_elapsed <- first_event_elapsed + 1
                if (first_event_elapsed >= first_event_timeout) break
                Sys.sleep(1)
              }
              log(glue::glue("Main loop: waited {first_event_timeout} seconds but not events appeared in eventlog"))
              cli::cli_abort("Waited {first_event_timeout} seconds but not events appeared in eventlog")
            }

            cli::cli_alert_info("There are {nrow(eventlog_current)} events ({nrow(eventlog_current) - nrow(eventlog_processed)} new)")
            log(glue::glue("Main loop: there are {nrow(eventlog_current)} events ({nrow(eventlog_current) - nrow(eventlog_processed)} new)"))
            eventlog_new <- dplyr::anti_join(eventlog_current, eventlog_processed, by = dplyr::join_by(created_at, type, data))

            # Check if API is currently mid-response
            log("Main loop: checking whether API is currently responding")
            last_response_type <- eventlog$as_tibble() |>
              dplyr::filter(type %in% c("response.created", "response.done")) |>
              dplyr::pull(type) |>
              tail(1)
            if (length(last_response_type) > 0) {
              is_responding <- last_response_type == "response.created"
            } else {
              is_responding <- FALSE
            }
            log(glue::glue("Main loop: is_responding is {is_responding}"))

            # Only process text in buffers if the API is not currently returning a response
            cli::cli_h2("Processing text in buffers")
            log("Main loop: processing text in buffers")
            if (is_responding) {
              cli::cli_alert_info("API is currently responding, will buffer text in until response complete")
              log("Main loop: API is currently responding, will buffer text in until response complete")
            } else {
              
              # Check if there is anything in the text in buffer
              fs::file_touch(text_in_lock)
              text_in_buffer <- readRDS(text_in_path)
              fs::file_delete(text_in_lock)
              cli::cli_alert_info("Text in buffer has {nrow(text_in_buffer)} items")
              log(glue::glue("Main loop: text in buffer has {nrow(text_in_buffer)} items"))

              # Stream any items in the buffer
              if (nrow(text_in_buffer) > 0) {
                cli::cli_alert_info("Streaming new text in items")
                log("Main loop: streaming new text in items")

                # Send the text
                for (i in seq_len(nrow(text_in_buffer))) {
                  text <- stringr::str_trim(text_in_buffer$text[i])
                  if (! checkmate::qtest(text, "S")) next
                  if (text == "") next
                  log(glue::glue("Main loop: about to send text in line ->{text}<-"))
                  payload <- list(
                    type = jsonlite::unbox("conversation.item.create"),
                    item = list(
                      type = jsonlite::unbox("message"),
                      role = jsonlite::unbox(text_in_buffer$role[i]),
                      content = list(list(
                        type = jsonlite::unbox("input_text"),
                        text = jsonlite::unbox(text)
                      ))
                    )
                  )
                  websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))

                  # Trigger a response, if needed
                  if (text_in_buffer$trigger_response[i]) {
                    payload <- list(type = jsonlite::unbox("response.create"))
                    websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))
                    log("Main loop: sent response.create signal")
                  }
                }

                # Wipe the text in buffer
                fs::file_touch(text_in_lock)
                text_in <- tibble::tibble(role = character(0), trigger_response = logical(0), text = character(0))
                saveRDS(text_in, text_in_path)
                fs::file_delete(text_in_lock)

                cli::cli_alert_success("Finished streaming system text in")
                log("Main loop: finished streaming new text in")
              }

            }

            cli::cli_h2("Processing audio in buffer")
            log("Main loop: processing audio in buffer")

            # Check if audio in buffer has grown
            audio_in_buffer_size <- fs::file_size(audio_in_buffer_file) |> as.numeric()
            cli::cli_alert_info("Audio in buffer size is {audio_in_buffer_size} ({audio_in_buffer_size - audio_in_last_processed_byte} new bytes)")
            log(glue::glue("Main loop: audio in buffer size is {audio_in_buffer_size} ({audio_in_buffer_size - audio_in_last_processed_byte} new bytes)"))

            # If audio in buffer has grown, stream the delta
            if (audio_in_buffer_size > audio_in_last_processed_byte) {

              cli::cli_alert_info("Streaming new audio in")
              log("Main loop: streaming new audio in")

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

              # Send the audio, there is no need to commit or trigger a response as
              # this happens automatically in VAD mode
              payload <- list(
                type = jsonlite::unbox("input_audio_buffer.append"),
                audio = jsonlite::unbox(new_audio_b64)
              )
              websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))
              cli::cli_alert_info("{length(new_audio_bin)} bits streamed from audio in buffer")
              log(glue::glue("Main loop: {length(new_audio_bin)} bits streamed from audio in buffer"))

              # Update the last processed position
              audio_in_last_processed_byte <- audio_in_buffer_size

              cli::cli_alert_success("Finished streaming new audio in")
              log("Main loop: finished streaming new audio in")
            }

            cli::cli_h2("Processing text out from event log")
            log("Main loop: processing text out from event log")

            # Write new text out events to the buffer
            new_text_out <- eventlog_new |>
              dplyr::filter(type == "response.text.done")
            cli::cli_alert_info("There are {nrow(new_text_out)} new text out events")
            log(glue::glue("Main loop: there are {nrow(new_text_out)} new text out events"))
            if (nrow(new_text_out) > 0) {
              new_text_out <- new_text_out |>
                dplyr::pull(data) |>
                dplyr::bind_rows() |>
                dplyr::pull(text) |>
                as.list()
              text_received <- readRDS(text_out_path)
              text_received <- c(text_received, new_text_out)
              saveRDS(text_received, text_out_path)
            }
            cli::cli_alert_success("New text out events written to buffer")
            log("Main loop: new text out events written to buffer")

            # Update the eventlog
            eventlog_processed <- eventlog_current

            # Iterate the loop
            main_loop_i <- main_loop_i + 1

            # Flush pending futures
            realtalk::do_later_now()
            Sys.sleep(0.2)

          } # End of main streaming loop

          cli::cli_alert_info("Main streaming loop ended")
          log("Main loop ended")
          return(eventlog)

        }, 
        args = list(
          api_key = api_key,
          model = model,
          voice = voice,
          bg_close_path = self$bg_close_path,
          text_in_path = private$text_in_path,
          text_in_lock = private$text_in_lock,
          text_out_path = self$text_out_path,
          stream_ready_path = stream_ready_path,
          audio_out_buffer_path = self$audio_out_buffer_path,
          eventlog = self$eventlog,
          log = self$log
        ),
        stderr = fs::file_temp(), # These redirects are needed as keepalives for
        stdout = fs::file_temp()  # the background process
      ) # End of callr::r_bg call for main loop

      # Wait until stream initiation signal received
      stream_ready_timer <- 0
      stream_ready_timeout <- 10
      stream_ready_polling_interval <- 1
      cli::cli_alert_info("Initialising stream...")
      self$log("start_streaming: waiting for confirmation that stream is ready")
      while (! fs::file_exists(stream_ready_path)) {
        stream_ready_timer <- stream_ready_timer + stream_ready_polling_interval
        if (stream_ready_timer >= stream_ready_timeout) {
          self$log(glue::glue("start_streaming: timed out waiting for stream to intialise after {stream_ready_timer} seconds"))
          cli::cli_abort("Timed out waiting for stream to intialise after {stream_ready_timer} seconds")
        }
        Sys.sleep(stream_ready_polling_interval)
      }
      cli::cli_alert_success("Stream initialised")
      self$log("start_streaming: stream initialised")
    },

    #' @description
    #'
    #' Ends live audio streaming
    #'
    #' @return No return
    stop_streaming = function() {

      self$log("stop_streaming: called")

      # Touch the background close file
      fs::file_touch(self$bg_close_path)

      # Wait for the background process to shut down
      shutdown_timer <- 0
      shutdown_timeout <- 30
      shutdown_polling_interval <- 1
      cli::cli_alert_info("Shutting down stream...")
      while (self$bg_process$is_alive()) {
        shutdown_timer <- shutdown_timer + shutdown_polling_interval
        if (shutdown_timer >= shutdown_timeout) {
          self$log(glue::glue("stop_streaming: timed out waiting for shutdown after {shutdown_timer} seconds"))
          cli::cli_abort("Timed out waiting for shutdown after {shutdown_timer} seconds")
        }
        Sys.sleep(shutdown_polling_interval)
      }
      cli::cli_alert_success("Stream shut down")
      self$log("stop_streaming: stream shut down")

      # Dump the event log
      self$eventlog <- self$bg_process$get_result()
      eventlog_dump <- fs::file_temp(pattern = "eventlog", ext = "rds")
      saveRDS(self$eventlog, eventlog_dump)
      self$log(glue::glue("stop_streaming: event log dumped to {eventlog_dump}"))

    },

    #' @description
    #'
    #' Set up the Stream object
    #'
    initialize = function() {

      # Set up the local log
      self$log_path <- fs::file_temp(pattern = "log", ext = "txt")
      fs::file_create(self$log_path)
      self$log <- function(entry) {
        if (! checkmate::qtest(entry, "S")) {
          cli::cli_abort("Attempted to log a non-text entry")
          print(entry)
        }
        entry <- glue::glue("[{realtalk::timestamp()}] {entry}")
        connection <- file(self$log_path, "at") # "at" is appending in text mode
        writeLines(entry, connection)
        close(connection)
      }
      self$log("Stream object initialised")

      # Set the background close file path
      self$bg_close_path <- fs::file_temp(pattern = "background_close_signal")
      self$log(glue::glue("bg_close_path is {self$bg_close_path}"))

      # Set the text input buffer file paths
      private$text_in_path <- fs::file_temp(pattern = "text_in_buffer", ext = "rds")
      text_in <- tibble::tibble(role = character(0), trigger_response = logical(0), text = character(0))
      saveRDS(text_in, private$text_in_path)
      self$log(glue::glue("text_in_path is {private$system_text_in_path}"))
      private$text_in_lock <- fs::file_temp(pattern = "text_in_lock")
      self$log(glue::glue("text_in_lock is {private$text_in_lock}"))

      # Set the text output buffer file path
      self$text_out_path <- fs::file_temp(pattern = "text_out", ext = "rds")
      text_received <- list()
      saveRDS(text_received, self$text_out_path)
      self$log(glue::glue("text_out_path is {self$text_out_path}"))

      # Set the audio out buffer path and initialise the buffer file
      self$audio_out_buffer_path <- fs::file_temp(pattern = "audio_out", ext = "txt")
      fs::file_create(self$audio_out_buffer_path)
      self$log(glue::glue("audio_out_buffer_path is {self$audio_out_buffer_path}"))

      cli::cli_alert_success("Stream created")
      cli::cli_alert_info("Call {.fun start_streaming} to connect to the API and commence audio and text streaming")

      # Set up the logfile split
      while (! fs::file_exists(self$log_path) ) { Sys.sleep(0.01) }
      cli::cli_alert_info("Logfile split should open below")
      cli::cli_alert_info("Logfile path is:")
      cli::cli_text(self$log_path)
      system2("tmux", args = c(
        "split-window",
        "-v",
        "-p", "33",
        glue::glue("tail -f {self$log_path}"),
        ";", "tmux", "last-pane"
      ))

      # Initialise the event log
      self$eventlog <- realtalk::EventLog$new()

    },

    #' Send a text message to the stream
    #'
    #' @param text A character string to send.
    #' @param role The role, either "user" (default) or "system".
    #' @param trigger_response Whether to send a "response.create" signal to
    #' trigger a response. Defaults to FALSE
    #'
    send_text = function(text, role = "user", trigger_response = FALSE) {

      # Wait for control of the text in buffer file
      lock_elapsed <- 0
      lock_timeout <- 10
      while (fs::file_exists(private$text_in_lock)) {
        lock_elapsed <- lock_elapsed + 1
        if (lock_elapsed >= lock_timeout) {
          cli::cli_abort("Timed out waiting for text in buffer file to unlock after {lock_elapsed} seconds")
        }
        Sys.sleep(1)
      }

      # Lock the text in buffer and append the text
      fs::file_touch(private$text_in_lock)
      text_in <- readRDS(private$text_in_path)
      new_text <- tibble::tibble(role = role, trigger_response = trigger_response, text = text)
      text_in <- dplyr::bind_rows(text_in, new_text)
      saveRDS(text_in, private$text_in_path)
      fs::file_delete(private$text_in_lock)

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
  ),

  private = list(

    # Path to the file with the text input buffer (serialised tibble)
    text_in_path = NULL,

    # Lockfile for the text input buffer
    text_in_lock = NULL
  )
)
