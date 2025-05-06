#' R6 Class Representing A Stream
#'
#' @description Represents a streaming session with the OpenAI Realtime API
#'
#' @export
#' @importFrom lifecycle deprecate_soft
Stream <- R6::R6Class("Stream",

  public = list(

    #' @field eventlog The EventLog for the stream, an EventLog object
    eventlog = NULL,

    #' Add an entry to the log
    #'
    #' @param entry The entry to be written to the log
    log = function(entry) {
      # Since we can't modify private methods after initialization,
      # we implement the logging logic directly here
      if (! checkmate::qtest(entry, "S")) {
        cli::cli_abort("Attempted to log a non-text entry")
        print(entry)
      }
      entry <- glue::glue("[{realtalk::format_datetime()}] {entry}")
      connection <- file(private$log_path, "at") # "at" is appending in text mode
      writeLines(entry, connection)
      close(connection)
    },

    #' Wait until the current or next response is finished
    #'
    #' @param timeout Maximum time in seconds to wait before returning an
    #' error, defaults to 60
    wait_for_response = function(timeout = 60) {

      n_events_processed <- self$eventlog$as_tibble() |>
        nrow()
      start_time <- lubridate::now()
      while (TRUE) {
        if (lubridate::interval(start_time, lubridate::now()) / lubridate::dseconds() >= timeout) {
          cli::cli_abort("{.fun wait_for_response} timed out waiting for response after {timeout} seconds")
        }
        eventlog <- self$eventlog$as_tibble()
        new_types <- eventlog[seq(n_events_processed + 1, nrow(eventlog)), ]$type
        if ("response.done" %in% new_types) {
          break
        } else {
          n_events_processed <- nrow(eventlog)
          Sys.sleep(0.1)
        }
      }
    },

    #' Return all text and transcribed audio messages in the stream
    #'
    #' @return A tibble of all text and transcribed audio messages in the stream
    conversation = function() {

      # Capture events
      events <- self$eventlog$as_tibble()

      # If there are no events, return an empty tibble
      if (nrow(events) == 0) {
        return(tibble::tibble(role = character(), medium = character(), content = character()))
      }

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

      # If there are no printable events, return an empty tibble
      if (nrow(events) == 0) {
        return(tibble::tibble(role = character(), medium = character(), content = character()))
      }

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

    #' Print a formatted transcript of the stream
    #' 
    #' @return NULL, invisibly
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

    #' Start the streaming session
    #'
    #' Opens a WebSocket streaming connection to the OpenAI Realtime API and
    #' starts bidirectional text and audio streaming. The streaming loops are
    #' managed by background processes. 
    #'
    #' @param api_key Your long-term OpenAI API key. Defaults to
    #' `openai_api_key(verbose = FALSE)`.
    #' @param model A string specifying the model. Defaults to
    #' `"gpt-4o-realtime-preview-2024-12-17"`.
    #' @param voice A string specifying the voice to use. Defaults to "ballad".
    #' 
    start_streaming = function(
      api_key = openai_api_key(verbose = FALSE),
      model = "gpt-4o-realtime-preview-2024-12-17",
      voice = "ballad"
    ) {

      # Set up the background process
      self$log("start_streaming: setting up background process for main loop")
      # Create temporary files for stderr and stdout with descriptive names
      bg_stderr_file <- fs::file_temp(pattern = "bg_stderr_", ext = "log")
      bg_stdout_file <- fs::file_temp(pattern = "bg_stdout_", ext = "log")
      self$log(glue::glue("start_streaming: using stderr file: {bg_stderr_file}"))
      self$log(glue::glue("start_streaming: using stdout file: {bg_stdout_file}"))
      
      # Store the paths for later reference
      private$bg_stderr_file <- bg_stderr_file
      private$bg_stdout_file <- bg_stdout_file
      
      # Initialize the background process with redirected output to files
      private$bg_process <- callr::r_bg(
        function(
          api_key,
          model,
          voice,
          bg_close_path,
          text_in_path,
          status_message_path,
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
            tryCatch({
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
            }, error = function(e) {
              log(glue::glue("WebSocket onMessage error: {e$message}"))
              log(glue::glue("Error occurred while processing message: {event$data}"))
              cli::cli_alert_danger("Error in WebSocket message handler: {e$message}")
            })
          })
          
          # Define callback event for WebSocket throwing an error
          websocket$onError(function(event) {
            cli::cli_alert_danger("WebSocket error: {event$message}")
            log("WebSocket error: {event$message}")
            # Remove ready signal with lock protection
            stream_ready_lock_path <- fs::path(stream_ready_path, ext = "lock")
            stream_ready_lock <- filelock::lock(stream_ready_lock_path, timeout = 10000)
            if (!is.null(stream_ready_lock)) {
              fs::file_delete(stream_ready_path)
              filelock::unlock(stream_ready_lock)
              log("Removed stream ready signal due to WebSocket error")
            }
          })
          
          # Define callback event for WebSocket closing
          websocket$onClose(function(event) {
            cli::cli_alert_success("WebSocket connection closed")
            # Remove ready signal with lock protection
            stream_ready_lock_path <- fs::path(stream_ready_path, ext = "lock")
            stream_ready_lock <- filelock::lock(stream_ready_lock_path, timeout = 10000)
            if (!is.null(stream_ready_lock)) {
              fs::file_delete(stream_ready_path)
              filelock::unlock(stream_ready_lock)
              log("Removed stream ready signal due to WebSocket closing")
            }
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

          # Wait for the first event to appear in the eventlog
          eventlog_current <- eventlog$as_tibble()
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
            log(glue::glue("Main loop: waited {first_event_timeout} seconds but no events appeared in eventlog"))
            cli::cli_abort("Waited {first_event_timeout} seconds but no events appeared in eventlog")
          }

          # Main streaming loop
          main_loop_i <- 0
          
          # Signal that the stream is ready using filelock to ensure thread safety
          stream_ready_lock_path <- fs::path(stream_ready_path, ext = "lock")
          stream_ready_lock <- filelock::lock(stream_ready_lock_path, timeout = 10000)
          if (is.null(stream_ready_lock)) {
            log("Failed to acquire lock for stream_ready_path")
            cli::cli_abort("Failed to acquire lock for stream ready signal")
          }
          fs::file_touch(stream_ready_path)
          filelock::unlock(stream_ready_lock)
          log("Set stream ready signal with lock protection")
          
          audio_in_last_processed_byte <- 0
          eventlog_processed <- eventlog$as_tibble()[0, ]
          n_response_done_status_message <- -1
          log("Commencing main streaming loop")
          while (TRUE) {

            cli::cli_h1("Main loop iteration #{main_loop_i}: {realtalk::format_datetime()}")
            log(glue::glue("Main loop: iteration #{main_loop_i}"))
            realtalk::do_later_now()

            # Every 10th iteration, compact the event log
            if (main_loop_i %% 10 == 0) {
              log("Main loop: compacting event log")
              eventlog$compact()
            }

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

            # Process status message
            # The status message is sent under the following conditions:
            # - The status message is non-null and non-blank
            # - The API is not currently responding
            # - The number of response.done events has changed since the last time the status message was sent
            cli::cli_h2("Processing status message")
            log("Main loop: processing status message")

            # Get a lock on the status message file
            lck <- filelock::lock(fs::path(status_message_path, ext = "lock"), timeout = 60000)
            if (is.null(lck)) {
              log("Failed to acquire lock on status message file")
              cli::cli_abort("Failed to acquire lock on status message file")
            }

            # Load the status message
            status_message <- readRDS(status_message_path)

            # Relinquish the lock
            lck <- filelock::unlock(lck)

            log(glue::glue("Main loop: status_message is {status_message}"))
            if (is.null(status_message)) {
              cli::cli_alert_info("Status message is null, skipping")
              log("Main loop: status message is null, skipping")
            } else if (is.na(status_message) | status_message == "") {
              cli::cli_alert_info("Status message is NA/blank, skipping")
              log("Main loop: status message is NA/blank, skipping")
            } else if (is_responding) {
              cli::cli_alert_info("API is currently responding, skipping status message")
              log("Main loop: API is currently responding, skipping status message")
            } else {

              n_response_done <- eventlog$as_tibble() |>
                dplyr::filter(type == "response.done") |>
                nrow()
              
              if (n_response_done > n_response_done_status_message) {
                  cli::cli_alert_info("Sending status message")
                  log("Main loop: sending status message")
                  payload <- list(
                    type = jsonlite::unbox("conversation.item.create"),
                    item = list(
                      type = jsonlite::unbox("message"),
                      role = jsonlite::unbox("system"),
                      content = list(list(
                        type = jsonlite::unbox("input_text"),
                        text = jsonlite::unbox(status_message)
                      ))
                    )
                  )
                  websocket$send(jsonlite::toJSON(payload, auto_unbox = FALSE))

                n_response_done_status_message <- n_response_done
                } else {
                cli::cli_alert_info("No new response since status message last sent, skipping status message")
                log("Main loop: no new response since status message last sent, skipping status message")
              }
            }

            # Only process text in buffers if the API is not currently returning a response
            cli::cli_h2("Processing text in buffers")
            log("Main loop: processing text in buffers")
            if (is_responding) {
              cli::cli_alert_info("API is currently responding, will buffer text in until response complete")
              log("Main loop: API is currently responding, will buffer text in until response complete")
            } else {
              
              # Get a lock on the text in buffer file
              lck <- filelock::lock(fs::path(text_in_path, ext = "lock"), timeout = 60000)
              if (is.null(lck)) {
                log("Failed to acquire lock on text in buffer file")
                cli::cli_abort("Failed to acquire lock on text in buffer file")
              }
              
              # Read the text in buffer
              text_in_buffer <- readRDS(text_in_path)
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
                text_in <- tibble::tibble(role = character(0), trigger_response = logical(0), text = character(0))
                saveRDS(text_in, text_in_path)
                
                # Relinquish the lock
                lck <- filelock::unlock(lck)

                cli::cli_alert_success("Finished streaming system text in")
                log("Main loop: finished streaming new text in")
              } else {
                # Relinquish the lock if we didn't process anything
                lck <- filelock::unlock(lck)
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
          bg_close_path = private$bg_close_path,
          text_in_path = private$text_in_path,
          status_message_path = private$status_message_path,
          stream_ready_path = private$stream_ready_path,
          audio_out_buffer_path = private$audio_out_buffer_path,
          eventlog = self$eventlog,
          log = self$log
        ),
        stderr = bg_stderr_file, # Use the files we created for better debugging
        stdout = bg_stdout_file
      ) # End of callr::r_bg call for main loop

      # Wait until stream initiation signal received
      stream_ready_timer <- 0
      stream_ready_timeout <- 60
      stream_ready_polling_interval <- 1
      cli::cli_alert_info("Initialising stream...")
      self$log("start_streaming: waiting for confirmation that stream is ready")
      
      # Track background process status
      stream_ready_lock_path <- fs::path(private$stream_ready_path, ext = "lock")
      while (TRUE) {
        # Check stream ready status with proper locking
        is_ready <- FALSE
        ready_lock <- filelock::lock(stream_ready_lock_path, timeout = 1000)
        if (!is.null(ready_lock)) {
          is_ready <- fs::file_exists(private$stream_ready_path)
          filelock::unlock(ready_lock)
        }
        
        if (is_ready) {
          self$log("start_streaming: detected stream ready signal")
          break
        }
        stream_ready_timer <- stream_ready_timer + stream_ready_polling_interval
        
        # Check if the background process is still alive
        if (! private$bg_process$is_alive()) {
          # Get the error information directly from the log files we created
          self$log("start_streaming: background process died unexpectedly")
          
          # Get exit status 
          exit_status <- tryCatch({
            private$bg_process$get_exit_status()
          }, error = function(e) {
            self$log(glue::glue("start_streaming: could not get exit status: {e$message}"))
            return("unknown")
          })
          
          self$log(glue::glue("start_streaming: background process exit status: {exit_status}"))
          
          # Read stderr and stdout from files
          stderr_output <- tryCatch({
            if (fs::file_exists(private$bg_stderr_file)) {
              stderr <- readLines(private$bg_stderr_file, warn = FALSE)
              if (length(stderr) > 0) {
                paste(stderr, collapse = "\n")
              } else {
                "No stderr output"
              }
            } else {
              "stderr file does not exist"
            }
          }, error = function(e) {
            self$log(glue::glue("start_streaming: error reading stderr file: {e$message}"))
            return("Error reading stderr")
          })
          
          stdout_output <- tryCatch({
            if (fs::file_exists(private$bg_stdout_file)) {
              stdout <- readLines(private$bg_stdout_file, warn = FALSE)
              if (length(stdout) > 0) {
                paste(stdout, collapse = "\n")
              } else {
                "No stdout output"
              }
            } else {
              "stdout file does not exist"
            }
          }, error = function(e) {
            self$log(glue::glue("start_streaming: error reading stdout file: {e$message}"))
            return("Error reading stdout")
          })
          
          # Log the outputs
          self$log(glue::glue("start_streaming: stderr output: {stderr_output}"))
          self$log(glue::glue("start_streaming: stdout output: {stdout_output}"))
          
          # Try to determine if this is an API issue
          api_error <- FALSE
          if (grepl("OpenAI API", stderr_output, fixed = TRUE) || 
              grepl("OpenAI API", stdout_output, fixed = TRUE) ||
              grepl("Authorization", stderr_output, fixed = TRUE) ||
              grepl("WebSocket", stderr_output, fixed = TRUE)) {
            api_error <- TRUE
          }
          
          # Build appropriate error message
          if (api_error) {
            cli::cli_abort(c(
              "Background stream process terminated unexpectedly - API connection issue",
              "i" = "Exit status: {exit_status}",
              "i" = "Please check your OpenAI API key and account status",
              "i" = "The OpenAI Realtime API may be experiencing issues",
              "x" = "Error details: {stderr_output}"
            ))
          } else {
            cli::cli_abort(c(
              "Background stream process terminated unexpectedly",
              "i" = "Exit status: {exit_status}",
              "i" = "Check stderr and stdout for details",
              "i" = "If no useful information is available, try running again with debug logging",
              "i" = "stderr: {stderr_output}",
              "i" = "stdout: {stdout_output}"
            ))
          }
        }
        
        # Check for timeout
        if (stream_ready_timer >= stream_ready_timeout) {
          # Attempt to collect additional diagnostic information
          api_status <- tryCatch({
            "OpenAI API status could not be determined"
          }, error = function(e) {
            "Failed to check API status"
          })
          
          self$log(glue::glue("start_streaming: timed out waiting for stream to initialise after {stream_ready_timeout} seconds"))
          self$log(glue::glue("start_streaming: API status: {api_status}"))
          
          # Force kill the background process if it's still running
          if (private$bg_process$is_alive()) {
            private$bg_process$kill()
            self$log("start_streaming: killed background process")
          }
          
          cli::cli_abort(c(
            "Timed out waiting for stream to initialise after {stream_ready_timeout} seconds",
            "i" = "API status: {api_status}",
            "i" = "Try increasing the timeout or check your internet connection",
            "i" = "The stream may be experiencing rate limiting from OpenAI",
            "i" = "Check for any error messages in the preceding output"
          ))
        }
        
        Sys.sleep(stream_ready_polling_interval)
      }
      
      cli::cli_alert_success("Stream initialised")
      self$log("start_streaming: stream initialised")
    },

    #' Report whether the stream is ready
    #'
    #' @return A logical value indicating if the stream is ready to use
    is_ready = function() {
      # Check stream ready status with proper locking
      stream_ready_lock_path <- fs::path(private$stream_ready_path, ext = "lock")
      ready_lock <- filelock::lock(stream_ready_lock_path, timeout = 1000)
      
      if (is.null(ready_lock)) {
        self$log("is_ready: failed to acquire lock to check stream ready status")
        return(FALSE)
      }
      
      # Check if ready file exists
      is_ready <- fs::file_exists(private$stream_ready_path)
      filelock::unlock(ready_lock)
      
      return(is_ready)
    },

    #' Stop the streaming session
    #'
    #' Disconnects from the API and shuts down background I/O processes.
    #' 
    #' @return NULL, invisibly
    stop_streaming = function() {

      self$log("stop_streaming: called")

      # Touch the background close file
      fs::file_touch(private$bg_close_path)

      # Wait for the background process to shut down
      shutdown_timer <- 0
      shutdown_timeout <- 30
      shutdown_polling_interval <- 1
      cli::cli_alert_info("Shutting down stream...")
      while (private$bg_process$is_alive()) {
        shutdown_timer <- shutdown_timer + shutdown_polling_interval
        if (shutdown_timer >= shutdown_timeout) {
          self$log(glue::glue("stop_streaming: timed out waiting for shutdown after {shutdown_timer} seconds"))
          cli::cli_abort("Timed out waiting for shutdown after {shutdown_timer} seconds")
        }
        Sys.sleep(shutdown_polling_interval)
      }
      cli::cli_alert_success("Stream shut down")
      self$log("stop_streaming: stream shut down")

      # Remove the stream ready file with proper locking
      stream_ready_lock_path <- fs::path(private$stream_ready_path, ext = "lock")
      ready_lock <- filelock::lock(stream_ready_lock_path, timeout = 10000)
      if (!is.null(ready_lock)) {
        if (fs::file_exists(private$stream_ready_path)) {
          fs::file_delete(private$stream_ready_path)
          self$log("stop_streaming: removed stream ready signal with lock protection")
        }
        filelock::unlock(ready_lock)
      } else {
        self$log("stop_streaming: could not acquire lock to remove stream ready signal")
      }

    },

    #' Set up a new Stream
    #'
    #' @param tmux_split Logical, whether to open the log file in a new tmux
    #' split. Defaults to FALSE.
    initialize = function(tmux_split = FALSE) {

      # Set up the local log file
      private$log_path <- fs::file_temp(pattern = "log", ext = "txt")
      fs::file_create(private$log_path)
      
      # Since we can't modify the private$log_impl function directly due to locked bindings,
      # we'll set the private path variable instead and use it in the log implementation
      
      # Log initialization
      self$log("Stream object initialised")

      # Set signal file for stream ready
      private$stream_ready_path <- fs::file_temp(pattern = "stream_ready")
      self$log(glue::glue("stream_ready_path set to {private$stream_ready_path}"))

      # Set the background close file path
      private$bg_close_path <- fs::file_temp(pattern = "background_close_signal")
      self$log(glue::glue("bg_close_path is {private$bg_close_path}"))

      # Set the text input buffer file path
      private$text_in_path <- fs::file_temp(pattern = "text_in_buffer", ext = "rds")
      text_in <- tibble::tibble(role = character(0), trigger_response = logical(0), text = character(0))
      saveRDS(text_in, private$text_in_path)
      self$log(glue::glue("text_in_path is {private$text_in_path}"))

      # Set the status message buffer file path
      private$status_message_path <- fs::file_temp(pattern = "status_message", ext = "rds")
      status_message <- NA_character_
      saveRDS(status_message, private$status_message_path)
      self$log(glue::glue("status_message_path is {private$status_message_path}"))

      # Set the audio out buffer path and initialise the buffer file
      private$audio_out_buffer_path <- fs::file_temp(pattern = "audio_out", ext = "txt")
      fs::file_create(private$audio_out_buffer_path)
      self$log(glue::glue("audio_out_buffer_path is {private$audio_out_buffer_path}"))

      cli::cli_alert_success("Stream created")
      cli::cli_alert_info("Call {.fun start_streaming} to connect to the API and commence audio and text streaming")

      # Set up the logfile
      while (! fs::file_exists(private$log_path) ) { Sys.sleep(0.01) }
      cli::cli_alert_info("Logfile path is:")
      cli::cli_text(private$log_path)

      # Set up the tmux split
      if (tmux_split) {
        cli::cli_alert_info("Logfile split should open below")
        system2("tmux", args = c(
          "split-window",
          "-v",
          "-p", "33",
          glue::glue("tail -f {private$log_path}"),
          ";", "tmux", "last-pane"
        ))
      }

      # Initialise the event log
      self$eventlog <- EventLog$new()

    },

    #' Send a text message to the stream
    #'
    #' @param text A string to send
    #' @param role The role, either "user" (default) or "system"
    #' @param trigger_response Whether to trigger a response from the API.
    #' Defaults to FALSE
    send_text = function(text, role = "user", trigger_response = FALSE) {

      # Get a lock on the text input buffer file
      lck <- filelock::lock(fs::path(private$text_in_path, ext = "lock"), timeout = 60000)
      if (is.null(lck)) {
        self$log("Failed to acquire lock on text input buffer file")
        cli::cli_abort("Failed to acquire lock on text input buffer file")
      }
      
      # We have the lock, now operate on the file
      tryCatch({
        text_in <- readRDS(private$text_in_path)
        new_text <- tibble::tibble(role = role, trigger_response = trigger_response, text = text)
        text_in <- dplyr::bind_rows(text_in, new_text)
        saveRDS(text_in, private$text_in_path)
      }, error = function(e) {
        self$log(glue::glue("Error while processing text buffer: {e$message}"))
        cli::cli_abort("Error while processing text buffer: {e$message}")
      }, finally = {
        # Relinquish the lock
        lck <- filelock::unlock(lck)
      })
    },

    #' Set the status message
    #'
    #' The status message is sent (as a text message with the 'system' role)
    #' after each assistant audio output message.
    #'
    #' @param status_message String, the status message to be set.
    set_status_message = function(status_message) {
      checkmate::qassert(status_message, "s1")

      # Get a lock on the status message file
      lck <- filelock::lock(fs::path(private$status_message_path, ext = "lock"), timeout = 60000)
      if (is.null(lck)) {
        log("Failed to acquire lock to update status message file")
        cli::cli_abort("Failed to acquire to update status message file")
      }

      # Write the status message
      saveRDS(status_message, private$status_message_path)

      # Relinquish the lock
      lck <- filelock::unlock(lck)

      # Update the status message field
      private$status_message <- status_message
    }
  ),

  private = list(

    # The callr background R process in which the websocket and streaming loops
    # are instantiated
    bg_process = NULL,

    # Path for file used to signal the background process to close
    bg_close_path = NULL,

    # Path to the file with the text input buffer (serialised tibble)
    text_in_path = NULL,

    # Path to file containing the audio out buffer. New audio response deltas
    # are appended to this buffer.
    audio_out_buffer_path = NULL,

    # Path to file used to signal that the stream is ready
    stream_ready_path = NULL,

    # A message which will be passed (as a system text message) after each
    # model audio output finishes
    status_message = NULL,

    # Path to file in which the status message will be buffered
    status_message_path = NULL,

    # Path to the local log file (as distinct from the stream EventLog)
    log_path = NULL,
    
    # Path to the stdout and stderr files for the background process
    bg_stdout_file = NULL,
    bg_stderr_file = NULL

  )
)
