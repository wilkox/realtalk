# Set up session
load_all()
stream <- Stream$new()
eventlog <- stream$eventlog$as_tibble()

# Initialise buffer directories
audio_out_tempdir <- fs::path_temp("audio_out")
fs::dir_create(audio_out_tempdir)

# Background function to catch and play streamed audio
audio_out_bg <- callr::r_bg(function(audio_out_tempdir) {
  cli::cli_alert_info("Background audio in loop initiated")
  cli::cli_alert_info("Directory is {audio_out_tempdir}")

  # Main loop
  i <- 0
  played_up_to <- 0
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
    
    i <- i + 1
    if (i == 30) break
    Sys.sleep(1)
  }

}, args = list(audio_out_tempdir = audio_out_tempdir))

# Main loop handles stream events
j <- 0
while (TRUE) {

  cli::cli_alert_info("Main loop iteration {j}")
  do_later_now()

  # Send questions
  if (j == 0) {
    cli::cli_alert_info("Sending question")
    stream$send_text("What is the capital of Germany?", response_modalities = c("audio", "text"))
  }
  if (j == 5) {
    cli::cli_alert_info("Sending question")
    stream$send_text("And France?", response_modalities = c("audio", "text"))
  }

  # Check event log for updates
  eventlog_current <- stream$eventlog$as_tibble()
  cli::cli_alert_info("There are {nrow(eventlog_current)} total events")
  eventlog_new <- dplyr::anti_join(eventlog_current, eventlog)
  cli::cli_alert_info("There are {nrow(eventlog_new)} new events")

  # Write new audio out events to the buffer
  new_audio <- eventlog_new %>%
    dplyr::filter(type == "response.audio.delta") 
  if (nrow(new_audio) > 0) {
    cli::cli_alert_info("There are {nrow(new_audio)} new audio events")
    new_audio <- new_audio %>%
      dplyr::pull(data) %>%
      dplyr::bind_rows() %>%
      dplyr::pull(delta) %>%
      paste0(collapse = "")
    if (stringr::str_length(new_audio) > 0) {
      audio_buffer_file <- fs::file_temp(pattern = lubridate::now() |> as.character(), tmp_dir = audio_out_tempdir)
      writeLines(new_audio, audio_buffer_file)
    }
  }

  # Update the eventlog
  eventlog <- eventlog_current

  j <- j + 1
  if (j == 10) break
  Sys.sleep(1)
}

# Report on what happened in audio in loop, once it has finished
audio_out_bg$read_all_error_lines()
audio_out_bg$get_result()
