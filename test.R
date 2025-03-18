# Set up session
load_all()
stream <- Stream$new()
eventlog <- stream$eventlog$as_tibble()

# Initial instructions
stream$send_text(
  "You are the front end for a clinical advice expert system for treating patients with poorly-controlled hypertension. The setting is Australia. A doctor has phoned you to ask for advice. Your role is to be a humane and professional interface to the system. You do not, and are not permitted to, independently provide medical advice or make medical decisions in any way, shape, or form. Instead, you will be communicate with the system backend via passing text messages. The backend will advise you on (for example) what questions to ask and provide you with the answers to any questions the doctor might have. If you do not have an answer provided to you, be open and honest about this; feel free to give a lightly humorous response such as 'I've got the brains trust in the back room working on that one, I'll have an answer for you soon'. Only respond to the doctor with audio. Be friendly and professional. If the doctor is unsure how to start the conversation, encourage them to explain the clinical problem, you will be passed more specific questions to ask as appropriate."
, role = "system")

# Initialise buffer directories
audio_out_tempdir <- fs::path_temp("audio_out")
fs::dir_create(audio_out_tempdir)
audio_in_tempdir <- fs::path_temp("audio_in")
fs::dir_create(audio_in_tempdir)

# Background function to catch and play streamed audio out
audio_out_outfile <- fs::file_temp()
audio_out_errfile <- fs::file_temp()
audio_out_bg <- callr::r_bg(
  function(audio_out_tempdir) {
    cli::cli_alert_info("Background audio out loop initiated")
    cli::cli_alert_info("Directory is {audio_out_tempdir}")

    # Main loop
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

# Background function to record audio in to file
audio_in_bg <- callr::r_bg(function(audio_in_tempdir) {
  cli::cli_alert_info("Background audio in loop initiated")
  cli::cli_alert_info("Directory is {audio_in_tempdir}")

  realtalk::capture_audio_chunks_to_dir(audio_in_tempdir)

}, args = list(audio_in_tempdir = audio_in_tempdir))

# Main loop handles stream events
j <- 0
while (TRUE) {

  cli::cli_h1("Main loop iteration {j}")
  do_later_now()

  # Check audio in buffer for new audio chunks and stream them
  audio_in_files <- fs::dir_ls(audio_in_tempdir)
  cli::cli_alert_info("There are {length(audio_in_files)} file{?s} in the audio in buffer")
  for (audio_in_file in audio_in_files) {
    audio_chunk_base64 <- readBin(audio_in_file, what = "raw", n = file.info(audio_in_file)$size) |>
    base64enc::base64encode()
    if (length(audio_chunk_base64) > 0) stream$send_audio(audio_chunk_base64)
    fs::file_delete(audio_in_file)
  }

  # Check event log for updates
  eventlog_current <- stream$eventlog$as_tibble()
  cli::cli_alert_info("There are {nrow(eventlog_current)} total events")
  eventlog_new <- dplyr::anti_join(eventlog_current, eventlog, by = dplyr::join_by(created_at, type, data))

  # Write new audio out events to the buffer
  new_audio <- eventlog_new %>%
    dplyr::filter(type == "response.audio.delta") 
  cli::cli_alert_info("There are {nrow(new_audio)} new audio out events")
  if (nrow(new_audio) > 0) {
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
  Sys.sleep(1)
}

# Report on what happened in audio in loop, once it has finished
audio_out_bg$read_all_error_lines()
audio_out_bg$get_result()
