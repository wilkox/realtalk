# Set up session
load_all()
stream <- Stream$new()
eventlog <- stream$eventlog$as_tibble()

# Initial instructions
# stream$send_text(
#   "You are the front end for a clinical advice expert system for treating patients with poorly-controlled hypertension. The setting is Australia. A doctor has phoned you to ask for advice. Your role is to be a humane and professional interface to the system. You do not, and are not permitted to, independently provide medical advice or make medical decisions in any way, shape, or form. Instead, you will be communicate with the system backend via passing text messages. The backend will advise you on (for example) what questions to ask and provide you with the answers to any questions the doctor might have. If you do not have an answer provided to you, be open and honest about this; feel free to give a lightly humorous response such as 'I've got the brains trust in the back room working on that one, I'll have an answer for you soon'. Only respond to the doctor with audio. Reserve text responses for communicating with the backend system. Be friendly and professional. If the doctor is unsure how to start the conversation, encourage them to explain the clinical problem, you will be passed more specific questions to ask as appropriate."
# , role = "system", response_modalities = c("text", "audio"))
stream$send_text(
  "The user will ask you some simple questions in the audio stream, respond with audio."
, role = "system", response_modalities = c("text", "audio"))

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

# Main loop handles stream events
j <- 0
audio_in_last_processed_byte <- 0
while (TRUE) {

  cli::cli_h1("Main loop iteration {j}")
  do_later_now()

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
    stream$send_audio(new_audio_b64)
    cli::cli_alert_info("{length(new_audio_bin)} bits streamed from audio in buffer")

    # Update the last processed position
    audio_in_last_processed_byte <- audio_in_buffer_size
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
