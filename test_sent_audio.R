# Set up session
load_all()
stream <- Stream$new()
stream$send_text("The user will ask you a question, please respond with audio.")

# Capture 5 1-second chunks of audio
audio <- purrr::map_chr(1:5, capture_audio_chunk)

# Send them to the stream
for (chunk in audio) {
  stream$send_audio(chunk)
  play_audio_chunk(chunk)
}

# Wait 10 seconds
Sys.sleep(10)

stream$eventlog$as_tibble()

# Pull and play the response
audio_response <- stream$eventlog$as_tibble() |>
  dplyr::filter(type == "response.audio.delta") |>
  dplyr::pull(data) |>
  dplyr::bind_rows() |>
  dplyr::pull(delta) |>
  paste0(collapse = "")
play_audio_chunk(audio_response)
