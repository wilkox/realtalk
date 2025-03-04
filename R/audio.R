#' Capture an Audio Chunk and Encode to Base64
#'
#' @param duration Duration in seconds to record (default 1 sec).
#' @return A base64-encoded string of the recorded audio.
#' @export
capture_audio_chunk <- function(duration = 1) {
  cli::cli_alert_info("Starting to record audio...")
  chunk <- audio::record(
    where = 8000 * duration,
    rate = 8000 * duration,
    channels = 1
  )
  Sys.sleep(duration) # Wait for the audio to record
  cli::cli_alert_success("Finished recording audio")
  temp_wav <- tempfile(fileext = ".wav")
  audio::save.wave(chunk, temp_wav)
  audio_b64 <- base64enc::base64encode(temp_wav)
  unlink(temp_wav)
  return(audio_b64)
}

#' Decode Base64 Audio and Play It
#'
#' @param audio_b64 A base64-encoded string representing audio.
#' @export
play_audio_chunk <- function(audio_b64) {
  temp_wav <- tempfile(fileext = ".wav")
  base64enc::base64decode(audio_b64, temp_wav)
  audio::play(temp_wav)
  unlink(temp_wav)
}
