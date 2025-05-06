#' Capture Audio Chunks to a Directory
#' 
#' @param dir Directory to write the wav files to
#' @param duration Duration in seconds to record (default 1 sec).
#' @param bitrate Bitrate in Hz (default 24000)
#'
#' @export
capture_audio_chunks_to_dir <- function(dir, duration = 1, bitrate = 24000) {
  cli::cli_alert_info("Capturing audio chunks to directory {dir}")

  if (! fs::dir_exists(dir)) {
    cli::cli_alert_info("Creating directory {dir}")
    fs::dir_create(dir)
  }

  audio_in_wav <- fs::path(dir, "audio_in", ext = "wav")
  sox_args <- c(
    "-q",
    "-t", "coreaudio", "default",
    "-b", "16",
    "-r", as.character(bitrate),
    "-c", "1",
    paste0("\"", audio_in_wav, "\""),
    "silence", "1", "0.01", "1%", "1", "0.5", "1%",
    ":", "newfile",
    ":", "restart" 
  )
  system2("sox", sox_args, stderr = FALSE)
}


#' Capture an Audio Chunk to File
#'
#' @param file File to write to
#' @param duration Duration in seconds to record (default 1 sec).
#' @param bitrate Bitrate in Hz (default 24000)
#'
#' @export
capture_audio_chunk_file <- function(file, duration = 1, bitrate = 24000) {
  cli::cli_alert_info("Capturing to file {file}")

  # Use sox with specific device settings
  # -q: quiet mode
  # -b 16: 16-bit depth
  # -r 16000: 16kHz sample rate (better quality than 8kHz)
  # -c 1: mono channel
  # -d: default audio input device
  
  # First attempt with sox using default device
  sox_args <- c(
    "-q",
    "-t", "coreaudio", "default",
    "-b", "16",
    "-r", as.character(bitrate),
    "-c", "1",
    paste0("\"", file, "\""),
    "trim", "0", as.character(duration)
  )
  
  system2("sox", sox_args, stderr = FALSE)
}

#' Capture an Audio Chunk and Encode to Base64
#'
#' @param duration Duration in seconds to record (default 1 sec).
#' @param bitrate Bitrate in Hz (default 24000)
#' @return A base64-encoded string of the recorded audio.
#'
#' @export
capture_audio_chunk_base64 <- function(duration = 1, bitrate = 24000) {

  # Create a temporary file
  temp_wav <- tempfile(fileext = ".wav")
  
  # Capture audio
  capture_audio_chunk_file(temp_wav, duration = duration, bitrate = bitrate)
  
  # Encode the WAV file
  audio_b64 <- base64enc::base64encode(temp_wav)
  
  # Delete the temporary file
  unlink(temp_wav)
  
  return(audio_b64)
}

#' Decode Base64 Audio and Play It
#'
#' @param audio_b64 A base64-encoded string representing audio.
#' @param bitrate Bitrate in Hz (default 24000).
#'
#' @export
play_audio_chunk <- function(audio_b64, bitrate = 24000) {
  audio_binary <- base64enc::base64decode(audio_b64)
  audio_int <- readBin(
    audio_binary, 
    integer(), 
    n = length(audio_binary)/2,  # 2 bytes per 16-bit sample
    size = 2,                    # 2 bytes for 16-bit
    signed = TRUE                # PCM16 is signed
  )
  audio::play(audio_int, rate = bitrate)
  duration <- length(audio_binary) / (bitrate * 2) 
  Sys.sleep(duration)
}
