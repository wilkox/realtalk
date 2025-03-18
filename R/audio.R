#' Capture an Audio Chunk and Encode to Base64
#'
#' @param duration Duration in seconds to record (default 1 sec).
#' @param bitrate Bitrate in Hz (default 24000)
#' @return A base64-encoded string of the recorded audio.
#' @export
capture_audio_chunk <- function(duration = 1, rate = 24000) {

  # Create a temporary file
  temp_wav <- tempfile(fileext = ".wav")
  
  # Use sox with specific device settings
  # -q: quiet mode
  # -b 16: 16-bit depth
  # -r 16000: 16kHz sample rate (better quality than 8kHz)
  # -c 1: mono channel
  # -d: default audio input device
  
  # First attempt with sox using default device
  sox_args <- c("-q", "-t", "coreaudio", "default", 
                "-b", "16", "-r", as.character(rate), "-c", "1",
                temp_wav, "trim", "0", as.character(duration))
  
  system2("sox", sox_args, stderr = FALSE)
  
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
