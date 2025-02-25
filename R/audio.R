#' Capture an Audio Chunk and Encode to Base64
#'
#' @param duration Duration in seconds to record (default 1 sec).
#' @param bitrate Bitrate in Hz (default 24000)
#' @return A base64-encoded string of the recorded audio.
#' @export
capture_audio_chunk <- function(duration = 1, rate = 24000) {
  # Create a temporary file
  temp_wav <- tempfile(fileext = ".wav")
  
  cli::cli_alert_info("Starting audio recording on macOS...")
  
  # First, check for available audio devices (useful for debugging)
  cli::cli_alert_info("Audio devices available:")
  system("system_profiler SPAudioDataType | grep 'Input\\|Name\\|Manufacturer' | grep -A 2 'Input'", intern = TRUE)
  
  # Try sox with specific device settings
  # -q: quiet mode
  # -b 16: 16-bit depth
  # -r 16000: 16kHz sample rate (better quality than 8kHz)
  # -c 1: mono channel
  # -d: default audio input device
  
  # First attempt with sox using default device
  cli::cli_alert_info("Recording using sox...")
  sox_args <- c("-q", "-t", "coreaudio", "default", 
                "-b", "16", "-r", as.character(rate), "-c", "1",
                temp_wav, "trim", "0", as.character(duration))
  
  system2("sox", sox_args, stderr = FALSE)
  sox_success <- file.exists(temp_wav) && file.info(temp_wav)$size > 100
  
  # Check if we got good audio
  if (sox_success) {
    cli::cli_alert_success("Sox recording successful!")
  } else {
    # Try with Apple's afrecord
    cli::cli_alert_info("Sox failed, trying afrecord...")
    
    afrecord_args <- c("-d", as.character(duration), 
                      "-f", "WAVE", 
                      "-c", "1", 
                      "-r", as.character(rate),
                      temp_wav)
    
    system2("afrecord", afrecord_args)
    afrecord_success <- file.exists(temp_wav) && file.info(temp_wav)$size > 100
    
    if (!afrecord_success) {
      # Last attempt with rec command
      cli::cli_alert_info("Trying rec command...")
      rec_args <- c("-q", "-r", as.character(rate), "-c", "1", temp_wav, "trim", "0", as.character(duration))
      system2("rec", rec_args)
    }
  }
  
  # Final check for successful recording
  if (file.exists(temp_wav) && file.info(temp_wav)$size > 100) {
    cli::cli_alert_success(paste("Recording successful! File size:", file.info(temp_wav)$size, "bytes"))
    
    # Display file info for debugging
    cli::cli_alert_info("File information:")
    system2("file", temp_wav, stdout = TRUE)
    
    # Encode the WAV file
    audio_b64 <- base64enc::base64encode(temp_wav)
    
    # Delete the temporary file
    unlink(temp_wav)
    
    return(audio_b64)
  } else {
    cli::cli_alert_danger("Recording failed or contained only silence")
    
    # Let's try one last approach with higher gain
    cli::cli_alert_info("Trying with higher gain settings...")
    
    # Use sox with gain adjustment
    sox_args <- c("-q", "-t", "coreaudio", "default", 
                 "-b", "16", "-r", as.character(rate), "-c", "1",
                 temp_wav, "gain", "10", "trim", "0", as.character(duration))
    
    system2("sox", sox_args)
    
    if (file.exists(temp_wav) && file.info(temp_wav)$size > 100) {
      cli::cli_alert_success("Recording with gain adjustment successful!")
      audio_b64 <- base64enc::base64encode(temp_wav)
      unlink(temp_wav)
      return(audio_b64)
    } else {
      # Create placeholder silent audio as last resort
      cli::cli_alert_warning("All recording attempts failed. Creating silent placeholder audio.")
      
      # Generate silent PCM16 data
      silent_pcm <- raw(rate * duration * 2)  # 2 bytes per sample for 16-bit
      
      # Add WAV header
      wav_header <- raw(44)
      writeBin(charToRaw("RIFF"), wav_header, start = 1)
      writeBin(as.integer(length(silent_pcm) + 36), wav_header, start = 5, size = 4, endian = "little")
      writeBin(charToRaw("WAVE"), wav_header, start = 9)
      writeBin(charToRaw("fmt "), wav_header, start = 13)
      writeBin(as.integer(16), wav_header, start = 17, size = 4, endian = "little")
      writeBin(as.integer(1), wav_header, start = 21, size = 2, endian = "little")
      writeBin(as.integer(1), wav_header, start = 23, size = 2, endian = "little")
      writeBin(as.integer(rate), wav_header, start = 25, size = 4, endian = "little")
      writeBin(as.integer(rate * 2), wav_header, start = 29, size = 4, endian = "little")
      writeBin(as.integer(2), wav_header, start = 33, size = 2, endian = "little")
      writeBin(as.integer(16), wav_header, start = 35, size = 2, endian = "little")
      writeBin(charToRaw("data"), wav_header, start = 37)
      writeBin(as.integer(length(silent_pcm)), wav_header, start = 41, size = 4, endian = "little")
      
      wav_file <- file(temp_wav, "wb")
      writeBin(wav_header, wav_file)
      writeBin(silent_pcm, wav_file)
      close(wav_file)
      
      audio_b64 <- base64enc::base64encode(temp_wav)
      unlink(temp_wav)
      return(audio_b64)
    }
  }
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
