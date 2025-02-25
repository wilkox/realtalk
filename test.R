load_all()
stream <- Stream$new()
stream$send_text("What is the capital of France?", response_modalities = c("audio", "text"))
el <- stream$eventlog$as_tibble()

library(tidyverse)

audio_chunks <- el %>%
  filter(type == "response.audio.delta") %>%
  pull(data) %>%
  map(~ .x$delta)

audio_b64 <- audio_chunks[[6]]

  audio_binary <- base64enc::base64decode(audio_b64)
  audio_int <- readBin(
    audio_binary, 
    integer(), 
    n = length(audio_binary)/2,  # 2 bytes per 16-bit sample
    size = 2,                    # 2 bytes for 16-bit
    signed = TRUE                # PCM16 is signed
  )
  audio::play(audio_int, rate = 24000)

for (chunk in audio_chunks) { play_audio_chunk(chunk) }
