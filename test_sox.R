fs::file_copy("/var/folders/0k/pm1z1z0d7vq66z8b6m5v83f00000gn/T/RtmpFaJqaD/audio_out14f1441ee7b64.txt", "~/tmp/buffer.txt")

audio_out_buffer_path <- "~/tmp/buffer.txt"

buffer |>
  paste0(collapse = "") |>
  realtalk::play_audio_chunk()
