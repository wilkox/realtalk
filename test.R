load_all()
stream <- Stream$new()

stream$start_streaming()

stream$eventlog
stream$transcript()

# stream$send_text("The user will be communicating with you via audio, while the the client software (the sender of this message) will be passing you instructions via text messages. Occasionally, you may be asked to send backchannel information to the client software with a text message. Begin by greeting the user and asking their name.", role = "system")

stream$send_text("What is the capital of Norway? Respond with text, not audio.", role = "user")

stream$text_received()

stream$stop_streaming()

stream$eventlog
stream$transcript()

stream$bg_process$read_all_error_lines()
stream$bg_process$read_all_output_lines()
