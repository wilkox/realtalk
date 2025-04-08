load_all()
stream <- Stream$new()
stream$start_streaming()
stream$send_text("This is a default message")
stream$send_text("This is a message with trigger_response = TRUE", trigger_response = TRUE)
stream$send_text("This is a message with trigger_response = FALSE", trigger_response = FALSE)
stream$stop_streaming()

stream$transcript()

system("killall r; killall sox; killall tail")
