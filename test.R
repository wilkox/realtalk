load_all()
stream <- Stream$new()
stream$send_text("Say hello to the user in the audio stream", role = "system", trigger_response = TRUE)
stream$start_streaming()

stream$stop_streaming()
stream$transcript()
system("killall r; killall sox; killall tail")
