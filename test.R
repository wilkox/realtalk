load_all()
stream <- Stream$new()
stream$send_text("Say hello to the user in the audio stream", role = "system", trigger_response = TRUE)
stream$set_status_message("This is a status message. Respond to the user in faux-French.")
stream$start_streaming()

stream$set_status_message("The status message has been updated. Now speak in faux-Russian.")
stream$send_text("What is the date today?", role = "user", trigger_response = TRUE)
stream$send_text("What is your favourite colour?", role = "user", trigger_response = TRUE)

stream$stop_streaming()
stream$transcript()
system("killall r; killall sox; killall tail")
