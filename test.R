load_all()
stream <- Stream$new()

stream$start_streaming()

stream$eventlog
stream$transcript()

stream$stop_streaming()

stream$eventlog
stream$transcript()

stream$bg_process$read_all_error_lines()

