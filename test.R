load_all()
stream <- Stream$new()

stream$start_streaming()

stream$bg_process$read_all_error_lines()
