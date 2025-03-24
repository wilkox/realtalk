# Set up
load_all()
Stream <- readRDS("stream.rds")

Stream$transcript()

Stream$eventlog$as_tibble()
