#' Start a Streaming Connection to OpenAI Realtime API via WebSocket
#'
#' Connects to the realtime endpoint using a WebSocket with the given model and
#' API key. 
#'
#' @param api_key Your long-term OpenAI API key. Defaults to
#' `Sys.getenv("OPENAI_API_KEY")`.
#' @param model A character string specifying the model. Defaults to
#' `lemur::openai_api_key()`.
#' @param voice A character string specifying the voice to use. Defaults to
#' "verse".
#' @param verbose Print messages? Defaults to TRUE. Errors will always be
#' printed.
#' @export
start_stream <- function(
  api_key = lemur::openai_api_key(verbose = verbose),
  model = "gpt-4o-realtime-preview-2024-12-17",
  voice = "verse",
  verbose = TRUE
) {

  # Construct the websocket URL with the model query parameter.
  url <- paste0("wss://api.openai.com/v1/realtime?model=", model)
  
  # Create header strings as required.
  headers <- list(
    "Authorization" = paste0("Bearer ", api_key),
    "OpenAI-Beta" = "realtime=v1"
  )
  
  # Create a new WebSocket client.
  ws <- websocket::WebSocket$new(url, header = headers, autoConnect = FALSE)
  
  # Define event callbacks.
  ws$onOpen(function(event) {
    if (verbose) cli::cli_alert_success("Connected to server.")
  })
  
  ws$onMessage(function(event) {
    data <- jsonlite::fromJSON(event$data)
    if (verbose) cli::cli_alert_info("Received event:\n{jsonlite::toJSON(data, pretty = TRUE)}")
  })
  
  ws$onError(function(event) {
    cli::cli_alert_danger("WebSocket error: {event$message}")
  })
  
  ws$onClose(function(event) {
    if (verbose) cli::cli_alert_success("Connection closed.")
  })
  
  # Connect to the websocket server.
  ws$connect()
  
  return(ws)
}
