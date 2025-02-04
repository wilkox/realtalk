#' Send Text Message
#'
#' @param ws The websocket connection object.
#' @param text A character string to send.
#' @param response_modalities The modalit(y|ies) that the model should respond
#' in. At least one of "text" and "audio". Defaults to text only.
#' @export
send_text <- function(ws, text, response_modalities = c("text")) {

  # Send the text
  payload <- list(
    type = jsonlite::unbox("conversation.item.create"),
    item = list(
      type = jsonlite::unbox("message"),
      role = jsonlite::unbox("user"),
      content = list(list(
        type = jsonlite::unbox("input_text"),
        text = jsonlite::unbox(text)
      ))
    )
  )
  ws$send(jsonlite::toJSON(payload, auto_unbox = FALSE))

  # Trigger a response
  payload <- list(
    type = jsonlite::unbox("response.create"),
    response = list(modalities = response_modalities)
  )
  ws$send(jsonlite::toJSON(payload, auto_unbox = FALSE))
}
