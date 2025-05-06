#' Do 'later' tasks now
#'
#' The future is now
#'
#' @export
do_later_now <- function() { later::run_now(1L) }

#' Format current datetime
#'
#' @return A character vector containing the formatted current datetime
#'
#' @export
format_datetime <- function() {
  lubridate::now() |> format("%Y-%m-%d %H:%M:%S %Z")
}

#' Retrieve OpenAI API key from the OPENAI_API_KEY environmental variable
#'
#' @param verbose Print messages? Defaults to TRUE. Errors will always be
#' printed.
#' @return A character string containing the API key
#' @export
openai_api_key <- function(verbose = TRUE) {

  # Check if already set in options
  if (!is.null(getOption("realtalk.OPENAI_API_KEY"))) {
    return(getOption("realtalk.OPENAI_API_KEY"))
  }

  if (verbose) cli::cli_alert_info("Retrieving OpenAI API key from {.envvar OPENAI_API_KEY} environmental variable...")
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_API_KEY}")
  }
  options(`realtalk.OPENAI_API_KEY` = openai_api_key)
  openai_api_key
}
