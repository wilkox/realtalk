#' Do 'later' tasks now
#'
#' The future is now
#'
#' @export
do_later_now <- function() { later::run_now(1L) }

#' A nicely formatted timestamp
#'
#' @return A character vector containing the timestamp
#'
#' @export
timestamp <- function() {
  lubridate::now() |> format("%Y-%m-%d %H:%M:%S %Z")
}
