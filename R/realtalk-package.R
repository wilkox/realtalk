#' realtalk: Interface to the OpenAI Realtime API
#'
#' @description
#' Provides tools for interacting with the OpenAI Realtime API.
#' Create streaming sessions with real-time text and audio input/output,
#' handle events, and manage communication with the API through a WebSocket
#' connection. The package supports bidirectional streaming of text and audio
#' content in real-time applications.
#'
#' @name realtalk
#' @aliases realtalk-package
#' @author David Wilkins <david@wilkox.org>
#'
#' @import base64enc 
#' @import checkmate 
#' @import cli 
#' @import dplyr 
#' @import filelock 
#' @import fs 
#' @import glue 
#' @importFrom jsonlite fromJSON toJSON unbox
#' @import later 
#' @import lubridate 
#' @import purrr 
#' @import stringr 
#' @import tibble
#' @import websocket
#' @importFrom audio play
NULL