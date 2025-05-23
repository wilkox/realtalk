% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audio.R
\name{capture_audio_chunk_base64}
\alias{capture_audio_chunk_base64}
\title{Capture an Audio Chunk and Encode to Base64}
\usage{
capture_audio_chunk_base64(duration = 1, bitrate = 24000)
}
\arguments{
\item{duration}{Duration in seconds to record (default 1 sec).}

\item{bitrate}{Bitrate in Hz (default 24000)}
}
\value{
A base64-encoded string of the recorded audio.
}
\description{
Capture an Audio Chunk and Encode to Base64
}
