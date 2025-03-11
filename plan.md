# realtalk

## Overview

This is an R interface to the OpenAI 'realtime' API, which provides access to a multimodal (voice and text) language model for real-time conversations. The API is documented at [https://platform.openai.com/docs/api-reference/realtime] and [https://platform.openai.com/docs/api-reference/realtime]. The package does not aim to be a comprehensive interface, but rather to support a specific use case: simultaneous real-time streaming of a bidirectional audio conversation between a human user and the model, and a bidirectional text stream between client software and the model providing additional instructions, context, etc. As an illustrative example:

| Audio stream                         | Text stream                          |
|--------------------------------------|--------------------------------------|
|                                      | CLIENT: You are a customer service   |
|                                      | assistant for the Watts car rental   |
|                                      | company. Your name is James. The     |
|                                      | customer who has called is named     |
|                                      | Marvin and they have a reservation   |
|                                      | for a white Honda Fit to be picked   |
|                                      | up tomorrow.                         |
|--------------------------------------|--------------------------------------|
| MODEL: Watts customer service, this  |                                      |
| is James speaking, how may I help    |                                      |
| you?                                 |                                      |
|--------------------------------------|--------------------------------------|
| USER: Uh hi, I have a car rental     |                                      |
| booked with you tomorrow?            |                                      |
|--------------------------------------|--------------------------------------|
| MODEL: That's correct, we have you   |                                      |
| white Honda Fit reserved for you     |                                      |
| tomorrow.                            |                                      |
|--------------------------------------|--------------------------------------|
| USER: I just wanted to check, did I  | MODEL: Where is the collection point |
| reserve that to collect from the     | for the reservation? CLIENT: The     |
| Central Train Station office or from | collection point for the reservation |
| the Northern Train Station office?   | is the Northern Train Station office.|
|--------------------------------------|--------------------------------------|
| MODEL: It's reserved for collection  |                                      |
| from the Northern Train Station      |                                      |
| office.                              |                                      |
|--------------------------------------|--------------------------------------|
| USER: Great, thanks.                 |                                      |
|--------------------------------------|--------------------------------------|


## Current state

The package currently implements an interface to the Stream of Events with the API, as well as functions for capturing and playing back base64-encoded audio.

## Next step

The next step is to implement real-time, turn-taking audio conversations with the model. The OpenAI realtime API implements Voice Activity Detection (VAD), where a stream of audio input is automatically analysed to detect the end of a user's voice input, which triggers an audio response without any particular event or trigger being required from the client. This means that enabling real-time, turn-taking conversations only requires implementing audio capture and streaming to the API, and then catching and playing the response.

This should be implemented as two buffers, one for audio in and one for audio out. The audio in loop should capture and buffer chunks of audio and send them to the API. The audio out loop should capture audio response chunks from the stream (these have the event type "response.audio.delta") and play them using the already implemented `play_audio_chunk()` function. Special care must be taken with playback to minimise audible gaps between audio chunks. In practice, concatenating the chunks prior to playback results in smooth-sounding audio, but this must be balanced against the need for responsiveness; waiting until a full response has been buffered before playing it will introduce unacceptable latency. 

The following code gives an example of receiving and playing audio from the stream using already implemented functions:

```{r}
library(tidyverse)
load_all()
stream <- Stream$new()
stream$send_text("What is the capital of France?", response_modalities = c("audio", "text"))
el <- stream$eventlog$as_tibble()

audio_chunks <- el %>%
  filter(type == "response.audio.delta") %>%
  pull(data) %>%
  map(~ .x$delta)

for (chunk in audio_chunks) { play_audio_chunk(chunk) }

allchunks <- paste0(audio_chunks, collapse = "")
play_audio_chunk(allchunks)

test_audio_chunk <- capture_audio_chunk()
play_audio_chunk(test_audio_chunk)
```

The two buffer loops will ideally be implemented as non-blocking concurrent processes, for example using the futures package.

## Roadmap

Once audio conversation I/O has been implemented, a similar set of text buffers will need to be implemented. These will be slightly more complicated as they will need to have an interface (e.g. a socket) which other processes in the client application can use to send and receive text messages in the stream.
