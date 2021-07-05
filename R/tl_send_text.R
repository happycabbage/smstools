# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
#
# Copyright(c) Happy Cabbage Analytics
# -------------------------------------


#' @title Send text with Telnyx
#'
#' @description
#' Send a text with the Telnyx API. Specifically to the \code{"https://api.telnyx.com/v2/messages"} endpoint
#' Note that using \code{tl_send_messaging_profile} is the preferred way of sending multiple SMS as it is more feature rich.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/messaging/}{here} for more details.
#'
#' @param to number to send the text to
#' @param from number to send the text from
#' @param message character string of the text
#' @param key Telnyx API key
#' @param params optional further params to pass into the API call. Should be a named list.
#'
#' @export
tl_send_text <- function(to,
                         from,
                         message,
                         key,
                         params = NULL) {
  body <- list(to = to, from = from,
               text = message)
  r <- ..tl_postv2(body, ep = 'messages',
                   key, params)
  return(r)
}
