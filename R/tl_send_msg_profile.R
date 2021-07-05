#' @title Send SMS via a Number Pool attached to Telnyx Messaging Profil
#'
#' @description
#' Send an SMS through the Number Pool Feature of Telnyx. Specifically to the \code{"https://api.telnyx.com/v2/messages/number_pool"} endpoint.
#' This is the recommended way of sending texts because it takes advantage of all the Messaging Profile features
#' Which include things like matching locale and sticky sender. You also don't need need to remember which phone number to use as the sending number.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/messaging/Messaging-Profiles}{here} for more details.
#'
#' @param to the number to send the message to
#' @param message the message to send
#' @param profile_id messaging profile id
#' @param key Telnyx API key
#' @param params optional further params to pass into the API call. Should be a named list.
#'
#' @export
tl_send_msg_profile <- function(to,
                                message,
                                profile_id,
                                key,
                                params = NULL) {
  body <- list(to = to,
               text = message,
               messaging_profile_id = profile_id)
  r <- ..tl_postv2(body, ep = 'messages/number_pool',
                   key, params)
  return(r)
}
