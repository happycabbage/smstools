# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
#
# Copyright(c) Happy Cabbage Analytics
# -------------------------------------


#' @title Create or update a messaging profile
#'
#' @description
#' Create a Telnyx Messaging Profile via the Messgaging Profiles endpoint.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/messaging/Messaging-Profiles}{here} for more details.
#'
#' @param key Required. Telnyx API key
#' @param name Optional. A user friendly name for the messaging profile.
#' @param geomatch Should numbers match area codes? Defaults to TRUE.
#' @param skip_unhealthy Should numbers that are blocked not be used? Defaults to FALSE.
#' @param sticky_sender Should contacts receive messages from the same number? Defaults to TRUE.
#' @param long_code_weight Probability of using Long Code (as a share of the sum of the weights). Defaults to 1.
#' @param toll_free_weight Probability of using Toll Free (as a share of the sum of the weights). Defaults to 1.
#' @param whitelisted_destinations Vector of country codes numbers can send to. Defaults to 'US'. Should be ISO 3166-1 alpha-2 country codes.
#' @param webhook_url Optional. URL for webhook response.
#' @param webhook_failover_url Optional. Backup URL for webhook response.
#' @param profile_id The messaging profile to update. Required for \code{tl_update_msg_profile}.
#' @param params Optional further arguments to pass into API body. Should be a named list.
#'  Required if using \code{tl_update_msg_profile}, use this to pass in updated values as a named list.
#' @param ... Unused at this time.
#'
#' @export
tl_create_msg_profile <- function(key,
                                  name             = NULL,
                                  geomatch         = TRUE,
                                  skip_unhealthy   = FALSE,
                                  sticky_sender    = TRUE,
                                  long_code_weight = 1,
                                  toll_free_weight = 1,
                                  whitelisted_destinations = 'US',
                                  webhook_url              = "",
                                  webhook_failover_url     = "",
                                  params                   = NULL,
                                  ...) {
  body <- list(
    "number_pool_settings" = list(
      "geomatch"         = geomatch,
      "skip_unhealthy"   = skip_unhealthy,
      "sticky_sender"    = sticky_sender,
      "long_code_weight" = long_code_weight,
      "toll_free_weight" = toll_free_weight
    ),
    "whitelisted_destinations" = as.list(whitelisted_destinations),
    "webhook_url"              = webhook_url,
    "webhook_failover_url"     = webhook_failover_url
  )
  if (!is.null(name)) {
    body <- c(body, list("name" = name))
  }
  r <- ..tl_postv2(body   = body,
                   ep     = 'messaging_profiles',
                   apikey = key,
                   params)
  httr::stop_for_status(r)
  return(httr::content(r, as = 'parsed', encoding = 'UTF-8'))
}
