# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
#
# Copyright(c) Happy Cabbage Analytics
# -------------------------------------
#' @title Setup call forwarding for a given Messaging Profile
#'
#' @description
#' Sets call forwarding up for a given Messaging Profile.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/numbers/Number-Configurations}{here} for more details.
#'
#'
#' @param key Required. Telnyx API key.
#' @param fwd_number Required. Phone number to forward to. Should be a string in the format +14156660420.
#' @param profile_id  Messaging Profile id to update all numbers. Required if \code{phone_ids} are not set.
#' @param phone_ids Vector of phone ids to update. Note these are not the phone numbers, but the Telnyx assigned ID. Required if \code{profile_id} is not set.
#' @param ... Unused at this time.
#'
#' @export
tl_set_call_fwd <- function(key,
                            fwd_number,
                            profile_id = NULL,
                            phone_ids = NULL,
                            ...) {
  if (nchar(fwd_number) != 12 |
      stringr::str_sub(fwd_number, 1, 1) != "+") {
    stop(
      "Forwarding number must be a valid 10 digit number with country code supplied. e.g. +14206660069."
    )
  }

  # Get the list of phone numbers for the messaging service
  if (!is.null(profile_id)) {
    if (!is.null(phone_ids))
      warning("profile_id set, ignoring supplied phone_ids.")
    pids <-
      purrr::map_chr(tl_get_msg_profile_nums(key, profile_id), `[[`, 'id')
  } else if (!is.null(phone_ids)) {
    pids <- phone_ids
  } else {
    stop("Neither profile_id nor phone_ids supplied.")
  }

  # Now set the call forwarding for all of them
  bdy <- list(
    'call_forwarding' = list(
      'call_forwarding_enabled' = TRUE,
      'forwarding_type'         = 'always',
      'forwards_to'             = fwd_number
    )
  )
  r_ll <- purrr::map(pids, function(id) {
    resp <- ..tl_postv2(
      body   = bdy,
      ep     = stringr::str_glue('phone_numbers/{id}/voice'),
      apikey = key,
      fun    = httr::PATCH
    )
    httr::stop_for_status(resp, task = "Assigning call forwarding to numbers")
    return(resp)
  })
  return(purrr::map(r_ll, httr::content, 'parsed', encoding = 'UTF-8'))
}
