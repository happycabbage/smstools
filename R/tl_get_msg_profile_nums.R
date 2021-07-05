# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
#
# Copyright(c) Happy Cabbage Analytics
# -------------------------------------
#' @title List numbers associated with a Messaging Profile
#'
#' @description
#' Lists all phone numbers associated with a Messaging Profile and their settings.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/messaging/Messaging-Profiles#listMessagingProfilePhoneNumbers}{here} for more details.
#'
#'
#' @param key Required. Telnyx API key.
#' @param profile_id Required. Messaging Profile id to update all numbers.
#'
#' @export
tl_get_msg_profile_nums <- function(key,
                                    profile_id) {
  nums <- list()
  curr_pg <- 0
  tot_pg <- 1
  while (curr_pg < tot_pg) {
    r <- ..tl_getv2(
      ep = stringr::str_glue('messaging_profiles/{profile_id}/phone_numbers'),
      apikey = key,
      query = list("page[number]" = curr_pg + 1,
                   "page[size]"   = 250)
    )
    httr::stop_for_status(r,
                          stringr::str_glue("Paginating, at page {curr_pg +1} out of {tot_pg}"))
    res <- httr::content(r, 'parsed', encoding = 'UTF-8')
    nums <- c(nums, list(res))
    curr_pg <- res[['meta']][['page_number']]
    tot_pg <- res[['meta']][['total_pages']]
  }

  output <- purrr::map(nums, `[[`, 'data')
  output <- purrr::flatten(output)
  return(output)
}



