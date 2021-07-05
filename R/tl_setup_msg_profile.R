# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
#
# Copyright(c) Happy Cabbage Analytics
# -------------------------------------
#' @title Setup Messaging Profile with numbers and Call Forwarding
#'
#' @description
#' Setup a messaging profile, purchase a set of numbers, and apply call forwarding.
#' This is a convenience wrapper around \code{tl_create_msg_profile}, \code{tl_order_numbers}, and \code{tl_set_call_fwd}
#' If you want more specific settings use those functions individually or pass in function parameters.
#'
#' See \href{https://developers.telnyx.com/docs/api/v2/numbers/Number-Configurations}{here} for more details.
#'
#' @param api_key Required. Telnyx API key.
#' @param profile_name Required. Messaging Profile name.
#' @param n_phone_numbers Required. Number of phone numbers to allocate to the Messaging Service.
#' @param profile_state Required. Abbreviated state name to look up phone numbers. e.g. CA, OR, CO, etc.
#' @param profile_city Optional. City name to narrow down the search to provision phone numbers.
#' @param call_fwd_number Optional. Phone number to forward to. Should be a string in the format +14156660420.
#' @param verbose Defaults to \code{FALSE}.
#' @param ... Optional further parameters to pass into other functions.
#'
#' @export
tl_setup_msg_profile <- function(api_key,
                                 profile_name,
                                 n_phone_numbers,
                                 profile_state,
                                 profile_city    = NULL,
                                 call_fwd_number = NULL,
                                 verbose = FALSE,
                                 ...) {
  # Create Messaging Profile
  profile <- tl_create_msg_profile(
    key             = api_key,
    name            = profile_name,
    geomatch        = TRUE,
    skip_unhealthy  = TRUE,
    sticky_sender   = TRUE,
    ...
  )
  msg_profile_id <- profile[['data']][['id']]
  if (verbose) {
    print(
      stringr::str_glue(
        "Created Messaging Profile
                            with name {profile$data$name} and
                            id {msg_profile_id} ..."
      )
    )
  }

  # Order and assign numbers
  safe_order <-
    purrr::safely(tl_order_numbers, otherwise = list(), quiet = verbose)
  num_order <- safe_order(
    key   = api_key,
    n     = n_phone_numbers,
    city  = profile_city,
    state = profile_state,
    incl_best_effort = TRUE,
    profile_id       = msg_profile_id,
    ...
  )

  # Handle when no numbers are successfully ordered for a given area code.
  if (!is.null(num_order$error)) {
    del <- ..tl_getv2(
      ep     = stringr::str_glue('messaging_profiles/{msg_profile_id}'),
      apikey = api_key,
      fun    = httr::DELETE
    )
    httr::stop_for_status(del,
                          task = stringr::str_glue("Error in deleting messaging profile {msg_profile_id}"))
    stop(
      stringr::str_glue(
        "Error in ordering numbers.
                           Cleaning up by deleting messaging
                           profile {msg_profile_id}.
                           Maybe try a different region?"
      )
    )
  } else {
    nums <- num_order$result
  }

  # Check how many we bought.
  n_purchased <- nums[['data']][['phone_numbers_count']]
  if (verbose) {
    print(stringr::str_glue("Purchased {n_purchased} numbers ..."))
  }

  if (n_purchased != n_phone_numbers) {
    warning(
      "Not enough available phone numbers in region, only
            purcahsed {n_purchased} phone numbers"
    )
  }

  # to let the above finish processing.
  # TODO: Figure out latency between number allocation and order request
  Sys.sleep(2)

  # Setup call forwarding
  if (!is.null(call_fwd_number)) {
    fwd <- tl_set_call_fwd(key         = api_key,
                           fwd_number  = call_fwd_number,
                           profile_id  = msg_profile_id,
                           ...)
  } else {
    fwd <- NULL
  }

  returnObject <- list(
    'msg_profile'        = profile,
    'ordered_phone_nums' = nums,
    'p_voice_settings'   = fwd
  )
  return(returnObject)
}
