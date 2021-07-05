# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
#
# Copyright(c) Happy Cabbage Analytics
# -------------------------------------



#' @title Purchase numbers and optionally assign to Messaging Profile.
#'
#' @description
#' Purchase Telnyx numbers and optionally assign them to a Messaging Profile.
#' This leverages a combination of the Number Search API and Number Order API to find
#' numbers based on a series of search parameters.
#'
#' Currently only configured to purchase US phone numbers.
#'
#' See \href{https://developers.telnyx.com/docs/v2/numbers/quickstarts/number-search}{here} for more details.
#'
#'
#' @param key Required. Telnyx API key
#' @param n Required. Integer number of phone numbers to purchase. Maximum is 100 If less than 100 are available
#' for the given filter parameters it will purchase all available numbers.
#' @param search_only Boolean. If \code{TRUE} will return only the results of the number search and not purchase the numbers. Defaults to \code{FALSE}.
#' @param incl_best_effort Boolean. If \code{TRUE} will include 'best effort' results,
#'   i.e. results that are close to the filtered parameters (e.g. geographically) but are not exactly within the filter.
#'   Defaults to \code{FALSE} in order to make this decision explicit by the user.
#' @param type the type of phone number to purchase. Options are 'toll-free' and 'long-code'. Defaults to 'long-code'.
#' @param area_code Optional. Specify the 3 digit area code the phone number should be associated with as a character string.
#' @param state Optional. Two character state abbreviation that the phone number should be in.
#' @param city Optional. City that the phone number should be in. It is recommended to use this in conjunction with the \code{state} parameter.
#' @param profile_id Optional. The messaging profile to assign the numbers to.
#' @param params Optional further arguments to pass into API body. Should be a named list.
#' @param ... Unused at this time.
#'
#' @export
tl_order_numbers <- function(key,
                             n,
                             search_only      = FALSE,
                             incl_best_effort = FALSE,
                             type        = 'long-code',
                             area_code   = NULL,
                             city        = NULL,
                             state       = NULL,
                             profile_id  = NULL,
                             params      = NULL,
                             ...) {
  if (!type %in% c("long-code", "toll-free")) {
    stop("Invalid type parameter. Options are 'long-code' and 'toll-free'")
  }

  if (n < 1 | n > 100) {
    stop("n must be an integer between 1 and 100 inclusive.")
  }

  # Prepare filters for search
  query <- list(
    "filter[country_code]" = "US",
    "filter[features]"     = "sms",
    "filter[limit]"        = n,
    "filter[best_effort]"  = incl_best_effort
  )
  if (type == 'toll-free') {
    query <- c(query, list('filter[number_type]' = 'toll-free'))
  }
  if (!is.null(area_code)) {
    area_code <- as.character(area_code)
    if (nchar(area_code) != 3) {
      warning("Likely invalid area_code. Use a 3 character string to improve results.")
    }
    query <-
      c(query,
        list('filter[national_destination_code]' = area_code))
  }
  if (!is.null(state)) {
    if (nchar(state) > 2) {
      warning(
        "State parameter is not a recognized state abbreviation. Recommend using 2 character state abbreviations."
      )
    }
    query <- c(query, list('filter[administrative_area]' = state))
  }
  if (!is.null(city)) {
    if (is.null(state)) {
      warning(
        "City set without state parameter state. It is recommended to set the state parameter with the city parameter."
      )
    }
    query <- c(query, list('filter[locality]' = city))
  }

  # Perform search and parse results
  r <- ..tl_getv2(ep = 'available_phone_numbers',
                  apikey = key,
                  query = query)
  httr::stop_for_status(r)
  avail_nums <- httr::content(r, 'parsed', encoding = 'UTF-8')
  if (avail_nums[['meta']][['total_results']] == 0) {
    stop("No numbers for provided filters. Adjust parameters to widen search.")
  }
  if (search_only) {
    return(avail_nums)
  }

  # Now order the numbers
  order <- purrr::map_chr(avail_nums$data, `[[`, 'phone_number')
  order <- purrr::map(order, ~ list('phone_number' = .x))
  bdy <- list('phone_numbers' = order)
  if (!is.null(profile_id)) {
    bdy <- c(bdy, list('messaging_profile_id' = profile_id))
  }
  r_order <- ..tl_postv2(ep = 'number_orders',
                         apikey = key,
                         body = bdy)
  httr::stop_for_status(r_order)
  return(httr::content(r_order, 'parsed', encoding = 'UTF-8'))
}
