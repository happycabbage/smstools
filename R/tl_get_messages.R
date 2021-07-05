# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
#
# Copyright(c) Happy Cabbage Analytics
# -------------------------------------

#' @title Get SMS messages.
#'
#' @description
#' Get SMS messages from the MDR reports endpoint.
#' This reporting works as an asynchronous call to the report api and returns a CSV parsed
#' into a data table using \code{fread}
#'
#' See \href{https://developers.telnyx.com/docs/api/v1/reports}{here} for more details.
#'
#' @param start_time start time of report. Required. Must include the timezone.
#' @param end_time optional time for end of report. Must include the timezone.
#' @param profile_ids an optional vector of Messaging Profile ids to filter down to.
#' @param verbose defaults to FALSE.
#' @param v1_token Telnyx API v1 Token
#' @param v1_user Telnyx API v1 user, either an email or the preferred masked API user id.
#' @param params optional further arguments to pass into API query. Should be a named list.
#' @param incl_msg_body should the report include the text of the message?
#'    Note that MMS have invalid JSON mixed with CSV problems. It's recommended you
#'    use \code{tl_get_raw_messages}
#'    Which will make subsequent calls to the message endpoint to get a JSON list.
#'    When using \code{tl_get_raw_messages} set this to \code{FALSE} to save yourself a headache.
#' @param directions Should be list() object. Allowed values are 'inbound' and 'outbound'
#' @param key API v2 Key
#' @param ... Downstream params
#' @param js Output of \code{tl_get_raw_messages} to pass into \code{tl_extract_raw_messages}
#'
#' @importFrom data.table fread
#'
#' @name tl_get_messages
NULL

#' @describeIn tl_get_messages TBD
#' @export
tl_get_messages <- function(start_time,
                            end_time = NULL,
                            profile_ids = NULL,
                            verbose     = FALSE,
                            v1_token,
                            v1_user,
                            params = NULL,
                            incl_msg_body = TRUE,
                            directions = list()) {
  body <- list(
    connections          = list(),
    directions           = directions,
    record_types         = list(),
    include_message_body = incl_msg_body,
    start_time           = format(
      lubridate::with_tz(start_time, 'UTC'),
      '%Y-%m-%dT%H:%M:%S+00:00'
    ),
    timezone             = 'UTC',
    profiles             = as.list(profile_ids)
  )

  if (!is.null(end_time)) {
    body <-
      c(body, list(
        end_time = format(
          lubridate::with_tz(start_time, 'UTC'),
          '%Y-%m-%dT%H:%M:%S+00:00'
        )
      ))
  }

  # Queue the report
  r <- ..tl_postv1(body,
                   ep = 'reporting/mdr_requests',
                   v1_token,
                   v1_user)
  httr::stop_for_status(r)

  # Retry it until its done
  id <- httr::content(r, encoding = 'UTF-8')[['id']]
  if (verbose) {
    message(stringr::str_glue("Messages report queued for generation id: {id}"))
  }

  # Forcing a while loop here- not sure if I'm supposed to though lol
  status <- list()
  slp <- 0
  while (is.null(status[['report_url']])) {
    if (verbose) {
      message(stringr::str_glue("Checking status of report: {id}"))
    }
    Sys.sleep(slp)
    r_status <- ..tl_getv1(ep = stringr::str_glue('reporting/mdr_requests/{id}'),
                           v1_token,
                           v1_user)
    httr::stop_for_status(r_status)
    status <- httr::content(r_status, encoding = 'UTF-8')
    slp <- 1
  }

  # Download report and parse dataframe
  r_report <-
    httr::GET(status[['report_url']], httr::write_memory())
  httr::stop_for_status(r_report)
  txt <- httr::content(r_report, as = 'text', encoding = 'UTF-8')
  txt <-
    stringr::str_replace_all(txt, ',\\"\\{', ",\\'{") # clean up quoting problem
  txt <- stringr::str_replace_all(txt, '\\}\\"\n', "\\}\\'\n")
  dt <- fread(txt, quote = "'")

  return(dt)
}

#' @describeIn tl_get_messages TBD
#' @export
tl_get_raw_messages <- function(key, verbose, ...) {
  df <- tl_get_messages(verbose = verbose, ...)
  ids <- unique(df[['Unique Mdr ID']])
  if (verbose) {
    print(stringr::str_glue("Report retrieved, querying details for {length(ids)} messages..."))
  }
  res <- purrr::map(ids, function(i) {
    ..tl_getv2(ep = stringr::str_glue('messages/{i}'), key)
  })
  cnt <- purrr::map_chr(res, httr::content, as = 'text', encoding = 'UTF-8')
  return(cnt)
}

#' @describeIn tl_get_messages TBD
#' @export
tl_extract_raw_messages <- function(js) {
  jsons <- purrr::map(js, jsonlite::fromJSON, simplifyDataFrame = F, simplifyVector = F)
  jsons <- purrr::map(jsons, `[[`, 'data')
  df <- tibble::tibble(list = jsons) %>%
    tidyr::unnest_wider("list") %>%
    tidyr::unnest_wider("to", names_sep = '_') %>%
    tidyr::unnest_wider("to_1", names_sep = '_') %>%
    tidyr::unnest_wider("from", names_sep = '_') %>%
    tidyr::unnest_wider("cost", names_sep = '_')

  if ("media" %in% names(df)) {
    df <- df %>%
      tidyr::unnest_wider("media", names_sep = '_') %>%
      tidyr::unnest_wider("media_1", names_sep = '_')
  }

  if ("errors" %in% names(df)) {
    df <- df %>%
      tidyr::unnest_wider("errors", names_sep = "_") %>%
      tidyr::unnest_wider("errors_1", names_sep = "_") %>%
      tidyr::unnest_wider("errors_1_meta", names_sep = "_")
  }

  df <- df %>%
    dplyr::mutate_if(is.list, ~purrr::map_chr(., jsonlite::toJSON, force = T))

  return(df)
}
