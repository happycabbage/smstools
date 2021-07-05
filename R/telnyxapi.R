
### these functions are describedIn non-existant files.


#' @describeIn POST to Telnyx API v2 (thats why its called v2)
#' @export
..tl_postv2 <- function(body,
                        ep,
                        apikey,
                        p   = NULL,
                        fun = httr::POST) {
  authentication <- stringr::str_glue('Bearer {apikey}')
  args <- list(
    url  = stringr::str_glue('https://api.telnyx.com/v2/{ep}'),
    body = jsonlite::toJSON(c(body, p), auto_unbox = T),
    httr::add_headers('Authorization' = authentication),
    httr::content_type_json()
  )
  resp <- do.call(fun, args)
  return(resp)
}

#' @describeIn POST to Telnyx API v1 (thats why its called v1)
#' @export
..tl_postv1 <- function(body,
                        ep,
                        token,
                        user,
                        p = NULL) {
  resp <- httr::POST(
    url  = stringr::str_glue('https://api.telnyx.com/{ep}'),
    body = jsonlite::toJSON(c(body, p), auto_unbox = T),
    httr::add_headers('x-api-token' = token,
                      'x-api-user'  = user),
    httr::content_type_json()
  )
  return(resp)
}

#' @describeIn GET to Telnyx API v1 (thats why its called v1)
#' @export
..tl_getv1 <- function(ep, token, user, ...) {
  resp <- httr::GET(
    url  = stringr::str_glue('https://api.telnyx.com/{ep}'),
    httr::add_headers('x-api-token' = token,
                      'x-api-user'  = user),
    ...
  )
  return(resp)
}

#' @describeIn GET to Telnyx API v2 (thats why its called v2)
#' @export
..tl_getv2 <- function(ep, apikey,
                       fun = httr::GET,
                       ...) {
  authentication <- stringr::str_glue('Bearer {apikey}')
  args <- list(
    url  = stringr::str_glue('https://api.telnyx.com/v2/{ep}'),
    httr::add_headers('Authorization' = authentication),
    ...
  )
  resp <- do.call(fun, args)
  return(resp)
}


#' @describeIn WHERE?
#' @export
tl_update_msg_profile <- function(key,
                                  profile_id,
                                  params) {
  r <- ..tl_postv2(
    body   = params,
    ep     = stringr::str_glue('messaging_profiles/{profile_id}'),
    apikey = key,
    fun    = httr::PATCH
  )
  httr::stop_for_status(r)
  return(httr::content(r, as = 'parsed', encoding = 'UTF-8'))
}




#' #' @title Get SMS messages via MDR beta endpoint
#' #'
#' #' @description
#' #' Get SMS messages from the MDR search endpoint.
#' #'
#' #' See \href{https://developers.telnyx.com/docs/api/v2/messaging/MDR-Search-Beta}{here} for more details.
#' #'
#' #' @param start_time start time of report. Required. Must include the timezone.
#' #' @param end_time end time for end of report. Required. Must include the timezone.
#' #' @param key Required. API v2 Key
#' #' @param profile_id optional. Messaging profile.
#' #' @param params. More params to pass down.
#' #'
#' #' @export
#' tel_getv2_messages <- function(
#'   start_time,
#'   end_time,
#'   key,
#'   profile_id = NULL,
#'   params     = NULL
#' ) {
#'
#'   # Prepare Query
#'   qry <- list(
#'     'start_date' = format(lubridate::with_tz(start_time, 'UTC'), '%Y-%m-%dT%H:%M:%S+00:00'),
#'     'end_date'   = format(lubridate::with_tz(end_time, 'UTC'), '%Y-%m-%dT%H:%M:%S+00:00')
#'   )
#'   if (!is.null(profile_id)) {
#'     qry[['outbound_profile_id']] = profile_id
#'   }
#'   qry <- c(qry, params)
#'   results <- list()
#'   curr_pg <- 5
#'   tot_pg <- 10
#'   while(curr_pg < tot_pg) {
#'     pgs <- list(
#'       "page[number]" = curr_pg + 1,
#'       "page[size]"   = 25
#'     )
#'     r <- ..tl_getv2(
#'       ep     = stringr::str_glue('message_detail_records'),
#'       apikey = key,
#'       query = c(qry, pgs)
#'     )
#'     print(httr::content(r))
#'     httr::stop_for_status(r, stringr::str_glue("Paginating, at page {curr_pg +1} out of {tot_pg}"))
#'     res <- httr::content(r, 'parsed', encoding = 'UTF-8')
#'     results <- c(results, list(results))
#'     curr_pg <- res[['meta']][['page_number']]
#'     tot_pg <- res[['meta']][['total_pages']]
#'   }
#'   return(results)
#' }


