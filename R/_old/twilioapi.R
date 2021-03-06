#'
#' #' @title Send text with Twilio
#' #'
#' #' @description
#' #' Send a text with the Twilio API. Specifically to the \code{"https://api.twilio.com/2010-04-01/Accounts/YOUR_SID/Messages.json"} endpoint
#' #' Works similar \code{twilio::tw_send_message()} only we are explicit that this is a text message, and allow the \code{sid} and \code{token} to be passed in as parameters
#' #' Note that using \code{tw_send_messaging_service} is the preferred way of sending multiple SMS as it is more feature rich.
#' #'
#' #' See \href{https://www.twilio.com/docs/sms/send-messages}{here} for more details.
#' #'
#' #' @param to number to send the text to
#' #' @param from number to send the text from
#' #' @param message character string of the text
#' #' @param sid Twilio account sid. Defaults to \code{Sys.getenv('TWILIO_SID')}
#' #' @param token Twilio account token. Defaults to \code{Sys.getenv('TWILIO_TOKEN')}
#' #' @param params optional further params to pass into the API call. Should be a named list.
#' #'
#' #' @export
#' tw_send_text <- function(
#'   to,
#'   from,
#'   message,
#'   sid = Sys.getenv("TWILIO_SID"),
#'   token = Sys.getenv("TWILIO_TOKEN"),
#'   params = NULL
#' ) {
#'
#'   url <- paste0("https://api.twilio.com/2010-04-01/Accounts/", sid,"/Messages.json")
#'   authentication <- httr::authenticate(sid, token)
#'   body <- list(
#'     To = to,
#'     From = from,
#'     Body = message
#'   )
#'   twilioResponse <- httr::POST(url, config = authentication, body = c(body, params))
#'   httr::stop_for_status(twilioResponse)
#'
#'   return(twilioResponse)
#' }
#'
#'
#' #' @title Send a message through the Twilio Notify Service.
#' #'
#' #' @description
#' #' Send a message with the Twilio Notify API. Specifically to the \code{"https://notify.twilio.com/v1/Services/YOUR_NOTIFY_TOKEN/Notifications"} endpoin.
#' #'
#' #' See \href{https://www.twilio.com/docs/notify/send-notifications}{here} for more details.
#' #' Unless you are a user of the Notify API and are trying to send notifcations across device types I suggest using \code{tw_send_messaging_service}
#' #' With your favorite \code{apply}, \code{purrr:map}, or \code{for(m in messages)} equivalent.
#' #'
#' #' @param message character string of the text
#' #' @param tag character string of the Twilio Notify tag you are messaging. See \href{https://www.twilio.com/docs/notify/send-notifications}{here}
#' #' @param service_sid optional explicit decleration of \code{TWILIO_NOTIFY_SID} defaults to \code{Sys.getenv('TWILIO_NOTIFY_SERVICE'}. See \href{https://www.twilio.com/docs/notify/quickstart/sms#gather-account-information}{here}..
#' #' @param sid Twilio account sid. Defaults to \code{Sys.getenv('TWILIO_SID')}
#' #' @param token Twilio account token. Defaults to \code{Sys.getenv('TWILIO_TOKEN')}
#' #' @param params further optional vakues to send into the API call (e.g. \code{identity}). Should be a named ;ist. See \href{https://www.twilio.com/docs/notify/api/notification-resource}{here}
#' #'
#' #' @export
#' tw_notify_twilio <- function(
#'   message,
#'   tag,
#'   service_sid = Sys.getenv('TWILIO_NOTIFY_SID'),
#'   sid         = Sys.getenv('TWILIO_SID'),
#'   token       = Sys.getenv('TWILIO_TOKEN'),
#'   params      = NULL
#'
#' ) {
#'   url <- paste0("https://notify.twilio.com/v1/Services/", service_sid, "/Notifications")
#'   body <- list(
#'     Tag = tag
#'     , Body = message
#'   )
#'   authentication <- httr::authenticate(sid, token)
#'
#'   # Notifiy response
#'   response <- httr::POST(url, config = authentication, body = c(body, params))
#'   httr::stop_for_status(response)
#'   return(response)
#' }
#'
#'
#' #' @title Get numbers from a Twilio Messaging Service
#' #'
#' #' @description
#' #' Get phone numbers from a Messaging Service from the \code{"https://messaging.twilio.com/v1/Services/SERVICE_SID/PhoneNumbers"} endpoint.
#' #'
#' #' See \href{https://www.twilio.com/docs/sms/services}{here} for more details.
#' #'
#' #' @param service_sid messaging service sid. Defaults to \code{Sys.getenv('TWILIO_MESSAGING_SERVICE')}.
#' #' @param sid Twilio account sid. Defaults to \code{Sys.getenv('TWILIO_SID')}
#' #' @param token Twilio account token. Defaults to \code{Sys.getenv('TWILIO_TOKEN')}
#' #'
#' #' @export
#' tw_get_messaging_service_nums <- function(
#'   service_sid = Sys.getenv('TWILIO_MESSAGING_SERVICE'),
#'   sid         = Sys.getenv('TWILIO_SID'),
#'   token       = Sys.getenv('TWILIO_TOKEN')
#' ) {
#'
#'   # Get First Set
#'   url <- paste0("https://messaging.twilio.com/v1/Services/", service_sid,"/PhoneNumbers")
#'   authentication <- httr::authenticate(sid, token)
#'   response <- httr::GET(url, config = authentication)
#'   httr::stop_for_status(response)
#'   responseContent <- jsonlite::fromJSON(httr::content(response, 'text', encoding = 'UTF-8'))
#'   numbers <- responseContent$phone_numbers$phone_number
#'
#'   # Page Through if More than 50
#'   while (!is.null(responseContent$meta$next_page_url)) {
#'     response <- httr::GET(responseContent$meta$next_page_url, config = authentication)
#'     httr::stop_for_status(response)
#'     responseContent <- jsonlite::fromJSON(httr::content(response, 'text', encoding = 'UTF-8'))
#'     numbers <- c(numbers, responseContent$phone_numbers$phone_number)
#'   }
#'
#'   return(numbers)
#' }
#'
#'
#' #' @title Send SMS via the Twilio Messaging service
#' #'
#' #' @description
#' #' Send an SMS through the Twilio Messaging Service. Specifically to the \code{"https://api.twilio.com/2010-04-01/Accounts/YOUR_SID/Messages.json"} endpoint.
#' #' This is the recommended way of sending texts because it takes advantage of all the \href{https://www.twilio.com/copilot}{Copilot} features
#' #' Which include things like matching locale and sticky sender. You also don't need need to remember which phone number to use as the sending number.
#' #'
#' #' See \href{https://www.twilio.com/docs/sms/services}{here} for more details.
#' #'
#' #' @param to the number to send the message to
#' #' @param message the message to send
#' #' @param service_sid messaging service sid. Defaults to \code{Sys.getenv('TWILIO_MESSAGING_SERVICE')}.
#' #' @param sid Twilio account sid. Defaults to \code{Sys.getenv('TWILIO_SID')}
#' #' @param token Twilio account token. Defaults to \code{Sys.getenv('TWILIO_TOKEN')}
#' #' @param params further params to pass into the API call. Should be a named list.
#' #'
#' #' @export
#' tw_send_messaging_service <- function(
#'   to,
#'   message,
#'   service_sid = Sys.getenv('TWILIO_MESSAGING_SERVICE'),
#'   sid         = Sys.getenv('TWILIO_SID'),
#'   token       = Sys.getenv('TWILIO_TOKEN'),
#'   params      = NULL
#' ) {
#'   url <- paste0("https://api.twilio.com/2010-04-01/Accounts/", sid,"/Messages.json")
#'   body <- list(
#'     MessagingServiceSid = service_sid
#'     , To = to
#'     , Body = message
#'   )
#'   authentication <- httr::authenticate(sid, token)
#'   response <- httr::POST(url, config = authentication, body = c(body, params))
#'   httr::stop_for_status(response)
#'   return(response)
#' }
#'
#' #' Internal function for importing messages
#' #'
#' #' @param messages value on the \code{responseContent$messages} object form the \code{Messages.json} endpoint
#' #'
#' #' @import magrittr
#' #' @importFrom rlang .data
#' `_parse_tw_messages` <- function(messages) {
#'   output <- messages %>%
#'     dplyr::mutate(
#'       "date_created" = lubridate::dmy_hms(.data[['date_created']])
#'       , "date_updated" = lubridate::dmy_hms(.data[['date_updated']])
#'       , "date_sent"    = lubridate::dmy_hms(.data[['date_sent']])
#'     ) %>%
#'     dplyr::mutate_if(
#'       is.integer,
#'       as.character
#'     ) %>%
#'     dplyr::mutate_if(
#'       is.logical,
#'       as.character
#'     ) %>%
#'     dplyr::mutate_if(
#'       is.list,
#'       ~purrr::map_chr(., jsonlite::toJSON)
#'     )
#'
#'   return(output)
#' }
#'
#'
#' #' @title Get SMS messages.
#' #'
#' #' @description
#' #' Get SMS messages from the  \code{"https://api.twilio.com/2010-04-01/Accounts/YOUR_SID/Messages.json"} endpoint.
#' #' This can loop through the pagination of the messages for pulling down more than 50 messages at a time.
#' #'
#' #' See \href{https://www.twilio.com/docs/sms/services}{here} for more details.
#' #'
#' #' @param date the date the message was sent. Defaults to \code{NULL} which gives you all dates.
#' #' @param from the phone number the message was sent from. Defaults to \code{NULL} which gives you all sending phone numbers.
#' #' @param to the phone number the message was sent to. Defaults to \code{NULL} which gives you all receiving phone numbers.
#' #' @param get_all should we get all messages instead of the first 50? Defaults to \code{FALSE}. Caution, lots of messages will take a lot of time.
#' #' @param verbose optional for verbose output. Defaults to \code{FALSE}
#' #' @param sid Twilio account sid. Defaults to \code{Sys.getenv('TWILIO_SID')}
#' #' @param token Twilio account token. Defaults to \code{Sys.getenv('TWILIO_TOKEN')}
#' #' @param page_size Number of messages per page to return. Defaults to 50.
#' #' @param params optional further arguments to pass into API query. Should be a named list.
#' #'
#' #' @import magrittr
#' #'
#' #' @export
#' tw_get_messages <- function(
#'   date = NULL,
#'   from = NULL,
#'   to = NULL,
#'   get_all = FALSE,
#'   verbose = FALSE,
#'   sid = Sys.getenv('TWILIO_SID'),
#'   token = Sys.getenv('TWILIO_TOKEN'),
#'   params = NULL,
#'   page_size = 50
#' ) {
#'
#'   # First Go to endpoint to pull down the first set
#'   url <- paste0("https://api.twilio.com/2010-04-01/Accounts/", sid,"/Messages.json")
#'   authentication <- httr::authenticate(sid, token)
#'   query <- list(
#'     DateSent = date,
#'     To = to,
#'     From = from,
#'     PageSize = page_size
#'   )
#'
#'   response <- httr::GET(
#'     url,
#'     config = authentication,
#'     query = c(query, params)
#'   )
#'   httr::stop_for_status(response)
#'
#'   # First set of messages
#'   responseContent <- jsonlite::fromJSON(httr::content(response, 'text', encoding = 'UTF-8'))
#'
#'   if(length(responseContent$messages) > 0) {
#'     if(verbose){message(paste("Pulled down first set of messages from", url))}
#'     messages <- responseContent$messages
#'     messages$subresource_uris <- NULL
#'     output <- messages %>%
#'       `_parse_tw_messages`() %>%
#'       dplyr::distinct()
#'   } else {
#'     warning("No messages in response: ", response$url)
#'     output <- NULL
#'   }
#'
#'
#'   if(get_all) {
#'     nextPageUri <- responseContent$next_page_uri
#'     nextPage <- paste0("https://api.twilio.com",nextPageUri)
#'     while(!is.null(nextPageUri)) {
#'
#'       # Grab data
#'       response <- httr::GET(nextPage, config = authentication)
#'       if(verbose){message(paste("Pulled down messages from", nextPage))}
#'
#'       # We do this here in case during pagination there is an error
#'       # For very large pulls this avoids losing all the data
#'       # Which is why its preferred than an explicit httr::stop_for_status()
#'       responseContent <- tryCatch({
#'         json <- jsonlite::fromJSON(httr::content(response, 'text', encoding = 'UTF-8'))
#'       }, error = function(e) {
#'         warning("Message response content failed: ", nextPage, httr::message_for_status(response))
#'         return(NULL)
#'       })
#'
#'       # grab shit
#'       if (!is.null(responseContent$messages)) {
#'         messages <- responseContent$messages
#'         nextPageUri <- responseContent$next_page_uri
#'         nextPage <- paste0("https://api.twilio.com",nextPageUri)
#'         messages$subresource_uris <- NULL
#'
#'         # Combine
#'         output <- messages %>%
#'           `_parse_tw_messages`() %>%
#'           dplyr::bind_rows(output) %>%
#'           dplyr::distinct()
#'       } else {
#'         nextPage <- NULL
#'       }
#'     }
#'
#'   }
#'
#'   return(output)
#' }
#'
#' #' @describeIn twilio buys a single number
#' #' @export
#' buyTwilioNumber <- function(sid, apikey, area_code) {
#'   arg <- list(
#'     AccountSid = sid,
#'     AreaCode = area_code
#'   )
#'   url <- paste0('https://api.twilio.com/2010-04-01/Accounts/',sid,'/IncomingPhoneNumbers.json?')
#'   r <- httr::POST(
#'     url,
#'     httr::authenticate(sid, apikey),
#'     body = arg
#'   )
#'   return(content(r)$sid)
#' }
#'
#'
#' #' @describeIn twilio sets up a single phone number
#' #' @export
#' setupTwilioNumber <- function(sid, apikey, phone_sid){
#'   #gets studio flow
#'   flow_id <- getFlowId(sid, apikey)
#'   #calls http webhook embedded in the studio flow, sets up call forwarding
#'   voice_url <- paste0('https://webhooks.twilio.com/v1/Accounts/',sid,'/Flows/', flow_id)
#'   update_url <- paste0('https://api.twilio.com/2010-04-01/Accounts/',sid,'/IncomingPhoneNumbers/',phone_sid,'.json')
#'   voice_body <- list(VoiceUrl = voice_url)
#'   voice_req <- httr::POST(update_url, httr::authenticate(sid, apikey), body = voice_body)
#'
#'   #gets msg_service sid to connect the phone number we just purchased
#'   serv_req <- httr::content(httr::GET('https://messaging.twilio.com/v1/Services/',
#'                                       httr::authenticate(sid, apikey)))$services
#'
#'   #checks to see which service is 'live'
#'   if (stringr::str_detect(serv_req[[1]]$friendly_name, pattern = 'retired')) {
#'     service_sid <- serv_req[[2]]$sid
#'   }else{
#'     service_sid <- serv_req[[1]]$sid
#'   }
#'   #add phone-sid to message service
#'   url <- paste0('https://messaging.twilio.com/v1/Services/',service_sid,'/PhoneNumbers')
#'   msg_body <- list(
#'     ServiceSid = service_sid,
#'     PhoneNumberSid = phone_sid
#'   )
#'   #check to see if the phone number is associated with the correct msg_service
#'   check <- httr::POST(url, httr::authenticate(sid, apikey), body = msg_body)
#'   return(httr::content(check))
#' }
#'
#'
#' #' @describeIn twilio cycle nums
#' #' @export
#' cycleNums <- function(sid, apikey, area_code, num){
#'   i <- 0
#'   while (i < num) {
#'     phone_sid <- buyTwilioNumber(sid, apikey, carea_code)
#'     resp  <- setupTwilioNumber(sid, apikey, phone_sid)
#'     i <- i + 1
#'   }
#' }
