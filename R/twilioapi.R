#' TWILIO + POLARIS
#'
#' TBD
#'
#' @param message TBD
#' @param mediaUrl TBD
#' @param date TBD
#' @param from TBD
#' @param to TBD
#' @param ms TBD
#' @param sid TBD
#' @param token TBD
#' @param apikey TBD
#' @param area_code TBD
#' @param phone_sid TBD
#' @param num TBD
#'
#' @import httr
#'
#' @describeIn twilio Get the phone numbers associated with a messaging service
#' @export
getTwilioMessagingNums <- function(ms, sid, token) {
  url <- paste0("https://messaging.twilio.com/v1/Services/", ms,"/PhoneNumbers")
  r <- httr::GET(url, config = authenticate(sid, token))
  parsed <- jsonlite::fromJSON(content(r, 'text'))
  return(parsed$phone_numbers$phone_number)
}


#' @describeIn twilio Get all text message for a certain criteria
#' @export
getTwilioMessages <- function(date = NULL, from = NULL, to = NULL, sid, token) {
  r <- httr::GET(
    url = paste0("https://api.twilio.com/2010-04-01/Accounts/", sid,"/Messages.json"),
    config = httr::authenticate(sid, token),
    query = list(
      DateSent = date,
      To = to,
      From = from
    ))
  status <- httr::http_status(r)
  parsed <- jsonlite::fromJSON(content(r, 'text'))
  msg <- parsed$messages
  msg$subresource_uris <- NULL

  if (length(msg > 0)) {
    out <- msg %>%
      mutate(
        date_created = dmy_hms(date_created),
        date_updated = dmy_hms(date_updated),
        date_sent    = dmy_hms(date_sent)
      ) %>%
      select(
        sid,
        date_created,
        date_updated,
        date_sent,
        to,
        from,
        body,
        status,
        num_segments,
        num_media,
        price
      )
    return(out)
  }
  return(NULL)
}

#' @describeIn twilio Send a text message via a messaging service
#' @export
sendTwilioMessagingService <- function(to, message, mediaUrl = NA, ms, sid, token) {
  url <- paste0("https://api.twilio.com/2010-04-01/Accounts/", sid,"/Messages.json")
  body <- list(MessagingServiceSid = ms, To = to, Body = message)
  if (!is.na(mediaUrl))
    body <- c(body, MediaUrl = mediaUrl)
  r <- httr::POST(url, config = httr::authenticate(sid, token), body = body)
  return(r)
}


#' @describeIn twilio buys a single number
#' @export
buyTwilioNumber <- function(sid, apikey, area_code) {
  arg <- list(
    AccountSid = sid,
    AreaCode = area_code
  )
  url <- paste0('https://api.twilio.com/2010-04-01/Accounts/',sid,'/IncomingPhoneNumbers.json?')
  r <- httr::POST(
    url,
    httr::authenticate(sid, apikey),
    body = arg
  )
  return(content(r)$sid)
}


#' @describeIn twilio sets up a single phone number
#' @export
setupTwilioNumber <- function(sid, apikey, phone_sid){
  #gets studio flow
  flow_id <- getFlowId(sid, apikey)
  #calls http webhook embedded in the studio flow, sets up call forwarding
  voice_url <- paste0('https://webhooks.twilio.com/v1/Accounts/',sid,'/Flows/', flow_id)
  update_url <- paste0('https://api.twilio.com/2010-04-01/Accounts/',sid,'/IncomingPhoneNumbers/',phone_sid,'.json')
  voice_body <- list(VoiceUrl = voice_url)
  voice_req <- httr::POST(update_url, httr::authenticate(sid, apikey), body = voice_body)

  #gets msg_service sid to connect the phone number we just purchased
  serv_req <- httr::content(httr::GET('https://messaging.twilio.com/v1/Services/',
                                      httr::authenticate(sid, apikey)))$services

  #checks to see which service is 'live'
  if (stringr::str_detect(serv_req[[1]]$friendly_name, pattern = 'retired')) {
    service_sid <- serv_req[[2]]$sid
  }else{
    service_sid <- serv_req[[1]]$sid
  }
  #add phone-sid to message service
  url <- paste0('https://messaging.twilio.com/v1/Services/',service_sid,'/PhoneNumbers')
  msg_body <- list(
    ServiceSid = service_sid,
    PhoneNumberSid = phone_sid
  )
  #check to see if the phone number is associated with the correct msg_service
  check <- httr::POST(url, httr::authenticate(sid, apikey), body = msg_body)
  return(httr::content(check))
}


#' @describeIn twilio cycle nums
#' @export
cycleNums <- function(sid, apikey, area_code, num){
  i <- 0
  while (i < num) {
    phone_sid <- buyTwilioNumber(sid, apikey, carea_code)
    resp  <- setupTwilioNumber(sid, apikey, phone_sid)
    i <- i + 1
  }
}
