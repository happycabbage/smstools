
# Get the phone numbers associated with a messaging service ---------------------------------
getTwilioMessagingNums <- function(ms, sid, token) {
  print(ms)
  print(sid)
  print(token)
  url <- paste0("https://messaging.twilio.com/v1/Services/", ms,"/PhoneNumbers")
  authentication <- authenticate(sid, token)
  response <- GET(url, config = authentication)
  status <- http_status(response)
  print(status$message)
  responseContent <- jsonlite::fromJSON(content(response, 'text'))
  numbers <- responseContent$phone_numbers$phone_number
  return(numbers)
}

# Get all text message for a certain criteria ---------------------------------
getTwilioMessages <- function(date = NULL, from = NULL, to = NULL, sid, token) {
  url <- paste0("https://api.twilio.com/2010-04-01/Accounts/", sid,"/Messages.json")
  authentication <- authenticate(sid, token)
  response <- GET(url, config = authentication, query = list(DateSent = date, To = to, From = from))
  status <- http_status(response)
  print(status$message)
  responseContent <- jsonlite::fromJSON(content(response, 'text'))
  messages <- responseContent$messages
  messages$subresource_uris <- NULL
  if(length(messages > 0)) {
    output <- messages %>%
      mutate(
          date_created = dmy_hms(date_created)
        , date_updated = dmy_hms(date_updated)
        , date_sent    = dmy_hms(date_sent)
      ) %>%
      select(
        sid
        , date_created
        , date_updated
        , date_sent
        , to
        , from
        , body
        , status
        , num_segments
        , num_media
        , price
      )
  } else {
    output <- NULL
  }

  return(output)
}

# Send a text message via a messaging service ---------------------------------
sendTwilioMessagingService <- function(to, message, mediaUrl = NA, ms, sid, token) {
  url <- paste0("https://api.twilio.com/2010-04-01/Accounts/", sid,"/Messages.json")
  body <- list(
      MessagingServiceSid = ms
    , To = to
    , Body = message
  )

  if (!is.na(mediaUrl)) {
    body <- c(body, MediaUrl = mediaUrl)
  }

  authentication <- authenticate(sid, token)
  response <- POST(url, config = authentication, body = body)
  return(response)
}


#this buys a single number
buyTwilioNumber<- function(accountSid, apikey, area_code) {
  #prepare body for POST req
  phone_body <- list(
    AccountSid = accountSid,
    AreaCode = area_code

  )
  #then post for incomingphonenumbers
  incoming_nums_endpt <- paste0('https://api.twilio.com/2010-04-01/Accounts/',accountSid,'/IncomingPhoneNumbers.json?')
  req <- POST(incoming_nums_endpt, authenticate(accountSid, apikey), body = phone_body)
  phone_sid <- content(req)$sid
  return(phone_sid)
}

#this sets up a single phone number
setupTwilioNumber <- function(accountSid, apikey, phone_sid){
  #gets studio flow
  flow_id<-getFlowId(accountSid, apikey)
  #calls http webhook embedded in the studio flow, sets up call forwarding
  voice_url <- paste0('https://webhooks.twilio.com/v1/Accounts/',accountSid,'/Flows/', flow_id)
  update_url <- paste0('https://api.twilio.com/2010-04-01/Accounts/',accountSid,'/IncomingPhoneNumbers/',phone_sid,'.json')
  voice_body <- list(
    VoiceUrl = voice_url
  )
  voice_req<- POST(update_url, authenticate(accountSid, apikey), body = voice_body)

  #gets msg_service sid to connect the phone number we just purchased
  serv_req <- content(GET('https://messaging.twilio.com/v1/Services/', authenticate(accountSid, apikey)))$services

  #checks to see which service is 'live'
  if (str_detect(serv_req[[1]]$friendly_name, pattern = 'retired')){
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
  check <- POST(url, authenticate(accountSid, apikey), body = msg_body)
  return(content(check))
}

cycleNums <- function(accountSid, apikey, area_code, num){
  i<- 0
  while (i < num){
    phone_sid<-buyTwilioNumber(accountSid, apikey, carea_code)
    resp <-setupTwilioNumber(accountSid, apikey, phone_sid)
    i <- i+1
  }
}
