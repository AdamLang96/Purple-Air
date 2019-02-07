#Libraries 
library(purrr)
library(lubridate)
library(RCurl)
library(stringr)
library(dplyr)


# SloFilter <- function(url)
#
# Args:
#  url: https://www.purpleair.com/json
#
# Returns:
#  Dataframe with: "ID","Label", "THINGSPEAK_PRIMARY_ID", 
#  "THINGSPEAK_PRIMARY_ID_READ_KEY",
#  "THINGSPEAK_SECONDARY_ID", "THINGSPEAK_SECONDARY_ID_READ_KEY", 
#  "Lat", "Lon", "PM2_5Value", "LastSeen" for sensors in SLO County
#
SloFilter <- function(url) {
  raw <- RCurl::getURL(url)    
  datadump <- jsonlite::fromJSON(raw)
  dumpresults <- datadump$results 
  slo <- dumpresults[dumpresults$Lon > -120.88 & dumpresults$Lon < -120.30 
                     & dumpresults$Lat > 34.97 & dumpresults$Lat < 35.79 
                     & is.na(dumpresults$Lat) == FALSE, ]
  keepers = c("ID","Label", "THINGSPEAK_PRIMARY_ID", "THINGSPEAK_PRIMARY_ID_READ_KEY",
              "THINGSPEAK_SECONDARY_ID", "THINGSPEAK_SECONDARY_ID_READ_KEY", 
              "Lat", "Lon", "PM2_5Value", "LastSeen")
  newdata <- slo[keepers]
  newdata <- newdata[newdata$LastSeen >= as.numeric(Sys.time()) - 60*60*24, ]
  return(newdata)
  
}



# CreateURL <- function(id, key)
#
# Args:
#  id:  ThingSpeak Primary ID
#  key: ThingSpeak Primary ID Read Key
#
# Returns:
#  Single URL for retrieving sensor data constructed 
#  from ThingSpeak Primary ID and ThingSpeak Primary ID Read Key
#
CreateURL <- function(id, key){
  url <- paste0("https://api.thingspeak.com/channels/", id,  
                "/feeds.json?api_key=", key,
                "&average=60&days=1")
}




# MapThrough <- function(df)
#
# Args
#  df: Outputted dataframe from SloFilter function
#
# Returns:
#  List of URLs based off ThingSpeak Primary ID and
#  and ThingSpeak Primary ID Read Key
#
# Notes:
#  Written with CreateURL
#
MapThrough <- function(df){
  arg1 <- df["THINGSPEAK_PRIMARY_ID"]
  arg2 <- df["THINGSPEAK_PRIMARY_ID_READ_KEY"]
  urlist <- mapply(CreateURL, arg1, arg2)
  return(urlist)
}




# ExtractValues <- function(url)
#
# Args:
#  url: A  single URL constructed by CreateURL/MapThrough
#
# Returns:
#  Dataframe with contained 24 hours of values for
#  Date/Time, PM2.5, Humidity, Temperature, Sensor Name,
#  Latitude, and Longitude
#
# Notes:
#  First row is the previous hour so that the entire hours average is considered
#
ExtractValues <- function(url) {
  rawinfo <- RCurl::getURL(url)
  parseddata <- jsonlite::fromJSON(rawinfo)
  if(length(parseddata$feeds) == 0) {
    cleandata <- NULL
  } else {
    parseddata$feeds$created_at <- as.POSIXct(parseddata$feeds$created_at, tz = "UTC", 
                                              "%Y-%m-%dT%H:%M:%SZ")-8*60*60
    parseddata$feeds$created_at <- force_tz(parseddata$feeds$created_at, 
                                            tzone = "Etc/GMT+8")
    cleandata <- data.frame("Date-Time" = parseddata$feeds$created_at, 
                            "PM2.5" = parseddata$feeds$field2,
                            "Humidity" = parseddata$feeds$field7, 
                            "Temperature" = parseddata$feeds$field6,
                            "Name" = parseddata$channel$name, 
                            "Latitude" = parseddata$channel$latitude,
                            "Longitude" = parseddata$channel$longitude, 
                            stringsAsFactors = FALSE)
    # Reversing order so most recent value is on the top. Getting rid of first value
    # So that entire hours average is considered
    cleandata <- cleandata[order(nrow(cleandata):2), ]
  }
  return(cleandata)
}





# MasterFunc <- function(url)
#
# Args:
#  url: "https://www.purpleair.com/json"
#
# Returns:
#  List of dataframes
#  Each dataframe corresponds to a location of a Purple Air device
#  Each consists of 24 values for Date/Time, A and B PM 2.5 values, Latititude and
#  Longitude of sensor
#
MasterFunc  <- function(url) {
  # Creating a bunch of empty vectors and lists for later use
  datalist  <- c()
  datanames <- c()
  vector_a  <- c()
  vector_b  <- c()
  totalveclist <- list()
  # Initializing dataframe of appropriate size for later use
  dataframe <- data.frame("dummy"=rep(NA, 24))
  latframe  <- data.frame("dummy"=rep(NA,24))
  lonframe  <- data.frame("dummy"=rep(NA,24))
  filtered  <- SloFilter(url)
  urllist   <- MapThrough(filtered)
  # Creating a vector of all sensor names and a df of each column by iterating the
  # Extract values function over all the URLS
  for(i in 1:length(urllist)) {
    values <- ExtractValues(urllist[i])
    if(all(is.null(values) == TRUE)) {
      datanames <- append(datanames,"NULL")
      datalist  <- rep(NA,24)
      dataframe <- cbind(dataframe, datalist) 
    } else {
      # Making sure the times match
      if(nrow(values["Date.Time"]) < 24) {
        # Do nothing
      } else {
        # Pulling the ThingSpeak Primary ID from the URL based on pattern matching 
        IDVal <- unlist(str_extract_all(urllist[i], "([\\d]){6}"))
        # Finding corresponding Lat/Lon values from "filtered" dataframe 
        LatVal <- as.numeric(filter(filtered, THINGSPEAK_PRIMARY_ID == IDVal)["Lat"])
        LonVal <- as.numeric(filter(filtered, THINGSPEAK_PRIMARY_ID == IDVal)["Lon"])
        sensorname <-values["Name"][1, ]
        # Building dataframe with Lat/Lon values based on their sensor name
        latframe[sensorname] <- rep(LatVal, 24)
        lonframe[sensorname] <- rep(LonVal, 24)
        datalist  <- values["PM2.5"]
        dataframe <- cbind(dataframe, datalist)
        datanames <- append(datanames,sensorname)
        datatime  <- values["Date.Time"]
      }
    }
    # Getting rid of the dummy variable and assigning columns 
    # the appropriate names
    dataframe$dummy <- NULL
    colnames(dataframe) <- datanames
  }
  for(i in 1:length(datanames)) {
    if(datanames[i] == "NULL") {
      # This looks at all of the sensor names, and if it doesnt end in "_b"
      # it assigns it to a vector named vector_a
      # these are all the A sensors
    } else if(str_extract_all(datanames[i], "_[b]") == "character(0)") {
      vector_a <- append(vector_a,datanames[i])
      #similar idea but for B sensors
    }  else if(str_extract_all(datanames[i], ".(?<!_b)$") == "character(0)") {
      vector_b <- append(vector_b,datanames[i])
    } else {
      
    }
  }
  # This now compares the list of A and B sensors and uses pattern matching to see if they
  # correspond to the same Purple Air device
  # If they do then a dataframe is created with A and B sensor PM 2.5 values
  # as well as Date/Time values, Lat and Lon values of the device
  # This then becomes an element of a list which is returned at the end
  for(i in 1:length(vector_a)) {
    if(grepl(vector_a[i], vector_b)[i] == TRUE) {
      veclist <- data.frame(datatime, lapply(dataframe[vector_a[i]], as.numeric), 
                            lapply(dataframe[grep(vector_a[i],vector_b, value = TRUE)], as.numeric), 
                            latframe[vector_a[i]], lonframe[vector_a[i]])
      colnames(veclist) <- c("Date/Time", vector_a[i], 
                             grep(vector_a[i], vector_b, value = TRUE),"Lat","Lon")
      totalveclist[[i]] <- veclist
      
    } else {
      
    }
  }
  return(totalveclist)
}




# RunTime<- function(url)
#
# Args:
#  url: "https://www.purpleair.com/json"
#
# Returns:
#  Time it takes to run MasterFunc
#
RunTime <- function(url) {
  start_time <- Sys.time()
  MasterFunc(url)
  end_time <- Sys.time()
  print(end_time - start_time)
}






