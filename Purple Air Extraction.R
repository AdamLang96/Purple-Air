#Libraries 
#Need dplyr 0.7.7 or newer
library(purrr)
library(lubridate)
library(RCurl)
library(stringr)
library(dplyr)
library(jsonlite)



#################################### Script ####################################

pa.url   <- "https://www.purpleair.com/json"
datalist <- MasterFunc(pa.url)
logvec   <- datalist  %>% ValidityCheck %>% ValidityLog
slofilt  <- SloFilter(pa.url) 
for(i in 1:length(logvec)) {
  if(logvec[i] == FALSE) {
    datalist <- datalist[-i]
  }
}
finalcheck <- PA.AQI(datalist)


#################################### Functions #################################### 

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
  keepers = c("ID","Label", 
              "THINGSPEAK_PRIMARY_ID", 
              "THINGSPEAK_PRIMARY_ID_READ_KEY",
              "THINGSPEAK_SECONDARY_ID", 
              "THINGSPEAK_SECONDARY_ID_READ_KEY", 
              "Lat", "Lon", "PM2_5Value", "LastSeen")
  newdata <- slo[keepers]
  newdata <- newdata[newdata$LastSeen >= as.numeric(Sys.time()) - 60*60*24, ]
  return(newdata)
}


########################################################################

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
CreateURL <- function(id, key) {
  url <- paste0("https://api.thingspeak.com/channels/", id,  
                "/feeds.json?api_key=", key,
                "&average=60&days=1")
}



########################################################################

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
MapThrough <- function(df) {
  arg1 <- df["THINGSPEAK_PRIMARY_ID"]
  arg2 <- df["THINGSPEAK_PRIMARY_ID_READ_KEY"]
  urlist <- mapply(CreateURL, arg1, arg2)
  return(urlist)
}


########################################################################

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
    cleandata <- data.frame("Date" = parseddata$feeds$created_at, 
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


########################################################################

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
  # Initializing dataframes of appropriate size for later use
  dataframe <- data.frame("dummy" = rep(NA, 24))
  latframe  <- data.frame("dummy" = rep(NA, 24))
  lonframe  <- data.frame("dummy" = rep(NA, 24))
  filtered  <- SloFilter(url)
  urllist   <- MapThrough(filtered)
  # Creating a vector of all sensor names and a df of each column by iterating the
  # Extract values function over all the URLS
  for(i in 1:length(urllist)) {
    values <- ExtractValues(urllist[i])
    # Checking to see if any sensors are returning no information
    if(any(mapply(is_empty, values))) {
      datanames <- append(datanames,"NULL")
      datalist  <- rep(NA, 24)
      dataframe <- cbind(dataframe, datalist) 
    } else {
      if(nrow(values) < 24) {  # appending NA r
        values[(nrow(values)+1):24, ] <- NA
      } 
        # Pulling the ThingSpeak Primary ID from the URL based on pattern matching 
        IDVal <- unlist(str_extract_all(urllist[i], "([\\d]){6}"))
        # Finding corresponding Lat/Lon values from "filtered" dataframe 
        LatVal <- as.numeric(filter(filtered, THINGSPEAK_PRIMARY_ID == IDVal)["Lat"])
        LonVal <- as.numeric(filter(filtered, THINGSPEAK_PRIMARY_ID == IDVal)["Lon"])
        sensorname <-values["Name"][1, ]
        datalist  <- values["PM2.5"]
        datatime  <- values["Date"]
        # Building dataframe with Lat/Lon values based on their sensor name
        latframe[sensorname] <- rep(LatVal, 24)
        lonframe[sensorname] <- rep(LonVal, 24)
        dataframe <- cbind(dataframe, datalist)
        datanames <- append(datanames, sensorname)
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
    if(any(grepl(vector_a[i], vector_b), na.rm = TRUE)) {
      veclist <- data.frame(datatime, lapply(dataframe[vector_a[i]], as.numeric), 
                            lapply(dataframe[grep(vector_a[i], vector_b, value = TRUE)], as.numeric), 
                            latframe[vector_a[i]], lonframe[vector_a[i]])
      colnames(veclist) <- c("Date", vector_a[i], 
                             grep(vector_a[i], vector_b, value = TRUE),"Lat","Lon")
      totalveclist[[i]] <- veclist
      
    } else {
      
    }
  }
 
  return(totalveclist)
}


########################################################################

# RunTime <- function(url)
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

########################################################################

# ValidityCheck <- function(datalist)
#
# Args:
#  Output from MasterFunc
#  List of dataframes by PurpleAir device
#  Each df contains sensor a and sensor b
#
# Returns:
#  DF with each row corresponding to a device
#  Each row determines Rsquare val and slope of regression
#
ValidityCheck <- function(datalist) {
  RSquareValues <- c()
  SlopeValues   <- c()
  for(i in 1:length(datalist)) {
    if((sum(is.na(as.data.frame(datalist[i]))) / 24) >= .5) {
      RSquareValues <- append(RSquareValues, 0)
      SlopeValues   <-append(SlopeValues, 0)
    } else {
      regressionstats <- lm(as.matrix(datalist[[i]][2]) ~ 0 + as.matrix(datalist[[i]][3]))
      RSquareValues <- append(RSquareValues, as.numeric(summary(regressionstats)$r.squared))
      SlopeValues <- append(SlopeValues, as.numeric(coef(regressionstats)))
    }
  }
  df <- data.frame("RSquared" = RSquareValues, 
                  "Slope"     = SlopeValues)
  return(df)
}

########################################################################

# ValidityLog <- function(df)
#
# Args:
#  Output from ValidityCheck
#  DF with Rsqaure and slope of regression between a and b sensor
#  Each row corresponds to PA device
#
# Returns:
#  T/F per row depending on if 
#  both Rsquare and slope fall between set values
#  
ValidityLog <- function(df) {
  logvec <- with(df, Slope > .8 & Slope < 1.3, 
       RSquared >= .93)
  return(logvec)
}


########################################################################

AQICalc <- function(vect)  {
  # Takes a vector of PM10 values and
  # Calculates the 12 hour NowCast AQI
  # according to EPA specifications
  # Assumes PM values ordered with
  # most recent value first and decreasing
  # by time
  #
  # Args:
  #  vector of PM10 values
  #
  # Returns:
  #   NowCast AQI
  #
  num <- 0
  dem <- 0
  PM10 <- vect[1:12]
  if(sum(is.na(PM10[1:3])) >= 2) {
    return(NA)
  }
  WeightFactor <- (min(na.omit(PM10)) / max(na.omit(PM10)))
  #min max function cant handle NA
  #guarenteed at least two values so min/max will work
  if(WeightFactor < 1/2) {
    WeightFactor  <- 1/2
  }
  for(i in 1:length(PM10)) {
    if(!is.na(PM10[i])) {
      num <- num + (PM10[i] * (WeightFactor ^ (i-1)))
      dem <- dem + (WeightFactor ^ (i-1))
    } else { #do nothing 
      #skips step in the iteration
    }
  }
  AQI <- Converter(num / dem)
  return(AQI)
}

########################################################################

Converter <- function(x, table = AQILookUpTable, pol = "PM2.5") {
  #Takes in a NowCast concentration, AQIlookuptable, and polutant
  #Returns the NowCast AQI value
  #Used in AQICalc
  #
  #Args:
  # Nowcast AQI concentration for PM10 and AQIlookuptable
  #
  #Returns:
  # NowCastAQI
  stopifnot(is.numeric(x), length(x) == 1)
  if(is.na(x)) return (NA)
  AQILo  <- table$AQILo[max(which(x >= table[, pol]))]
  AQIHi  <- table$AQIHi[max(which(x >= table[, pol]))]
  ConcLo <- table[, pol][max(which(x >= table[, pol]))]
  ConcHi <- table[, pol][max(which(x >= table[, pol])) + 1] - 1
  AQI    <- round((((AQIHi-AQILo) / (ConcHi-ConcLo)) * (x-ConcLo)) + AQILo)
  return(AQI)
}
########################################################################

#DataFrame for comparing NowCast concentrations
#to AQI values
AQILookUpTable <- data.frame(Ozone = c(0, 55, 71, 86, 106, 201, NA, NA), 
                             PM2.5 = c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5, 500.4),
                             PM10  = c(0, 55, 155, 255, 355, 425, 505, 604),
                             so2   = c(0, 36, 76, 186, 305, 605, 805, 1004),
                             AQILo = c(0, 51, 101, 151, 201, 301, 401, NA),
                             AQIHi = c(50, 100, 150, 200, 300, 400, 500, NA))


########################################################################

# PA.AQI <- function(datalist)
# Takes MasterFunc output and returns
# a dataframe with each row corresponding
# to a device
#
# Args:
#  MasterFunc output
#
# Returns:
# a dataframe with each row corresponding
# to a device with cols:
#              Site
#              Date
#              AQI
#              Latitude
#              Longitude
#
PA.AQI <- function(datalist) {
  data <- data.frame("Site" = NA,
                     "Date" = NA,
                     "AQI"  =  NA,
                     "Latitude"  = NA,
                     "Longitude" = NA,
                     stringsAsFactors = FALSE)
 for(i in 1:length(datalist)) {
  df   <- datalist[[i]]   
  drops <- c("Lat", "Lon", "Date")
  cols  <- colnames(df)
  pmvec <- df[, 2]
  AQI   <- AQICalc(pmvec)
  sitename <- Site(df, slofilt)
  vec   <- data.frame( "Date" = as.character(df$Date[1]),
                       "Site" = sitename,
                       "AQI"  = AQI,
                       "Latitude"  = as.numeric(as.character(df$Lat[1])),
                       "Longitude" = as.numeric(as.character(df$Lon[1])))
  attributes(data$Date) <- attributes(df$Date)
  data <- rbind(data, vec)
 }
return(data)
}


########################################################################

# Site <- function(df, slofilt) 
# Changes the name from "Air_Monitor ...." to the name
# corresponding to its location
# When requesting data from specific sensors
# undesirable name is produced. This function
# cross references with SloFilter output
# to get the better name by matching Lat/Lon vals
#
# Args:
#  df: dataframe for individual sensor
#  slofilt: output from SloFilter
# 
# Returns:
# Corrected name for sensor
#
Site <- function(df, slofilt) {
dfLat    <- df[["Lat"]][1]
sitename <-filter(slofilt, 
                  abs(Lat - dfLat) < 0.000005)["Label"]
return(sitename[1, ])
}

########################################################################




