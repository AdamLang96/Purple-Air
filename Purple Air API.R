library(purrr)
library(lubridate)
library(RCurl)
library(data.table)
library(stringr)

#Filtering for just sensors in SLO and the variables of interest
#Takes in just https://www.purpleair.com/json
SloFilter <- function(url) {
raw <- RCurl::getURL(url)    
datadump <- jsonlite::fromJSON(raw)
dumpresults <- datadump$results 
slo <- dumpresults[dumpresults$Lon > -120.88 & dumpresults$Lon < -120.30 
            & dumpresults$Lat > 34.97 & dumpresults$Lat < 35.79 
            & is.na(dumpresults$Lat) == FALSE, ]
keepers=c("ID","Label","THINGSPEAK_PRIMARY_ID","THINGSPEAK_PRIMARY_ID_READ_KEY",
          "THINGSPEAK_SECONDARY_ID","THINGSPEAK_SECONDARY_ID_READ_KEY","Lat","Lon","PM2_5Value","LastSeen")
newdata <- slo[keepers]
return(newdata)
}


#Creates single url for sensor data based off Thingspeak ID and Key
CreateURL <- function(id, key){
  url <- paste0("https://api.thingspeak.com/channels/", id,  
                "/feeds.json?api_key=", key,
                "&average=60&days=1")
}

#Creates list of urls based off Thingspeak ID and Key
#Written with createurl
MapThrough <- function(df){
  arg1 <- df["THINGSPEAK_PRIMARY_ID"]
  arg2 <- df["THINGSPEAK_PRIMARY_ID_READ_KEY"]
  urlist <- map2_dfr(arg1, arg2, CreateURL)
  urlist <- as.data.frame(urlist)
  urlist <- as.list(urlist$THINGSPEAK_PRIMARY_ID)
  return(urlist)
}



#Takes in a single URL with Thingspeak ID and Key and returns dataframe with
#Date/Time, PM2.5, Humidity, and Temperature
#First row is the previous hour so that the entire hours average is considered
ExtractValues <- function(url) {
  rawinfo <- RCurl::getURL(url)
  parseddata <- jsonlite::fromJSON(rawinfo)
  if(length(parseddata$feeds)==0){
    cleandata <-NULL
  } else {
  parseddata$feeds$created_at <- as.POSIXct(parseddata$feeds$created_at, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")-8*60*60
  parseddata$feeds$created_at <- force_tz(parseddata$feeds$created_at, tzone = "Etc/GMT+8")
  cleandata<-data.frame("Date-Time"=parseddata$feeds$created_at,"PM2.5"=parseddata$feeds$field2,
                        "Humidity"=parseddata$feeds$field7,"Temperature"=parseddata$feeds$field6,
                        "Name"=parseddata$channel$name, "Latitude"=parseddata$channel$latitude,
                        "Longitude"=parseddata$channel$longitude, stringsAsFactors = FALSE)
  cleandata<-cleandata[order(nrow(cleandata):2),]
  }
  return(cleandata)
 }


##checks how long it takes to run MasterFunc
RunTime <- function(url) {
 start_time <- Sys.time()
 MasterFunc(url)
 end_time <- Sys.time()
 print(end_time - start_time)
}



####IT WORKS!!! NO ERRORS
##need to add test to make sure all hours have been brought back
## x variable is example output
MasterFunc <- function(url) {
  #creating a bunch of empty vectors and lists for later use
  datalist <- c()
  datanames <- c()
  vector_a <- c()
  vector_b <- c()
  totalveclist <- list()
  #since each column has 24 rows and you have to append columns
  #to a dataframe of equal size, i created a length 24 dummy variable
  dataframe <- data.frame("dummy"=1:24)
  filtered <- SloFilter(url)
  urllist <- MapThrough(filtered)
  #creating a vector of all sensor names and a df of each column by iterating the
  #extract values function over all the URLS
  for(i in 1:length(urllist)) {
    if(all(is.null(ExtractValues(urllist[[i]][1]))==TRUE)){
      datanames <- append(datanames,"NULL")
      datalist <- rep(NA,24)
      dataframe <- cbind(dataframe, datalist) 
    } else {
      sensorname <- ExtractValues(urllist[[i]][1])["Name"][1,]
      datanames <- append(datanames,sensorname)
      datalist <- ExtractValues(urllist[[i]][1])["PM2.5"]
      dataframe <- cbind(dataframe, datalist)  
    }
    #getting rid of the dummy variable and assigning columns 
    #the appropriate names
    dataframe$dummy <- NULL
    colnames(dataframe) <- datanames
  }
  for(i in 1:length(datanames)) {
    if(datanames[i]=="NULL"){
      #This looks at all of the sensor names, and if it doesnt end in "_b"
      #it assigns it to a vector named vector_a
      # hese are all the A sensors
    } else if(str_extract_all(datanames[i],"_[b]")=="character(0)") {
      vector_a<-append(vector_a,datanames[i])
      #similar idea but for B sensors
    }  else if(str_extract_all(datanames[i],".(?<!_b)$")=="character(0)") {
      vector_b<-append(vector_b,datanames[i])
    } else {
      
    }
  }
  #this now compares the list of A and B sensors and uses pattern matching to see if they
  #correspond to the same Purple Air device
  #if they do then a dataframe is created where one column is the A values and
  #the other is the B values. 
  #This then becomes an element of a list which is returned at the end
  for(i in 1:length(vector_a)) {
    if(grepl(vector_a[i],vector_b)[i]==TRUE){
      veclist<-data.frame(dataframe[vector_a[i]], dataframe[grep(vector_a[i],vector_b, value = TRUE)])
      colnames(veclist)<-c(vector_a[i], grep(vector_a[i],vector_b, value = TRUE))
      totalveclist[[i]]<-veclist
    } else {
      
    }
  }
  return(totalveclist)
}

