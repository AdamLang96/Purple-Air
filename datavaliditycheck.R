library(purrr)
library(lubridate)

#reading csv file 
AirGraphs <- read.csv(file = "/Users/adamlang/Desktop/APCD Stuff/APCDProgram/Purple Air API/PA_AvB.csv")

#round it to 9000 observations to make my life easier later
AirGraphs <- AirGraphs[1:9000, ]

#getting rid of date column and will just go by index
AirGraphs$date <- NULL

##takes Pair data groups it by sensor A and B and then slices each grouping into
##groups of 100 hours or roughly 4 days
##splitby4 is a list with 9 elements, each being a sensor location.
#each element is a list of 90 100 hour comparisons between A and B sensors at a given location

SplittingMap <- lapply(seq(1,  ncol(AirGraphs),  by=2), function(i) { 
AirGraphs[i: pmin((i+1), ncol(AirGraphs))]})
SplitBy90 <- map(SplittingMap, split, rep(1:90, each=100))
##from here we can look at our data and see what an r square value should look like
##since each element of splitby4 is a list it is converted to a dataframe before any computations are made
## this loop checks whether the amount of NA values is greater than 50% of the dataframe and if so
#returns "indeterminant and if not returns the r squared value and slope value
#slope is computed by multiplying t value (coefficent[3] in summary(lm)) 
#and standard error(coefficent[2] in summary(lm))

RSquareValues <- c()
SlopeValues <- c()
for (i in 1:9){
  for (j in 1:90){
    if((sum(is.na(as.data.frame(SplitBy90[[i]][j])))/200) >= .5) {
      RSquareValues <- append(RSquareValues, "indeterminant")
      SlopeValues <-append(SlopeValues, "indeterminant")
    } else {
      RSquareValues <- append(RSquareValues, summary(lm(as.data.frame(SplitBy90[[i]][j])[,1] ~ 0+as.data.frame(SplitBy90[[i]][j])[,2]))$r.squared)
      SlopeValues <- append(SlopeValues,
                            (summary(lm(as.data.frame(SplitBy90[[i]][j])[,1] ~ 0+as.data.frame(SplitBy90[[i]][j])[,2]))$coefficients[3])*
                              (summary(lm(as.data.frame(SplitBy90[[i]][j])[,1] ~ 0+as.data.frame(SplitBy90[[i]][j])[,2]))$coefficients[2]))
    }
  }
}

##dataframe comparing rsqaured value vs slope for each 100 hour period of an A and B sensor
RegressionAndSlope<-data.frame(RSquareValues,SlopeValues)
RegressionAndSlope[1:200,]

SplitBy90[[1]][1]
