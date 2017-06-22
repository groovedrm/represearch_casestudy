setwd("/users/chris/development/r/represearch_casestudy")

# Loading Up Libraries/Packages
is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}

if (!is.installed("data.table")){
  install.packages("data.table")
}

if (!is.installed("plyr")){
  install.packages("plyr")
}

if (!is.installed("dplyr")){
  install.packages("dplyr")
}

if (!is.installed("ggplot2")){
  install.packages("ggplot2")
}

library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)

unitConverter <- function(v) {
  v <- as.character(v)
  if (v %in% c("h","H")) {
    return(2)
  }
  else if (v %in% c("k","K")) {
    return(3)
  }
  else if (v %in% c("m","M")) {
    return(6)
  }
  else if (v %in% c("b","B")) {
    return(9)
  }
  else if (as.numeric(v %in% c(1:9))) {
    return(as.numeric(v))
  }
  else {
    return(0)
  }
}

# If data doesn't already exist, go get it.
if (!file.exists("repdata-data-StormData.csv")) {
  zipURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(zipURL, "repdata-data-StormData.csv")
  unzip(tmp, "repdata-data-StormData.csv")
}

# Load Up Data
sourceDir <- getSrcDirectory(function (d) {d})
dirForFile <- paste(sourceDir,"/","repdata-data-StormData.csv", sep = "")
baseData <- read.table(dirForFile, header=TRUE, sep=",", stringsAsFactors = TRUE)
distilledData <- baseData[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
propUnit <- sapply(distilledData$PROPDMGEXP, FUN = unitConverter)
distilledData$propDamage <- distilledData$PROPDMG * (10 ** propUnit)
cropUnit <- sapply(distilledData$CROPDMGEXP, FUN = unitConverter)
distilledData$cropDamage <- distilledData$CROPDMG * (10 ** cropUnit)

# Aggregate and then reorder to get the top events for fatalities and injuries
healthBaseTable <- ddply(distilledData, .(EVTYPE), summarize, fatalities = sum(FATALITIES), injuries = sum(INJURIES))
injuriesByEvent <- healthBaseTable[order(-healthBaseTable$injuries), ]
fatalitiesByEvent <- healthBaseTable[order(-healthBaseTable$fatalities), ]
topInjuriesByEvent <- injuriesByEvent[1:10, ]
topFatalitiesByEvent <- fatalitiesByEvent[1:10, ]

# Plot the health-based consequences in using the plot code below.
par(mfrow=c(2,1))
p1 <- ggplot(data = topInjuriesByEvent, aes(x = reorder(EVTYPE, -injuries), y = injuries)) +
              geom_bar(fill="Blue", stat="identity") +
              coord_flip() +
              theme_minimal()

p2 <- ggplot(data = topFatalitiesByEvent, aes(x = reorder(EVTYPE, -fatalities), y = fatalities)) +
              geom_bar(fill="Red", stat="identity") +
              coord_flip() +
              theme_minimal()

# Ok, now measure the economic losses using the damage estimates. 
economicBaseTable <- ddply(distilledData, .(EVTYPE), summarize, propDamage = sum(propDamage), cropDamage = sum(cropDamage))
propDmgByEvent <- economicBaseTable[order(-economicBaseTable$propDamage), ]
topPropDmg <- propDmgByEvent[1:10, ]
names(topPropDmg) <- c("EventType","PropertyDamage","CropDamage")
cropDmgByEvent <- economicBaseTable[order(-economicBaseTable$cropDamage), ]
topCropDmg <- cropDmgByEvent[1:10, ]
names(topCropDmg) <- c("EventType","PropertyDamage","CropDamage")

# Now, plot the economics consequences
png(filename = "plot_panel_2.png", width = 480, height = 480)
par(mfrow=c(2,1))
#p3
ggplot(topPropDmg, aes(x = reorder(EventType, -PropertyDamage), y = PropertyDamage)) +
              geom_bar(fill = "Blue", stat="identity") +
              coord_flip() +
              theme_minimal()
#p4
ggplot(topCropDmg, aes(x = reorder(EventType, -CropDamage), y = CropDamage)) +
              geom_bar(fill = "Red", stat="identity") + 
              coord_flip() +
              theme_minimal()

dev.off()
 



