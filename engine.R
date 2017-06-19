setwd("/users/chris/development/r/represearch_casestudy")

# If data doesn't already exist, go get it.
if (!file.exists("repdata-data-StormData.csv")) {
  zipURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(zipURL, "repdata-data-StormData.csv")
  unzip(tmp, "repdata-data-StormData.csv")
}

# Load Up Data
sourceDir <- getSrcDirectory(function (d) {d})
dirForFile <- paste(sourceDir,"/","repdata-data-StormData.csv", sep = "")
baseData <- fread(dirForFile, sep="auto", stringsAsFactors = TRUE)