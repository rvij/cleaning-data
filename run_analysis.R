setwd("/Users/rajeevvij/Documents/DocRep/~Coursera/Cleaning_Data/UCI_HAR_Dataset")
require(sqldf)

# Reading data from the given files into R
readFromFiles <- function() {
  subjectTest <<- read.table("./test/subject_test.txt", header=F, col.names=c("SubjectID"))
  subjectTrain <<- read.table("./train/subject_train.txt", header=F, col.names=c("SubjectID"))
  
  # We will read the column names from the features.txt. The file has two columns and only the second column is relevant.
  data_cols <<- read.table("features.txt", header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  
  xTest <<- read.table("./test/X_test.txt", header=F, col.names=data_cols$MeasureName)
  xTrain <<- read.table("./train/X_train.txt", header=F, col.names=data_cols$MeasureName)
  
  yTest <<- read.table("./test/y_test.txt", header=F,col.names=c("ActivityID"))
  yTrain <<- read.table("./train/y_train.txt", header=F, col.names=c("ActivityID"))
  
  activityLabels <<- read.table("./activity_labels.txt", header=F, as.is=T, col.names=c("ActivityID", "ActivityName"))
  activityLabels$ActivityName <- as.factor(activityLabels$ActivityName)
  
#  features <<- read.table("./features.txt")
}

createDataSet <- function() {
  xData <- rbind(xTest, xTrain)
#  names(xData) <<- features$V2
  yData <- rbind(yTest, yTrain)
  subject <- rbind(subjectTest, subjectTrain)
  data <- cbind(xData, yData, subject)
  names(data)[562] <- "ActivityID"
  names(data)[563] <- "SubjectID"
  # return data frame to caller
  data <- sqldf('SELECT a.*, b.ActivityName as ActivityName FROM data a JOIN activityLabels b USING(ActivityID)')
}


# Add dimensions (X, Y, Z) to duplicated column names
addDimensionsToDuplicatedColumns <- function() {
  # which(duplicated(names(data))) - is the instruction through which we got the duplicated columns
  
  for (i in 303:316) {
    colnames(data)[i] <<- paste(colnames(data)[i], "X", sep="")
  }
  for (n in 317:330) {
    colnames(data)[i] <<- paste(colnames(data)[i], "Y", sep="")
  }
  for (n in 331:344) {
    colnames(data)[i] <<- paste(colnames(data)[i], "Z", sep="")
  }
  for (n in 382:395) {
    colnames(data)[i] <<- paste(colnames(data)[i], "X",  sep="")
  }
  for (n in 396:409) {
    colnames(data)[i] <<- paste(colnames(data)[i], "Y", sep="")
  }
  for (n in 410:423) {
    colnames(data)[i] <<- paste(colnames(data)[i], "Z", sep="")
  }
  for (n in 461:474) {
    colnames(data)[i] <<- paste(colnames(data)[i], "X",  sep="")
  }
  for (n in 475:488) {
    colnames(data)[i] <<- paste(colnames(data)[i], "Y", sep="")
  }
  for (n in 489:502) {
    colnames(data)[i] <<- paste(colnames(data)[i], "Z", sep="")
  }
}


# Obtain the subset of measurements on the mean and standard deviation
getSummaryStatistics <- function() {
  meanColumnNum <<- grep("[Mm]ean", colnames(data))
  standardDeviationColumnNum <<- grep("[Ss]td", colnames(data))
  subData <<- data[, c(meanColumnNum, standardDeviationColumnNum, 562, 563, 564)]
  
  require(data.table)
  dataTable <<- data.table(subData)

  meanData <<- dataTable[, lapply(.SD, mean), by=c("SubjectID", "ActivityID", "ActivityName")]
  meanData <<- meanData[order(meanData$SubjectID),]    
}


# Exporting the data into a text file
createTidyDataFile <- function() {
  write.table(meanData, "tidyData.txt", sep="\t", row.names = FALSE)
}

readFromFiles()
# describeActivities()
data <- createDataSet()
addDimensionsToDuplicatedColumns()
getSummaryStatistics()
createTidyDataFile()
