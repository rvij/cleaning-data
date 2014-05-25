# Reading data from the given files into R
readFromFiles <- function() {
  subjectTest <<- read.table("./test/subject_test.txt")
  subjectTrain <<- read.table("./train/subject_train.txt")
  
  xTest <<- read.table("./test/X_test.txt")
  xTrain <<- read.table("./train/X_train.txt")
  
  yTest <<- read.table("./test/y_test.txt")
  yTrain <<- read.table("./train/y_train.txt")
  
  activityLabels <<- read.table("./activity_labels.txt")
  features <<- read.table("./features.txt")
}

# Applying descriptive names the activity levels
describeActivities <- function() {
  yTest$V1[yTest$V1==1] <<- "Walk"
  yTest$V1[yTest$V1==2] <<- "Walk_Upstairs"
  yTest$V1[yTest$V1==3] <<- "Walk_Downstairs"
  yTest$V1[yTest$V1==4] <<- "Sit"
  yTest$V1[yTest$V1==5] <<- "Stand"
  yTest$V1[yTest$V1==6] <<- "Lay"
  
  yTrain$V1[yTrain$V1==1] <<- "Walk"
  yTrain$V1[yTrain$V1==2] <<- "Walk_Upstairs"
  yTrain$V1[yTrain$V1==3] <<- "Walk_downstairs"
  yTrain$V1[yTrain$V1==4] <<- "Sit"
  yTrain$V1[yTrain$V1==5] <<- "Stand"
  yTrain$V1[yTrain$V1==6] <<- "Lay"
}

# Creating full data set
createDataSet <- function() {
  xData <<- rbind(xTest, xTrain)
  names(xData) <<- features$V2
  activity <<- rbind(yTest, yTrain)
  subject <<- rbind(subjectTest, subjectTrain)
  data <- cbind(xData, activity, subject)
  names(data)[562] <- "activity"
  names(data)[563] <- "subject"
  data
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
  subData <<- data[, c(meanColumnNum, standardDeviationColumnNum, 562, 563)]
  
  library(data.table)
  dataTable <<- data.table(subData)
  meanData <<- dataTable[, lapply(.SD, mean), by=c("subject", "activity")]
  meanData <<- meanData[order(meanData$subject),]    
}

# Exporting the data into a text file
export <- function() {
  write.table(meanData, "tidyData.txt", sep="\t")
}

readFromFiles()
describeActivities()
data <- createDataSet()
addDimensionsToDuplicatedColumns()
getSummaryStatistics()
export()
