# Coursera_Getting and Cleaning Data_Project_Week3
## Purpose of the project
#### The prupose of this project is to obtain data sets from a web and merge & clean the data to product a tidy data set.
#### The data sets can be downloaded from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#### The information for the data sets can be found here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## How the script works
#### 1. Download the data sets from the link above.
#### 2. Open and run the *run_analysis.R*
#### 3. You may need to set the working directory to where you saved the data and the *run_analysis.R* file.

## Explanaton on the scripts
#### 1. Load required packages
```
library(reshape2);library(data.table); library(Hmisc)
```
#### 2. Merges the training and the test sets to create one data set.
```
testX <- read.table("./test/X_test.txt")
testY <- read.table("./test/y_test.txt")
testSubj <- read.table("./test/subject_test.txt")
trainX <-read.table("./train/X_train.txt")
trainY <-read.table("./train/y_train.txt")
trainSubj <- read.table("./train/subject_train.txt")
dataX <- rbind(trainX, testX)
dataY <- rbind(trainY, testY)
dataSubj <- rbind(trainSubj, testSubj)
Data <- cbind(dataSubj,dataY, dataX)
```

#### 3. Extracts only the measurements of the mean and standard deviation for each measurement. 
```
features <- read.table("features.txt", colClasses = c("integer", "character"))
features <- features[c(grep("mean()", fixed = TRUE, features$V2),grep("std()", fixed = TRUE, features$V2)),]
```
#### 4. Uses descriptive activity names to name the activities in the data set
```
colnames(Data)[2] <- "activity"
activityLabels <- read.table("activity_labels.txt", colClasses = c("integer", "character"))
for(i in 1:nrow(activityLabels)){
    Data$activity <- replace(Data$activity, Data$activity == i, activityLabels$V2[i])
}
```
#### 5. Appropriately labels the data set with descriptive variable names. 
```
Data<- cbind(Data[1:2], Data[,features$V1+2])
colnames(Data)[3:ncol(Data)] <- features$V2
colnames(Data)[1] <- "subject"
```
#### 6. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
```
Data <- melt(Data, id = c("subject","activity"), measure.vars = c(colnames(Data)[3:ncol(Data)]),na.rm = TRUE)
Data <- dcast(Data, subject + activity ~ variable, mean)
Data <- melt(Data, id = c("subject","activity"), measure.vars = c(colnames(Data)[3:ncol(Data)]),na.rm = TRUE)
Data$variable <- as.character(Data$variable)

tempCol <- data.frame(tempCol = NA)

splitVar <- function(Data, feat1, feat2 = "NULL", feat3 = "NULL", colName){
    x<- grep(feat1, fixed = TRUE, Data$variable)
    y<- grep(feat2, fixed = TRUE, Data$variable)
    z<- grep(feat3, fixed = TRUE, Data$variable)
    Data <- cbind(Data, tempCol)
    colnames(Data)[ncol(Data)] <- colName
    Data[,ncol(Data)] <- replace(Data[,ncol(Data)], x, feat1)
    Data[,ncol(Data)] <- replace(Data[,ncol(Data)], y, feat2)
    Data[,ncol(Data)] <- replace(Data[,ncol(Data)], z, feat3)
    return(Data)
}

Data <- splitVar(Data, "Acc", "Gyro", , "device")
Data <- splitVar(Data, "t", "f", ,"domainSignal")
Data <- splitVar(Data, "Body", "Gravity", ,"accFeat")
Data <- splitVar(Data, "Jerk", , ,"jerkFeat")
Data <- splitVar(Data, "Mag", , ,"magFeat")
Data <- splitVar(Data, "x","y","z","axis")
Data <- splitVar(Data, "std","mean",,"var")

Data <- cbind(Data, Data$value); colnames(Data)[ncol(Data)] <- "value"
Data$variable <- NULL; Data$value <- NULL
```
