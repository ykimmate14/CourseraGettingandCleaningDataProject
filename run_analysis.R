# You should create one R script called run_analysis.R that does the following. 

library(reshape2);library(data.table); library(Hmisc)
##1. Merges the training and the test sets to create one data set.

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

##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("features.txt", colClasses = c("integer", "character"))
features <- features[c(grep("mean()", fixed = TRUE, features$V2),grep("std()", fixed = TRUE, features$V2)),]


##3. Uses descriptive activity names to name the activities in the data set
colnames(Data)[2] <- "activity"
activityLabels <- read.table("activity_labels.txt", colClasses = c("integer", "character"))
for(i in 1:nrow(activityLabels)){
    Data$activity <- replace(Data$activity, Data$activity == i, activityLabels$V2[i])
}


##4. Appropriately labels the data set with descriptive variable names. 
Data<- cbind(Data[1:2], Data[,features$V1+2])
colnames(Data)[3:ncol(Data)] <- features$V2
colnames(Data)[1] <- "subject"


##5. From the data set in step 4, creates a second, independent tidy data set with the 
##   average of each variable for each activity and each subject.
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


