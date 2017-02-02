# Getting and Cleaning Data Peer Reviewd Assignment

## You should create one R script called run_analysis.R that does the following:

### 1. Merges the training and the test sets to create one dataset
### 2. Extracts only the measurements on the mean and standard deviation for each measurement
### 3. Uses descriptive activity names to name the activities in the dataset
### 4. Appropriately labels the dataset with descriptive names
### 5. From the dataset in step 4, creates a second, independent tidy dataset with the average of each variable for each activity and each subject


# Download and Unzip files
if(!file.exists(".data")){dir.create("./data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl, destfile="./data/Dataset.zip")

unzip(zipfile="./data/Dataset.zip", exdir="./data")


# Bring in all the data
xTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

xTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

features <- read.table("./data/UCI HAR Dataset/features.txt")

activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")


# Assigning column names to train data
colnames(activityLabels) = c('activityId', 'activityType')

colnames(xTrain) = features[, 2]

colnames(yTrain) = "activityId";

colnames(subject_train) = "subjectId";


# Combine final train data
TrainData <- cbind(yTrain,subject_train,xTrain)
View(TrainData)


# Assign column names to test data
colnames(xTest) = features[,2]

colnames(yTest) = 'activityId';

colnames(subject_test) = "subjectId";


# Combine final test data
TestData <- cbind(yTest, subject_test, xTest)
View(TestData)


# Combine test and train data
AllData <- rbind(TrainData, TestData)
View(AllData)


# Create a vector of the "AllData" column names
ColNames <- colnames(AllData)


## Now get only the mean and standard deviation columns
MeanStd <- grepl("activityId", ColNames) | grepl("subjectId", ColNames) | grepl("mean..", ColNames) | grepl("std..", ColNames)

AllData2 <- AllData[MeanStd == TRUE]
View(AllData2)


# Merge "AllData2" with the ActivityLabels table
AllData3 <- merge(AllData2, activityLabels,by='activityId')
View(AllData3)


# Move activityType with activityId
library(dplyr)
AllData4 <- AllData3 %>% select(activityId, activityType, subjectId, `tBodyAcc-mean()-X`:`fBodyBodyGyroJerkMag-meanFreq()`)
View(AllData4)


# Create a second, tidy dataset with the mean of each variable

TidyData <- aggregate(. ~subjectId + activityId, AllData4, mean)
View(TidyData)

TidyData2 <- merge(TidyData, activityLabels, by='activityId')
View(TidyData2)

TidyData3 <- select(TidyData2, -activityType.x)
View(TidyData3)

FinalTidyData <- TidyData3%>% select(subjectId, activityId, activityType.y, `tBodyAcc-mean()-X`:`fBodyBodyGyroJerkMag-meanFreq()`)
View(FinalTidyData)

write.table(FinalTidyData, file = "TidyData.txt", row.names = FALSE)


