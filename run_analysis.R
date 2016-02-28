# setwd("~/Desktop/Data Science/Getting and Cleaning Data/Week 4/Assignment Week 4")
# Step 1: Libraries used
library(data.table)
library(dplyr)

#Step 2: Read the Train Data
train_data <- read.table("./data/train/X_train.txt")
# Should display 7352*61
dim(train_data)
head(train_data)
train_label <- read.table("./data/train/y_train.txt")
table(train_label)
train_subject <- read.table("./data/train/subject_train.txt")

#Step 3: Read the Test Data
test_data <- read.table("./data/test/X_test.txt")
#should display 2947*561
dim(test_data)
test_label <- read.table("./data/test/y_test.txt") 
table(test_label) 
test_subject <- read.table("./data/test/subject_test.txt")

# Step 4. Merges the train and test datasets to create one dataset.
join_data <- rbind(train_data, test_data)
# should display 10299*561
dim(join_data) 

join_label <- rbind(train_label, test_label)
# 10299*1
dim(join_label) 

join_subject <- rbind(train_subject, test_subject)
# should display 10299*1
dim(join_subject) 


# Step 5: Extracts only the measurements on the mean and SD for each measurement.
featureNames <- read.table("./data/features.txt")
# should display 561*2
dim(featureNames)  
meanStdInd <- grep("mean\\(\\)|std\\(\\)", featureNames[, 2])
# should display 66
length(meanStdInd)
join_data <- join_data[, meanStdInd]
# should display 10299*66
dim(join_data)
# remove "()"
names(join_data) <- gsub("\\(\\)", "", featureNames[meanStdInd, 2])
# capitalize M
names(join_data) <- gsub("mean", "Mean", names(join_data))
# capitalize S
names(join_data) <- gsub("std", "Std", names(join_data))
# remove "-" in column names 
names(join_data) <- gsub("-", "", names(join_data))

# Step 6: Uses descriptive activity names to name the activities in the dataset.
activityNames <- read.table("./data/activity_labels.txt")
activityNames[, 2] <- tolower(gsub("_", "", activityNames[, 2]))
substr(activityNames[2, 2], 8, 8) <- toupper(substr(activityNames[2, 2], 8, 8))
substr(activityNames[3, 2], 8, 8) <- toupper(substr(activityNames[3, 2], 8, 8))
activity_label <- activityNames[join_label[, 1], 2]
join_label[, 1] <- activity_label
names(join_label) <- "activity"

# Step 7: Appropriately labels the data set with descriptive activity names.
names(join_subject) <- "subject"
cleanedDataSet <- cbind(join_subject, join_label, join_data)
# should display 10299*68
dim(cleanedDataSet) 
# write out the 1st dataset
write.table(cleanedDataSet, "merged_data_set.txt") 

# Step 8: Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
# should display 30
subjectLength <- length(table(join_subject))
activityLength <- dim(activityNames)[1]
columnLength <- dim(cleanedDataSet)[2]
resultset <- matrix(NA, nrow=subjectLength*activityLength, ncol=columnLength) 
resultset <- as.data.frame(resultset)
colnames(resultset) <- colnames(cleanedDataSet)
row <- 1
for(i in 1:subjectLength) {
        for(j in 1:activityLength) {
                resultset[row, 1] <- sort(unique(join_subject)[, 1])[i]
                resultset[row, 2] <- activityNames[j, 2]
                boolean1 <- i == cleanedDataSet$subject
                boolean2 <- activityNames[j, 2] == cleanedDataSet$activity
                resultset[row, 3:columnLength] <- colMeans(cleanedDataSet[boolean1&boolean2, 
                                                                          3:columnLength])
                row <- row + 1
        }
}
head(resultset)
# write out the 2nd dataset
write.table(resultset, "data_with_avg.txt") 

# datanew <- read.table("./data_with_avg.txt")
# datanew[1:20, 1:3]
