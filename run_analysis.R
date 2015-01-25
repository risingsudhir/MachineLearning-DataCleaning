
# Script reads the test and training data
# Merges the training and test data sets to create a single data set.
# It extracts only the measurements on the mean and standard deviation for each measurement. 
# Generates a tidy data set with the average of each variable for each activity and each subject.

library(plyr)
library(reshape2)

# Helper method to rename columns
renameFeatures <- function(col) {
    col <- gsub("tBody", "TimeBody", col)
    col <- gsub("tGravity", "TimeGravity", col)
    col <- gsub("fBody", "FTBody", col)
    col <- gsub("fGravity", "FTGravity", col)
    col <- gsub("\\-mean\\(\\)\\-", ".Mean.", col)
    col <- gsub("\\-std\\(\\)\\-", ".Std.", col)
    col <- gsub("\\-mean\\(\\)", ".Mean", col)
    col <- gsub("\\-std\\(\\)", ".Std", col)
    
    col
}

data <- list()

# Read features file that has feature id and name
print("reading features.txt file...")
data$Features <- read.table("UCI HAR Dataset/features.txt", col.names = c('ID', 'Feature'))

# Read activity file
print("reading activity_labels.txt file...")
data$Activity <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c('ID', 'Activity'))

# Read test data
print("reading test/subject_test.txt file...")
subject <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = 'Subject')

print("reading test/y_test.txt file...")
y <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "ID.Activity")

print("reading test/X_test.txt file...")
x <- read.table("UCI HAR Dataset/test/X_test.txt")

data$Test <- cbind(subject, y, x)

#Read training data
print("reading train/subject_train.txt file...")
subject <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = 'Subject')

print("reading train/y_train.txt file...")
y <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "ID.Activity")

print("reading tain/X_train.txt file...")
x <- read.table("UCI HAR Dataset/train/X_train.txt")

data$Train <- cbind(subject, y, x)

print("merging test and training data...")

#extract only mean and standard deviations
featureIndexes <- grep("mean\\(\\)|std\\(\\)", data$Features$Feature)

data$Test <- data$Test[, c(1, 2, featureIndexes + 2)]
data$Train <- data$Train[, c(1, 2, featureIndexes + 2)]

data$Tidy <- rbind(data$Test, data$Train)

print("generating tidy data set...")
# set description name for data set columns
names(data$Tidy) <- c("Subject", "ID.Activity", renameFeatures(data$Features$Feature[featureIndexes]))

# set activity labels
data$Tidy <- merge(data$Tidy, data$Activity, by.x = "ID.Activity", by.y = "ID")

# take out "ID.Activity column - not required any more
data$Tidy <- data$Tidy[, !(names(data$Tidy) %in% "ID.Activity")]

# now, we need another dataset that has average of each variable for each activity and each subject.
# first, mel the data set by activity and subject
meltedData <- melt(data$Tidy, id.var = c("Subject", "Activity"))

# take average for each measurement
data$TidyMean <- ddply(meltedData, .(Subject, Activity), summarise, MeanSamples=mean(value))

print("saving datasets to data_tidy.txt, data_tidy_mean.txt...")
# tidy data sets
write.table(data$Tidy, "data_tidy.txt", row.names = FALSE)
write.table(data$TidyMean, "data_tidy_mean.txt", row.names = FALSE)

print("Done.")


