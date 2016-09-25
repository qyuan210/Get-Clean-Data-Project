#1. Merges the training and the test sets to create one data set.
rm(list = ls())
subject_train <- read.table("./train/subject_train.txt")
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_test <- read.table("./test/subject_test.txt")
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")

train <- cbind(subject_train, y_train,x_train)
test <- cbind(subject_test, y_test, x_test)

merged_data<- rbind(train, test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("features.txt", stringsAsFactors = FALSE)
index <- grep("mean\\(\\)|std\\(\\)", features[ ,2])
mean_std <- merged_data[,c(1,2,index+2)]

#3.Uses descriptive activity names to name the activities in the data set
activity_label <- read.table("activity_labels.txt")
names(activity_label) <- c("activity_id", "activity")
names(mean_std)[2] <- c("activity_id")
mean_std_activity <- merge(mean_std, activity_label, by = "activity_id")
mean_std_activity <- select(mean_std_activity, 2,69,3:68)

#4.Appropriately labels the data set with descriptive variable names.
names(mean_std_activity) <- c("subject_id", "activity", features[index,2])
names(mean_std_activity) <- gsub("^t","Time", names(mean_std_activity))
names(mean_std_activity) <- gsub("^f", "Frequency", names(mean_std_activity)) 
names(mean_std_activity) <- gsub("\\(\\)", "", names(mean_std_activity))
names(mean_std_activity) <- gsub("std", "StdDev", names(mean_std_activity))
names(mean_std_activity) <- gsub("mean", "Mean", names(mean_std_activity))


#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data <- group_by(mean_std_activity,activity, subject_id )
data_means <- summarize_each(data, funs = "mean")

write.table(data_means, file = "sensor_avg_by_act_sub.txt", row.names = FALSE)
