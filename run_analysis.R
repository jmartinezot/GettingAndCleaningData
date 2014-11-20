# You should create one R script called run_analysis.R that does the following.
 
# 1. Merges the training and the test sets to create one data set.

train_data <- read.table("train/X_train.txt")
test_data <- read.table("test/X_test.txt")
all_data <- rbind(train_data, test_data)

subj_train_data <- read.table("train/subject_train.txt")
subj_test_data <- read.table("test/subject_test.txt")
subj_data <- rbind(subj_train_data, subj_test_data)

activity_train_data <- read.table("train/y_train.txt")
activity_test_data <- read.table("test/y_test.txt")
activity_data <- rbind(activity_train_data, activity_test_data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.  

features <- read.table("features.txt")
# define the features we want to use, that are only the features whose name contains "-mean()" or "-std()"
used_features <- grep("-mean\\(\\)|-std\\(\\)",features[,2])
# get only the right features
all_data <- all_data[,used_features]
# remove parentheses
names(all_data) <- gsub("\\(|\\)","",features[used_features, 2])

# 3. Uses descriptive activity names to name the activities in the data set.

names(activity_data) <- "activity"
activity <- read.table("activity_labels.txt")
# Replace numbers with descriptive names
activity_data[,1] = activity[activity_data[,1],2]

# 4. Appropriately labels the data set with descriptive variable names. 

names(subj_data) <- "subject"
tidy_dataset <- cbind(subj_data, activity_data, all_data)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

subjects <- unique(subj_data)[,1]
n_subjects <- length(subjects)
n_activities <- nrow(activity)
n_cols <- dim(tidy_dataset)[2]
tidy_summary <- tidy_dataset[1:(n_subjects*n_activities), ]

row = 1

# summarize by subjects and activities, 
for (s in 1:n_subjects) {
  for (a in 1:n_activities) {
    tidy_summary[row, 1] = subjects[s]
    tidy_summary[row, 2] = activity[a, 2]
    idxs <- tidy_dataset[tidy_dataset$subject==s & tidy_dataset$activity==activity[a, 2], ]
    tidy_summary[row, 3:numCols] <- colMeans(idxs[, 3:numCols])
    row = row+1
  }
}

# write to disk the final result
write.table(tidy_summary, "tidy_dataset.txt", row.names=FALSE)

