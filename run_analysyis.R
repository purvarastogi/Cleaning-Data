library(dplyr)
library(plyr)

# Reading training datasets
x_train <- read.table("C:/Users/Dhawa/OneDrive/Desktop/Clean Data/train/X_train.txt")
y_train <- read.table("C:/Users/Dhawa/OneDrive/Desktop/Clean Data/train/y_train.txt")
subject_train <- read.table("C:/Users/Dhawa/OneDrive/Desktop/Clean Data/train/subject_train.txt")

## Reading test datasets
x_test <- read.table("C:/Users/Dhawa/OneDrive/Desktop/Clean Data/test/X_test.txt")
y_test <- read.table("C:/Users/Dhawa/OneDrive/Desktop/Clean Data/test/y_test.txt")
subject_test <- read.table("C:/Users/Dhawa/OneDrive/Desktop/Clean Data/test/subject_test.txt")

### Read data feature description
features_names <- read.table("C:/Users/Dhawa/OneDrive/Desktop/Clean Data/features.txt")

#### Read activity labels
activity_labels <- read.table("C:/Users/Dhawa/OneDrive/Desktop/Clean Data/activity_labels.txt")

# 1. Merges the training and the test sets to create one data set.

x_total <- rbind(x_train, x_test)
y_total <- rbind(y_train, y_test)
sub_total <- rbind(subject_train, subject_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

selected_feature <- features_names[grep("mean\\(\\)|std\\(\\)",features_names[,2]),]
x_total <- x_total[,selected_feature[,1]]

# 3. Uses descriptive activity names to name the activities in the data set

colnames(y_total) <- "activity"
y_total$activitylabel <- factor(y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- y_total[,-1]

# 4. Appropriately labels the data set with descriptive variable names.

colnames(x_total) <- features_names[selected_feature[,1],2]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

colnames(sub_total) <- "subject"
total <- cbind(x_total, activitylabel, sub_total)
total_mean <- total %>% 
  group_by(activitylabel, subject) %>%
  summarize_each(funs(mean))

write.table(total_mean, file = "C:/Users/Dhawa/OneDrive/Desktop/Clean Data/tidydata.txt", row.names = FALSE, col.names = TRUE)
