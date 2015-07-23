setwd('/home/jamsic/Documents/coursera/getdata/CourseProject/UCI HAR Dataset')

# function to replace activity ids with activity labels. It could also be done using merge()
# but as long as data is vector (not data.frame) relabel() works better

relabel <- function(vector, labels) {
  d <- as.character(vector)
  for (label in seq(along.with = labels)) {
    d <- sub(as.character(label), labels[label], d)
  }
  d
}

# reading test

data_features_test <- read.table('test/X_test.txt')
data_activity_test <- read.table('test/y_test.txt')
data_subject_test <- read.table('test/subject_test.txt')

# same for train

data_features_train <- read.table('train/X_train.txt')
data_activity_train <- read.table('train/y_train.txt')
data_subject_train <- read.table('train/subject_train.txt')

# merge data

data_features <- rbind(data_features_test, data_features_train)
data_activity <- rbind(data_activity_test, data_activity_train)
data_subject <- rbind(data_subject_test, data_subject_train)

# label the data set with descriptive variable names

feature_names <- read.table('features.txt')
feature_names <- feature_names[, 2]
feature_names <- gsub('\\(', '', feature_names)
feature_names <- gsub('\\)', '', feature_names)
feature_names <- gsub('-', '', feature_names)
feature_names <- gsub(',', '', feature_names)
feature_names <- tolower(feature_names)
feature_names <- gsub('^t', 'time', feature_names)
feature_names <- gsub('^f', 'fft', feature_names)
feature_names
names(data_features) <- as.character(feature_names)

# extract mean and std

has_mean_or_std <- grepl('mean|std', names(data_features))
needed_data_features <- data_features[has_mean_or_std]

# Use descriptive activity names to name the activities in the data set

names(data_activity) <- c('activitytype')
data_activity_labels <- read.table('activity_labels.txt')
data_activity_labels <- as.character(data_activity_labels[,2])
data_activity_labels
data_activity[,1] <- relabel(data_activity[,1], data_activity_labels)

names(data_subject) <- c('subjectid')

data <- cbind(needed_data_features, data_activity, data_subject)

# independent tidy data set with the average of each variable for each activity and each subject

tidydataset <- aggregate(needed_data_features, by=list(data_subject[,1], data_activity[,1]), mean)

names(tidydataset)[1:2] <- c(names(data_subject), names(data_activity))
names(tidydataset)

write.table(tidydataset, file='tidy_data_set.txt', row.name=FALSE)
