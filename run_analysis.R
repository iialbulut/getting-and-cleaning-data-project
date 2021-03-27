# Creating working directory for the project and download the file
library(reshape2); library(dplyr); library(tidyr)

if(!file.exists("./project")){dir.create("./project")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./project/Project_files.zip", method = "curl")

#Unzip the Project files
unzip(zipfile = "./project/Project_files.zip", 
      exdir = "./project")

# Reading data from the Project files
## Reading train data
train_data <- read.table("./project/UCI HAR Dataset/train/X_train.txt",
                        fill = T, header = F, sep = "", dec = ".")

train_labels <- read.table("./project/UCI HAR Dataset/train/y_train.txt",
                           fill = T, header = F, sep = "", dec = ".")

train_subjects <- read.table("./project/UCI HAR Dataset/train/subject_train.txt",
                             fill = T, header = F, sep = "", dec = ".")

## Reading test data
test_data <- read.table(file = "./project/UCI HAR Dataset/test/X_test.txt", 
                        fill = T, header = F, sep = "", dec = ".")

test_labels <- read.table("./project/UCI HAR Dataset/test/y_test.txt",
                           fill = T, header = F, sep = "", dec = ".")

test_subjects <- read.table("./project/UCI HAR Dataset/test/subject_test.txt",
                             fill = T, header = F, sep = "", dec = ".")

## Reading features and activity data
features <- read.table("./project/UCI HAR Dataset/features.txt",
                                      fill = T, header = F, sep = "", dec = ".")

activity <- 

# Merging data
train_test <- rbind(train_data, test_data)

labels_all <- rbind(train_labels, test_labels)

subjects_all <- rbind(train_subjects, test_subjects)

# set names
names(subjects_all) <- c("subject")
names(labels_all) <- c("activity")
names(train_test) <- features$V2

data_all <- cbind(subjects_all, labels_all, train_test)

# Extraction of measurements mean and std
features_required <- features$V2[grep("mean\\(\\)|std\\(\\)",
                                      features$V2)]

# subsetting merged all data by required columns
required_cols <- c("subject", "activity", features_required)
data_all <- subset(data_all, select = required_cols)

str(data_all)

# putting activity names and arrange as factor
label_names <- read.table("./project/UCI HAR Dataset/activity_labels.txt",
                          fill = T, header = F, sep = "", dec = ".")

data_all$activity <- factor(data_all$activity, levels = label_names[ ,1],
                            labels = label_names[ ,2])
data_all$subject <- as.factor(data_all$subject)

# correting the names
names(data_all)<-gsub("Acc", "Accelerometer", names(data_all))
names(data_all)<-gsub("Gyro", "Gyroscope", names(data_all))
names(data_all)<-gsub("BodyBody", "Body", names(data_all))
names(data_all)<-gsub("Mag", "Magnitude", names(data_all))
names(data_all)<-gsub("^t", "Time", names(data_all))
names(data_all)<-gsub("^f", "Frequency", names(data_all))
names(data_all)<-gsub("tBody", "TimeBody", names(data_all))
names(data_all)<-gsub("-mean()", "Mean", names(data_all), ignore.case = TRUE)
names(data_all)<-gsub("-std()", "STD", names(data_all), ignore.case = TRUE)
names(data_all)<-gsub("-freq()", "Frequency", names(data_all), ignore.case = TRUE)
names(data_all)<-gsub("angle", "Angle", names(data_all))
names(data_all)<-gsub("gravity", "Gravity", names(data_all))

#checking the last data structure
str(data_all)

# creating the tidy data
melt_data_all <- melt(data_all, id.vars = c("subject", "activity"))

data_all_tidy <- melt_data_all %>% 
        dcast(subject + activity ~ variable, mean)

## alternatively
gathered_data_all <- data_all %>% 
        gather(variable, measure, -subject, -activity)

data_tidy <- gathered_data_all %>% 
        group_by(activity, subject) %>% 
        summarise_all(funs(mean))
data_tidy <- data_tidy[ , -3]
data_tidy <- data_tidy %>% 
        summarize(subject_size = n(),
                  Avg_activity_meas = mean(measure))


# writing the outputs
write.table(data_all_tidy, file = "./project/Tidy_data_wide.txt", row.names = FALSE)
write.table(data_tidy, file = "./project/Tidy_data_narrow.txt", row.names = FALSE)

library(knitr)
rmarkdown::render("./project/codebook.Rmd")
