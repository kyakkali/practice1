require(plyr)

# This r program creates a single TidyDataSet from compiling multiple raw files of 
# Human Activity Recognition Using Smartphones Data
# This program needs to be run from the directory where the source data is extracted
#

# i am making chnges to the existing 

# Merge train and test data sets to a single TotalSet

    # First read the raw data files
      TrainSet <- read.table("train/X_train.txt")
      Subject_train <- read.table("train/subject_train.txt")
      Activity_train <- read.table("train/y_train.txt")
      TestSet <- read.table("test/X_test.txt")
      Subject_test <- read.table("test/subject_test.txt")
      Activity_test <- read.table("test/y_test.txt")
      Features <- read.table("features.txt")
      ActivityLabels <- read.table("activity_labels.txt")

    # Appropriately label the data set with descriptive variable names
      names(ActivityLabels) <- c("ActivityCode", "Activity")
      names(TrainSet) <- Features[,2]
      names(Subject_train) <- "Subject"
      names(Activity_train) <- "ActivityCode"
      names(TestSet) <- Features[,2]
      names(Subject_test) <- "Subject"
      names(Activity_test) <- "ActivityCode"

    # Prepare a single data set for each of Train and Test
      TrainSet <- cbind(Subject_train, Activity_train, TrainSet)
      TestSet <- cbind(Subject_test, Activity_test, TestSet)

    # Merge/combine train and test data sets into single TotalSet
      TotalSet <- rbind(TrainSet, TestSet)
      rm("TrainSet", "TestSet")

# Extract only the measurements on the mean and standard deviation for each measurement

    # Identify required measurements
      varNames <- names(TotalSet)
      neededVars <- c("Subject", "ActivityCode", 
                      varNames[sort(c(grep("mean()", varNames, fixed=TRUE), 
                                      grep("std()", varNames, fixed=TRUE)))])

    # Create a data set with only required measurements
      TotalSet <- TotalSet[,neededVars]
      names(TotalSet) <- gsub("-", "", gsub("-std()", "Std", 
                                            sub("-mean()", "Mean", 
                                                names(TotalSet), fixed=TRUE), 
                                            fixed=TRUE), fixed=TRUE)

# Assign descriptive activity names to name the activities in the data set

    TotalSet <- merge(TotalSet, ActivityLabels, by.x="ActivityCode", by.y = "ActivityCode")

# creates an independent tidy data set with the average of each variable for each activity and each subject

    TidyDataSet <- ddply(TotalSet, .(Subject, Activity), summarise, 
                     tBodyAccMeanX_avg = mean(tBodyAccMeanX, rm.na=TRUE),
                     tBodyAccMeanY_avg = mean(tBodyAccMeanY, rm.na=TRUE),
                     tBodyAccMeanZ_avg = mean(tBodyAccMeanZ, rm.na=TRUE),
                     tBodyAccStdX_avg = mean(tBodyAccStdX, rm.na=TRUE),
                     tBodyAccStdY_avg = mean(tBodyAccStdY, rm.na=TRUE),
                     tBodyAccStdZ_avg = mean(tBodyAccStdZ, rm.na=TRUE),
                     tGravityAccMeanX_avg = mean(tGravityAccMeanX, rm.na=TRUE),
                     tGravityAccMeanY_avg = mean(tGravityAccMeanY, rm.na=TRUE),
                     tGravityAccMeanZ_avg = mean(tGravityAccMeanZ, rm.na=TRUE),
                     tGravityAccStdX_avg = mean(tGravityAccStdX, rm.na=TRUE),
                     tGravityAccStdY_avg = mean(tGravityAccStdY, rm.na=TRUE),
                     tGravityAccStdZ_avg = mean(tGravityAccStdZ, rm.na=TRUE),
                     tBodyAccJerkMeanX_avg = mean(tBodyAccJerkMeanX, rm.na=TRUE),
                     tBodyAccJerkMeanY_avg = mean(tBodyAccJerkMeanY, rm.na=TRUE),
                     tBodyAccJerkMeanZ_avg = mean(tBodyAccJerkMeanZ, rm.na=TRUE),
                     tBodyAccJerkStdX_avg = mean(tBodyAccJerkStdX, rm.na=TRUE),
                     tBodyAccJerkStdY_avg = mean(tBodyAccJerkStdY, rm.na=TRUE),
                     tBodyAccJerkStdZ_avg = mean(tBodyAccJerkStdZ, rm.na=TRUE),
                     tBodyGyroMeanX_avg = mean(tBodyGyroMeanX, rm.na=TRUE),
                     tBodyGyroMeanY_avg = mean(tBodyGyroMeanY, rm.na=TRUE),
                     tBodyGyroMeanZ_avg = mean(tBodyGyroMeanZ, rm.na=TRUE),
                     tBodyGyroStdX_avg = mean(tBodyGyroStdX, rm.na=TRUE),
                     tBodyGyroStdY_avg = mean(tBodyGyroStdY, rm.na=TRUE),
                     tBodyGyroStdZ_avg = mean(tBodyGyroStdZ, rm.na=TRUE),
                     tBodyGyroJerkMeanX_avg = mean(tBodyGyroJerkMeanX, rm.na=TRUE),
                     tBodyGyroJerkMeanY_avg = mean(tBodyGyroJerkMeanY, rm.na=TRUE),
                     tBodyGyroJerkMeanZ_avg = mean(tBodyGyroJerkMeanZ, rm.na=TRUE),
                     tBodyGyroJerkStdX_avg = mean(tBodyGyroJerkStdX, rm.na=TRUE),
                     tBodyGyroJerkStdY_avg = mean(tBodyGyroJerkStdY, rm.na=TRUE),
                     tBodyGyroJerkStdZ_avg = mean(tBodyGyroJerkStdZ, rm.na=TRUE),
                     tBodyAccMagMean_avg = mean(tBodyAccMagMean, rm.na=TRUE),
                     tBodyAccMagStd_avg = mean(tBodyAccMagStd, rm.na=TRUE),
                     tGravityAccMagMean_avg = mean(tGravityAccMagMean, rm.na=TRUE),
                     tGravityAccMagStd_avg = mean(tGravityAccMagStd, rm.na=TRUE),
                     tBodyAccJerkMagMean_avg = mean(tBodyAccJerkMagMean, rm.na=TRUE),
                     tBodyAccJerkMagStd_avg = mean(tBodyAccJerkMagStd, rm.na=TRUE),
                     tBodyGyroMagMean_avg = mean(tBodyGyroMagMean, rm.na=TRUE),
                     tBodyGyroMagStd_avg = mean(tBodyGyroMagStd, rm.na=TRUE),
                     tBodyGyroJerkMagMean_avg = mean(tBodyGyroJerkMagMean, rm.na=TRUE),
                     tBodyGyroJerkMagStd_avg = mean(tBodyGyroJerkMagStd, rm.na=TRUE),
                     fBodyAccMeanX_avg = mean(fBodyAccMeanX, rm.na=TRUE),
                     fBodyAccMeanY_avg = mean(fBodyAccMeanY, rm.na=TRUE),
                     fBodyAccMeanZ_avg = mean(fBodyAccMeanZ, rm.na=TRUE),
                     fBodyAccStdX_avg = mean(fBodyAccStdX, rm.na=TRUE),
                     fBodyAccStdY_avg = mean(fBodyAccStdY, rm.na=TRUE),
                     fBodyAccStdZ_avg = mean(fBodyAccStdZ, rm.na=TRUE),
                     fBodyAccJerkMeanX_avg = mean(fBodyAccJerkMeanX, rm.na=TRUE),
                     fBodyAccJerkMeanY_avg = mean(fBodyAccJerkMeanY, rm.na=TRUE),
                     fBodyAccJerkMeanZ_avg = mean(fBodyAccJerkMeanZ, rm.na=TRUE),
                     fBodyAccJerkStdX_avg = mean(fBodyAccJerkStdX, rm.na=TRUE),
                     fBodyAccJerkStdY_avg = mean(fBodyAccJerkStdY, rm.na=TRUE),
                     fBodyAccJerkStdZ_avg = mean(fBodyAccJerkStdZ, rm.na=TRUE),
                     fBodyGyroMeanX_avg = mean(fBodyGyroMeanX, rm.na=TRUE),
                     fBodyGyroMeanY_avg = mean(fBodyGyroMeanY, rm.na=TRUE),
                     fBodyGyroMeanZ_avg = mean(fBodyGyroMeanZ, rm.na=TRUE),
                     fBodyGyroStdX_avg = mean(fBodyGyroStdX, rm.na=TRUE),
                     fBodyGyroStdY_avg = mean(fBodyGyroStdY, rm.na=TRUE),
                     fBodyGyroStdZ_avg = mean(fBodyGyroStdZ, rm.na=TRUE),
                     fBodyAccMagMean_avg = mean(fBodyAccMagMean, rm.na=TRUE),
                     fBodyAccMagStd_avg = mean(fBodyAccMagStd, rm.na=TRUE),
                     fBodyBodyAccJerkMagMean_avg = mean(fBodyBodyAccJerkMagMean, rm.na=TRUE),
                     fBodyBodyAccJerkMagStd_avg = mean(fBodyBodyAccJerkMagStd, rm.na=TRUE),
                     fBodyBodyGyroMagMean_avg = mean(fBodyBodyGyroMagMean, rm.na=TRUE),
                     fBodyBodyGyroMagStd_avg = mean(fBodyBodyGyroMagStd, rm.na=TRUE),
                     fBodyBodyGyroJerkMagMean_avg = mean(fBodyBodyGyroJerkMagMean, rm.na=TRUE),
                     fBodyBodyGyroJerkMagStd_avg = mean(fBodyBodyGyroJerkMagStd, rm.na=TRUE)
                    )

write.table(TidyDataSet, file = "TidyDataSet.txt", row.names = FALSE, append = FALSE)

# TestTidy <- read.table("TidyDataSet.txt", sep = "", header=TRUE)
Status API Training Shop Blog About Pricing
