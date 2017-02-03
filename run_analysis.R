# You should create one R script called run_analysis.R that does the following.
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Setting working directory
setwd("/Users/Rafael/Documents/GitHub/Coursera-JH-DataCleaning")

#library
library(data.table)
library(dplyr)

#Downloading and unzipping file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","original.zip")
unzip("original.zip")

#Understandind and reading files
files <- list.files("./UCI HAR Dataset", recursive = TRUE)
# [1] "activity_labels.txt"                          "features.txt"                                
# [3] "features_info.txt"                            "README.txt"                                  
# [5] "test/Inertial Signals/body_acc_x_test.txt"    "test/Inertial Signals/body_acc_y_test.txt"   
# [7] "test/Inertial Signals/body_acc_z_test.txt"    "test/Inertial Signals/body_gyro_x_test.txt"  
# [9] "test/Inertial Signals/body_gyro_y_test.txt"   "test/Inertial Signals/body_gyro_z_test.txt"  
# [11] "test/Inertial Signals/total_acc_x_test.txt"   "test/Inertial Signals/total_acc_y_test.txt"  
# [13] "test/Inertial Signals/total_acc_z_test.txt"   "test/subject_test.txt"                       
# [15] "test/X_test.txt"                              "test/y_test.txt"                             
# [17] "train/Inertial Signals/body_acc_x_train.txt"  "train/Inertial Signals/body_acc_y_train.txt" 
# [19] "train/Inertial Signals/body_acc_z_train.txt"  "train/Inertial Signals/body_gyro_x_train.txt"
# [21] "train/Inertial Signals/body_gyro_y_train.txt" "train/Inertial Signals/body_gyro_z_train.txt"
# [23] "train/Inertial Signals/total_acc_x_train.txt" "train/Inertial Signals/total_acc_y_train.txt"
# [25] "train/Inertial Signals/total_acc_z_train.txt" "train/subject_train.txt"                     
# [27] "train/X_train.txt"                            "train/y_train.txt" 
files <- files[c(1:2,5:length(files))]

#creating a list of filenames o use as variable names
filenames <- gsub("(^.*/|\\.txt)","",files)
for ( i in 1:length(files)){
    try(assign(filenames[i], fread(paste("./UCI HAR Dataset/",files[i],sep=""), sep = "auto")))
}

#auxiliar list to bind test and train sets
categories <- unique(gsub("_t.*$","",filenames[3:length(filenames)]))

#binding sets
for (i in 1:length(categories)){
    assign(categories[i], 
           rbindlist(
               list(
                   eval(
                       as.name(
                           paste(categories[i],"_train",sep = "")
                           )
                       )
                   ,eval(
                       as.name(
                           paste(categories[i],"_test",sep = "")
                           )
                       )
               )
               )
           )
}

#cleaning variables that will not be used anymore
rm(list = filenames[grepl("(test|train)",filenames)])

#Naming the columns and join the subject and activity as a label.
names(X) <- t(features$V2)
y <- y[activity_labels,on="V1"]
X[,c("subject", "activity"):= .(subject[,V1], y[,V2])]

#join the mean and std of all signals variables
signals <- categories[grepl("(acc|gyro)",categories)]
for ( i in 1:length(signals)){
    X[,c(paste(signals[i],"-mean", sep="")
         ,paste(signals[i],"-std", sep="")):= .(rowMeans(eval(as.name(signals[i])))
                                                ,apply(eval(as.name(signals[i]))[],1,sd))]
}

#Making names more readable
names(X) <- X[,gsub("[Aa]cc","Acceleration",names(X))]
names(X) <- X[,gsub("[Gg]yro","Gyroscope",names(X))]

#Adjusting as factor before grouping by
X$subject <- X[,as.factor(subject)]
X$activity <- X[,as.factor(activity)]

#Grouping by and removing duplicated columns 
tidy_data <- X[,lapply(X,mean),by=.(subject,activity)]
tidy_data[,c(564,565):=NULL]


fwrite(X,"tidy_data.csv")
