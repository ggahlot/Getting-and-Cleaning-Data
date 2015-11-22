#download data from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
##Load Library httr
library(httr) 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##save File as HAR Dataset
file <- "instancia.zip"
## Download file
##download.file(url, file)


#unzip into UCI HAR Dataset folder check if it exists.
datafolder <- "UCI HAR Dataset"
resultsfolder <- "results"
if(!file.exists(datafolder)){
  print("unzip file")
  unzip(file, list = FALSE, overwrite = TRUE)
} 

## create folder if it does not exist
if(!file.exists(resultsfolder)){
  print("create results folder")
  dir.create(resultsfolder)
} 

##Read txt and covnert to data.frame
gettables <- function (filename,cols = NULL){
  print(paste("Getting table:", filename))
  f <- paste(datafolder,filename,sep="/")
  data <- data.frame()
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  data
}

#TEst  gettables for Features File
Device_features <- gettables("features.txt")

#read data and build database

getdata <- function(type, Device_features){
  print(paste("Getting data", type))
  subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
  x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),Device_features$V2)
  return (cbind(subject_data,y_data,x_data))
}

#run and check getdata
test <- getdata("test", Device_features)
train <- getdata("train", Device_features)

#save the resulting data in the indicated folder
saveresults <- function (data,name){
  print(paste("saving results", name))
  file <- paste(resultsfolder, "/", name,".csv" ,sep="")
  write.csv(data,file)
}

### required activities with Help from internet ###

#1) Merges the training and the test sets to create one data set.
##install.packages("plyr")
library(plyr)
##Merge test and train DB
data <- rbind(train, test)
data <- arrange(data, id)

#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveresults(mean_and_std,"mean_and_std")

#3) Uses descriptive activity names to name the activities in the data set
activity_labels <- gettables("activity_labels.txt")

#4) Appropriately labels the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresults(tidy_dataset,"tidy_dataset")


## Finesed with help from Internet

##Create Documentation
##install.packages("rmarkdown")
##install.packages("rmarkdown")
##download and install pandoc from https://github.com/jgm/pandoc/releases/tag/1.15.2
## Knit created PDF  and html



