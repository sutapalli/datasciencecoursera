#Read data from site 
getDataFromNet <- function (URL, destfile) 
{ 
  if (!file.exists(destfile))  {download.file(URL, destfile);file <- unzip(destfile); folder <- unlist(strsplit(file[1],"/"))[2] } 
  else "UCI HAR Dataset"                      
} 

# 1- Merge all data together 
mergeAllData <- function(folder)
{ 
  trainDir  <- paste0(folder,"/train")               
  testDir   <- paste0(folder,"/test")                
  fileTrain <- paste0(trainDir,"/subject_train.txt") 
  fileTest  <- paste0(testDir,"/subject_test.txt")   
  subTrain  <- data.table(read.table(fileTrain))     
  subTest   <- data.table(read.table(fileTest))      
  mergeSub  <- rbind(subTrain, subTest)              
  setnames(mergeSub, "V1", "mergeSub")               
  fileTrain <- paste0(trainDir,"/Y_train.txt")        
  fileTest  <- paste0(testDir,"/Y_test.txt") 
  yTrain  <- data.table(read.table(fileTrain)) 
  yTest   <- data.table(read.table(fileTest)) 
  mergeY <- rbind(yTrain, yTest) 
  setnames(mergeY, "V1", "mergeY") 
  fileTrain <- paste0(trainDir,"/X_train.txt") 
  fileTest  <- paste0(testDir,"/X_test.txt") 
  xTrain  <- data.table(read.table(fileTrain)) 
  xTest   <- data.table(read.table(fileTest)) 
  mergeX <- rbind(xTrain, xTest) 
  setnames(mergeX, "V1", "mergeX") 
  allMerge <- cbind(mergeSub,mergeY,mergeX)
} 


# Function to shift right a vector of positions (integers) 
siftRigth <- function(vecPos, sift)
{ 
    tmp <- c(1,2) ; for(pos in vecPos) { tmp <- append(tmp, c(pos + sift))} 
    tmp 
} 

# Cleaning labels 
cleanLabels <- function (featureData,posMeanStd,meanAndStd)
  { 
    featuresNames <- featureData$V2[posMeanStd]
    featuresNames <- gsub("-","_",featuresNames)
    featuresNames <- gsub("\\(\\)","",featuresNames)
    featuresNames <- gsub("BodyBody","Body",featuresNames)
    featuresNames <- gsub("Acc","_Acc_",featuresNames)  
    featuresNames <- gsub("Body","Body_",featuresNames) 
    featuresNames <- gsub("Gyro","Gyro_",featuresNames) 
    featuresNames <- gsub("Jerk","Jerk_",featuresNames) 
    featuresNames <- gsub("meanFreq","mean_Freq",featuresNames) 
    featuresNames <- gsub("__","_",featuresNames) 
    featuresNames <- append(c("Activity_ID","Subject"),featuresNames)  
    featuresNames <- append(featuresNames,"Activity_Name") 
    names(meanAndStd) <- featuresNames
    meanAndStd 
  } 

#Function that load descriptive activity names 
descripActivNames <- function (folder,meanAndStd)
  { 
    activityFile <- paste0(folder,"/activity_labels.txt")
    activityNames<- fread(activityFile)                  
    names(activityNames) <- c("mergeY", "Activities")    
    setkey(meanAndStd,mergeY)                            
    setkey(activityNames,mergeY)                         
    meanAndStd <- merge(meanAndStd,activityNames)        
  } 

# Function to create tidy data 
createTidyData <- function(meanAndStd) 
{ 
  measureVars <- names(meanAndStd)[3:(length(meanAndStd)-1)] # Measure variable will be all columns except the first two and last one 
  dataMelt    <- melt(meanAndStd, id = c("Subject","Activity_ID","Activity_Name"), measure.vars = measureVars) 
  
  tidyData   <- dcast(dataMelt, Subject + Activity_Name ~ variable, mean) 
} 

# Function to load necessary libraries 
loadNecessaryLibs <- function () 
  { 
    if (!require("data.table"))  
    { 
        install.packages("data.table") 
    } 
    if (!require("reshape2"))  
    { 
      install.packages("reshape2") 
    } 
    library("data.table") 
    library("reshape2") 
  } 
 

main <- function() 
  {   
    loadNecessaryLibs() 
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
    destfile <- "dataset.zip" 
    # 1 - Merges the training and the test sets to create one data set. 
    folder <- getDataFromNet(URL, destfile)
    allData <- mergeAllData(folder)        
    # 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
    featureData  <- fread(paste0(folder,"/features.txt")) 
    posMeanStd   <- grep("mean|std",featureData$V2)       
    matchCol     <- siftRigth(posMeanStd, 2)              
    meanAndStd   <- subset(allData, select=matchCol,with=FALSE)
    # 3 - Uses descriptive activity names to name the activities in the data set 
    meanAndStd <- descripActivNames(folder,meanAndStd) 
    # 4 - Appropriately labels the data set with descriptive variable names.  
    meanAndStd <- cleanLabels (featureData,posMeanStd,meanAndStd) 
    # 5 - Creates a second, independent tidy data set with the average  
    # of each variable for each activity and each subject.  
    tidyData <- createTidyData(meanAndStd) 
    write.table(tidyData, paste0(folder,"/tidyData.txt"),row.name=FALSE) 
    tidyData 
} 

main()
