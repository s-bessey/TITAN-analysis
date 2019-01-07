
# Load necessary packages
#if packages aren't installed,use the following code prior to running the model:
library(plyr)
library(dplyr)
library(tidyr)
library(prettyGraphs)

## Read base cases results into the global environment

#import files to use


#function for cumulative incidence
accumulate <-function(rawData) {
  #set up empty data frame for the mean output
  #forCum <- as.data.frame(seq(0,max(rawData$t),1))
  #colnames(forCum) <- "t"
  
  #set file/object name for output
  dataOut <- paste((deparse(substitute(rawData))),"_Cum",sep="")
  
  #create a dataframe of cumulative sum of incidence at each timestep by seed
  forCum <- aggregate(.~rseed, rawData,function(x) cumsum = cumsum(x)) %>%
    separate_rows()
  
  colnames(forCum)[3:ncol(forCum)] <- paste(colnames(forCum)[3:ncol(forCum)],
                                            "_Cum",sep="")
  colnames(forCum)[which(names(forCum) == "t_Cum")] <- "t"
  #colnames(forCum)[1:2] <- c("seed", "t")   #above line adds mean to end of 
  #column. This corrects seed and time back to t
  forCum$t <- rawData$t
  
  #meanReport <- merge(forCum,forSd)  #this puts the mean and std in one file 
  #and merges the t columns
  
  assign(x=dataOut, value = forCum, env = parent.frame()) #create variable
  fileName <- paste((deparse(substitute(rawData))),"_Cum",".txt",sep="")
  #write.table(cumReport, file = fileName)
}

meanAndStd <-function(rawData) {
  #set up empty data frame for the mean output
  forMean <- as.data.frame(seq(0,max(rawData$t),1))
  colnames(forMean) <- "t"
  
  #set file/object name for output
  dataOut <- paste((deparse(substitute(rawData))),"_Mean",sep="")
  
  #create a dataframe of means at each timestep
  forMean <- aggregate(.~t, rawData,function(x) mean = mean(x))
  forMean$seed <- NULL #this is mean across seeds; don't need seed
  colnames(forMean) <- paste(colnames(forMean),"_Mean",sep="")
  colnames(forMean)[1] <- "t"   #above line adds mean to end of column. This
  #corrects time back to t
  
  #create a dataframe of standard deviations at each timestep
  forSd <- aggregate(.~t, rawData,function(x) sd = sd(x))
  forSd$seed <- NULL #sd across seeds; don't need seed
  colnames(forSd) <- paste(colnames(forSd),"_sd",sep="")
  colnames(forSd)[1] <- "t"   #above line adds mean to end of column. This
  #corrects time back to t
  
  meanReport <- merge(forMean,forSd)  #this puts the mean and std in one file 
  #and merges the t columns
  
  #because the mean and std are appended together, they're in order of all mean
  #columns then all std columns. This is easily fixed by sorting alphabetically
  meanReport <-meanReport[,order(names(meanReport))]
  meanReport <- select(meanReport,t,everything()) #put time back in front
  
  assign(x=dataOut, value = meanReport, env = parent.frame()) #create variable
  fileName <- paste((deparse(substitute(rawData))),"_Mean",".txt",sep="")
  write.table(meanReport, file = fileName)
}





