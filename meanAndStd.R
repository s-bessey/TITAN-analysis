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

