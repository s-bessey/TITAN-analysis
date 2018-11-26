accumulate <-function(rawData) {
  #set up empty data frame for the mean output
  #forCum <- as.data.frame(seq(0,max(rawData$t),1))
  #colnames(forCum) <- "t"
  
  #set file/object name for output
  dataOut <- paste((deparse(substitute(rawData))),"_Cum",sep="")
  
  #create a dataframe of cumulative sum of incidence at each timestep by seed
  forCum <- aggregate(.~seed, rawData,function(x) cumsum = cumsum(x))
  
  colnames(forCum)[3:ncol(forCum)] <- paste(colnames(forCum)[3:ncol(forCum)],
                                            "_Cum",sep="")
  #colnames(forCum)[1:2] <- c("seed", "t")   #above line adds mean to end of 
  #column. This corrects seed and time back to t
  
  
  #meanReport <- merge(forCum,forSd)  #this puts the mean and std in one file 
  #and merges the t columns
  
  assign(x=dataOut, value = forCum, env = parent.frame()) #create variable
  fileName <- paste((deparse(substitute(rawData))),"_Cum",".txt",sep="")
  #write.table(cumReport, file = fileName)
}
