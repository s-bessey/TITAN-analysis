accumulate <-function(rawData) {
  #set up empty data frame for the mean output
  #forCum <- as.data.frame(seq(0,max(rawData$t),1))
  #colnames(forCum) <- "t"
  
  #set file/object name for output
  dataOut <- paste((deparse(substitute(rawData))),"_Cum",sep="")
  
  #create a dataframe of cumulative sum of incidence at each timestep by seed
  forCum <- aggregate(.~rseed, rawData,function(x) cumsum = cumsum(x)) %>%
               separate_rows()
  forCum$t <- rawData$t
  forCum$pseed <- rawData$pseed
  forCum$nseed <- rawData$nseed
  
  colnames(forCum)[5:ncol(forCum)] <- paste(colnames(forCum)[5:ncol(forCum)],
                                            "_Cum",sep="")
  #colnames(forCum)[1:2] <- c("seed", "t")   #above line adds mean to end of 
  #column. This corrects seed and time back to t
  
  
  #meanReport <- merge(forCum,forSd)  #this puts the mean and std in one file 
  #and merges the t columns
  
  assign(x=dataOut, value = forCum, env = parent.frame()) #create variable
  fileName <- paste((deparse(substitute(rawData))),"_Cum",".txt",sep="")
  #write.table(cumReport, file = fileName)
  CIs <- tapply(forCum$Total_Cum, forCum$t, quantile, probs = c(.025, .5, .975)) %>%
    do.call("rbind",.) %>% as.data.frame()
  assign("Confidence", CIs, envir = .GlobalEnv)
}
meanAndStd()


xvalue <- as.numeric(unlist(basicReport_HF_Cum$t)[1:nrow(Confidence)])
yvalue <- as.numeric(unlist(Confidence[2]))
upperCI <- as.numeric(unlist(Confidence[3]))
lowerCI <- as.numeric(unlist(Confidence[1]))

plot(xvalue,yvalue, type = 'l')
polygon(c(rev(xvalue),xvalue), c(rev(lowerCI), upperCI), col = 'grey80', border = NA)
lines(lowerCI~xvalue)
lines(upperCI~xvalue)

lines(yvalue~xvalue)

