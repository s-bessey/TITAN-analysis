accumulate <-function(rawData, outputname) {
  #set up empty data frame for the mean output
  #forCum <- as.data.frame(seq(0,max(rawData$t),1))
  #colnames(forCum) <- "t"
  
  #set file/object name for output
  dataOut <- paste(outputname,"_Cum",sep="")
  
  #create a dataframe of cumulative sum of incidence at each timestep by seed
  forCum <- aggregate(.~rseed + t, rawData,function(x) cumsum = cumsum(x)) %>%
               separate_rows()
  #shouldn't need this when changing to aggregate by both seed and t
  #forCum$t <- rawData$t
  forCum$pseed <- rawData$pseed
  forCum$nseed <- rawData$nseed

  
  assign(x=dataOut, value = forCum, env = parent.frame()) #create variable
  fileName <- paste((deparse(substitute(rawData))),"_Cum",".txt",sep="")
  #write.table(cumReport, file = fileName)
  CIs <- tapply(forCum$Total, forCum$t, quantile, probs = c(.025, .975)) %>%
    do.call("rbind",.) %>% as.data.frame()
  meanCum <- aggregate(~.t, forCum, FUN = mean)
  assign("output", cbind(CIs, meanCum$Total), envir = .GlobalEnv)
}

accumulate(basicReport_BLACK)
# have to see where the confidences land in this
blackPlot <- ggplot(basicReport_BLACK) + geom_line(aes(t, total)) + 
  geom_ribbon(aes(x = t, ymin = Confidence[1], ymax = Confidence[2]), alpha = .3)

#completely untested
# check if the parse works
# check if the name works
for (i in 1:length(dataframe_names)){
  dataTemp <- parse(dataframe_names[i])
  accumulate(dataTemp, dataframe_names[i])
  #really unsure about this. I don't think i need to deparse? since that's a character df
  write.table(output, file = paste(dataframe_names[i], "_cum.txt", sep = ""))
  #may need to add output$ to variables
  plotTemp <- ggplot(output) + geom_line(aes(t, total)) + 
    geom_ribbon(aes(x = t, ymin = Confidence[1], ymax = Confidence[2]), alpha = .3) +
    theme_bw()
  ggsave(paste(dataframe_names[i], "fig.pdf",sep = ""))
}
