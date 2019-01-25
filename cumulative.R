# This function takes a raw input from basic reports, gets the cumulative incidence,
# and finds the mean/simulation intervals (2.5% and 97.5% quantiles)
# filetype = type to save plots



#still have to make if/then statements below to actually run this

# set some parameters
plotType = ".pdf" # eps won't work for this, but choose a filetype for the saved plot
transparency = .3 # how transparent the ribbon on the plot is
plotColor = # color for the ribbon

accumulate <-function(rawData, filetype, col, transp) {

  #set file/object name for output
  dataOut <-paste((deparse(substitute(rawData))),"Cum", sep = "_")
  
  # create a dataframe of cumulative sum of incidence at each timestep for each seed
  # the aggregate function allows you to perform any function on an entire data
  # frame grouped by one or more variables
  forCum <- aggregate(.~nseed + t, rawData,function(x) cumsum = cumsum(x))
  # take the mean of each timestep
  meanCum <- aggregate(.~t, rawData,function(x) mean = mean(x))


  # gives the cumulative sum BEFORE taking the mean, allowing further
  # processing of that raw data (if you want to plot all runs separately)
  assign(x=dataOut, value = forCum, env = parent.frame()) #create variable
  # file name for output txt file
  fileName <- paste(dataOut,".txt",sep="")
  
  #write.table(cumReport, file = fileName)
  # because of the way the quantile funciton works, we can't use it over 
  # the entire df. Because of that, we can use tapply, but need to coerce it
  # back to a dataframe with rbind and as.data.frame
  SIs <- tapply(forCum$Total, forCum$t, quantile, probs = c(.025, .975)) %>%
    do.call("rbind",.) %>% as.data.frame()
  # put the SIs and mean of the total cumulative incidence into a df, name
  # columns, and assign it to an output variable for analysis
  output <- cbind(meanCum$t, SIs, meanCum$Total)
  colnames(output) <- c("t", "lowerCI", "upperCI", "meanTotal")
  assign("output", output, envir = .GlobalEnv)
  #create line plot with ribbon
  outputplot <- ggplot(output) + geom_line(aes(x = t, y = meanTotal)) +
    geom_ribbon(aes(x = t, ymin = lowerCI, ymax = upperCI), alpha = transp) +
    theme_classic()
  # save plot
  ggsave(paste(dataOut, "plot", filetype, sep = ""), outputplot)
}

accumulate(basicReport_BLACK, plotType)
# have to see where the confidences land in this
# this should also go in the function?
blackPlot <- ggplot(basicReport_BLACK) + geom_line(aes(t, total)) + 
  geom_ribbon(aes(x = t, ymin = Confidence[1], ymax = Confidence[2]), alpha = transp,
              fill = ribbonColor)

#completely untested
# check if the parse works
# check if the name works
for (i in 1:length(dataframe_names)){
  dataTemp <- parse(dataframe_names[i])
  accumulate(dataTemp,plotType, plotColor, transparency)
  #really unsure about this. I don't think i need to deparse? since that's a character df
  write.table(output, file = paste(dataframe_names[i], "_cum.txt", sep = ""))
  #may need to add output$ to variables
  #plotTemp <- ggplot(output) + geom_line(aes(t, total)) + 
  #  geom_ribbon(aes(x = t, ymin = Confidence[1], ymax = Confidence[2]), alpha = .3) +
  #  theme_bw()
  #ggsave(paste(dataframe_names[i], "fig.pdf",sep = ""))
}
