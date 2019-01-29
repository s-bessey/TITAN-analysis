# This function takes a raw input from basic reports, gets the cumulative incidence,
# and finds the mean/simulation intervals (2.5% and 97.5% quantiles)
# filetype = type to save plots



#still have to make if/then statements below to actually run this

# set some parameters
plotType = ".pdf" # eps won't work for this, but choose a filetype for the saved plot
transparency = .3 # how transparent the ribbon on the plot is
plotColor = 'blue' # color for the ribbon

accumulate <-function(rawData, filetype, col, transp) {

  #set file/object name for output
  dataOut <-paste((deparse(substitute(rawData))),"Cum", sep = "_")
  
  # create a dataframe of cumulative sum of incidence at each timestep for each seed
  # the aggregate function allows you to perform any function on an entire data
  # frame grouped by one or more variables
  #forCum <- aggregate(.~t+nseed, rawData,function(x) cumsum = cumsum(x))
  #forCum <- rawData %>% group_by(nseed,t) %>% mutate(sum=cumsum(Incid))
  forCum <- rawData
  x <-forCum %>% group_by(nseed) %>% mutate(cumInc = cumsum(Incid))
  forCum <- data.frame(x)
  forCum$IncPerc <- (forCum$cumInc/forCum$Total)
  #cbind(forCum, cum_Inc)
  # take the mean of each timestep
  meanCum <<- aggregate(.~t, forCum, function(x) mean = mean(x))


  # gives the cumulative sum BEFORE taking the mean, allowing further
  # processing of that raw data (if you want to plot all runs separately)
  assign(x=dataOut, value = forCum, env = parent.frame()) #create variable
  # file name for output txt file
  fileName <- paste(dataOut,".txt",sep="")
  
  #write.table(cumReport, file = fileName)
  # because of the way the quantile funciton works, we can't use it over 
  # the entire df. Because of that, we can use tapply, but need to coerce it
  # back to a dataframe with rbind and as.data.frame
  SIs <<- tapply(forCum$IncPerc, forCum$t, quantile, probs = c(.025, .975)) %>%
    do.call("rbind",.) %>% as.data.frame()
  SIprev <- tapply(forCum$HIV, forCum$t, quantile, probs = c(.025, .975)) %>%
    do.call("rbind",.) %>% as.data.frame()
  # put the SIs and mean of the total cumulative incidence into a df, name
  # columns, and assign it to an output variable for analysis

  output <- cbind(meanCum$t, SIs, meanCum$IncPerc)
  outputPrev <- cbind(meanCum$t, SIprev, meanCum$HIV)
  colnames(output) <- c("t", "lowerCI", "upperCI", "CumulativeIncidencePercent")
  colnames(outputPrev) <- c("t", "lowerCI", "upperCI", "CumulativeIncidencePrev")
  assign("output", output, envir = .GlobalEnv)
  #create line plot with ribbon
  outputplot <- ggplot(output) + geom_line(aes(x = t, y = CumulativeIncidencePercent)) +
    geom_ribbon(aes(x = t, ymin = lowerCI, ymax = upperCI), alpha = transp) +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
    theme_classic()
  prevPlot <- ggplot(outputPrev) + geom_line(aes(x = t, y = CumulativeIncidencePrev)) +
    geom_ribbon(aes(x = t, ymin = lowerCI, ymax = upperCI), alpha = transp) +
    theme_classic()
  # save plots
  ggsave(paste(dataOut, "plot", filetype, sep = ""), outputplot)
  ggsave(paste(dataOut, "Prevplot", filetype, sep = ""), prevPlot)
}

accumulate(basicReport_BLACK, plotType,plotColor, transparency)
# have to see where the confidences land in this
# this should also go in the function?
blackPlot <- ggplot(basicReport_BLACK) + geom_line(aes(t, total)) + 
  geom_ribbon(aes(x = t, ymin = Confidence[1], ymax = Confidence[2]), alpha = transp,
              fill = ribbonColor)

if (black == T){
  if (exists("basicReport_BLACK")){
    accumulate(basicReport_BLACK, plotType, plotColor, transparency)
  } else{
    print("Error: basicReport_BLACK does not exist")
  }
}
  
if (white == T){
  if (exists("basicReport_WHITE")){
    accumulate(basicReport_WHITE,plotType, plotColor, transparency)
  } else{
    print("Error: basicReport_WHITE does not exist")
  }
}

if (HF == T){
  if (exists("basicReport_HF")){
    accumulate(basicReport_HF, plotType, plotColor, transparency)
  } else{
    print("Error: basicReport_HF does not exist")
  }
}
if (HM == T){
  if (exists("basicReport_HM")){
    accumulate(basicReport_HM, plotType, plotColor, transparency)
  } else{
    print("Error: basicReport_HM does not exist")
  }
}
if (MSM == T){
  if (exists("basicReport_MSM")){
    accumulate(basicReport_MSM, plotType, plotColor, transparency)
  } else{
    print("Error: basicReport_MSM does not exist")
  }
}
if (Incar == T){
  if (exists("basicReport_INCAR")){
    accumulate(basicReport_INCAR, plotType, plotColor, transparency)
  } else{
    print("Error: basicReport_INCAR does not exist")
  }
}




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
