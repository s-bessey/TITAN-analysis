# This function takes a raw input from basic reports, gets the cumulative incidence,
# and finds the mean/simulation intervals (2.5% and 97.5% quantiles)

# Load necessary packages
library('plyr')
library('dplyr')
library('tidyr')
library('ggplot2')

# function
accumulate <-function(rawData, filetype, col, transp, writefiles) {

  #set file/object name for output
  dataOutCum <-paste((deparse(substitute(rawData))),"Cum", sep = "_")
  dataOutPrev <-paste((deparse(substitute(rawData))),"Prev", sep = "_")
  
  # create a dataframe of cumulative sum of incidence at each timestep for each seed

  forCum <- rawData
  x <-forCum %>% group_by(nseed) %>% mutate(cumInc = cumsum(Incid))
  forCum <- data.frame(x)
  forCum$IncPerc <- (forCum$cumInc/forCum$Total)
  forCum$PrevPerc <- forCum$HIV/forCum$Total
  # take the mean of each timestep
  meanCum <<- aggregate(.~t, forCum, function(x) mean = mean(x))


  # gives the cumulative sum BEFORE taking the mean, allowing further
  # processing of that raw data (if you want to plot all runs separately)
  assign(x=dataOutCum, value = forCum, env = parent.frame()) #create variable
  # file name for output txt file
  fileName <- paste(dataOut,".txt",sep="")
  

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
  colnames(outputPrev) <- c("t", "lowerCI", "upperCI", "PrevPercent")
  assign("output", output, envir = .GlobalEnv)
  
  if (writefiles == T){
    write.table(forCum, file = fileName)
    write.table(output, file = paste(dataOutCum, "mean.txt", sep = "_"))
    write.table(outputPrev, file = paste(dataOutCum, "txt", sep = "."))
  }
  #create line plot with ribbon
  outputplot <- ggplot(output) + geom_line(aes(x = t, y = CumulativeIncidencePercent)) +
    geom_ribbon(aes(x = t, ymin = lowerCI, ymax = upperCI), alpha = transp, fill = col) +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
    theme_classic()
  prevPlot <- ggplot(outputPrev) + geom_line(aes(x = t, y = PrevPercent)) +
    geom_ribbon(aes(x = t, ymin = lowerCI, ymax = upperCI), alpha = transp, fill = col) +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
    theme_classic()
  # save plots
  ggsave(paste(dataOut, "Plot", filetype, sep = ""), outputplot)
  ggsave(paste(dataOutPrev, "Plot", filetype, sep = ""), prevPlot)
}

