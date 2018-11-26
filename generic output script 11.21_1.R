
# Load necessary packages
#if packages aren't installed,use the following code prior to running the model:
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

## Read base cases results into the global environment
#setwd("~/results") 
#import files to use
incidenceReport=read.table("incidenceReport.txt", sep="\t", header=TRUE)
                          # Incidence Report (Overall)
iduReport=read.table("iduReport.txt",sep="\t", 
                     header=TRUE) # Injection Drug User Report (Overall)


#testing -- need to find a standard word for all reports to be imported
temp=list.files(pattern = "Inc*")
for (i in 1:length(temp)){
  assign('temp2', read.table(temp[i],header = TRUE))
  temp2 <- temp2[order(temp2[,1]),]
  assign(temp[i],temp2)
}
  
  
#function for cumulative incidence
accumulate <- function(nonCumFile){
  #define cumulative incidence grouped by seed
  #creates a data frame of the cumulative incidence on each individual iteration
  #this data frame excludes the first two columns, because time and seed should
  #not be cumulative
  
  
  #I think this needs to change to an aggregate like meanAndStd
  cumIncidence <- as.data.frame(do.call("rbind",
                 by(nonCumFile[3:ncol(nonCumFile)],nonCumFile$seed,cumsum)))
  
  #create names for each column based on original column name with _Cum appended
  for (ii in 1:ncol(cumIncidence)){
    names(cumIncidence)[ii]<-(paste(colnames(cumIncidence[ii]),"_Cum",sep=""))
  }
  
  #create a filename for output file based on name of input file
  file.name <- paste((deparse(substitute(nonCumFile))),"_Cum",".txt",sep="")
  
  #bind the first two rows (timestep and seed) to the cumulative values
  cumulativeReport <- cbind(nonCumFile[,1:2],cumIncidence)
  
  #create output
  write.table(cumulativeReport,file=file.name)
  listName <- paste(deparse(substitute(nonCumFile)),"_Cum",sep="")
  output <- as.data.frame(cumulativeReport)
  assign(x = listName, value = output, env=parent.frame())
  
}




accumulate(incidenceReport)

#plot cumulative Total
p <- ggplot() + geom_line(data = incidenceReport_Cum, aes(x = t, y = Total_Cum, group = seed, color=factor(seed)))
p+geom_line(data = incidenceReport_Cum,aes(x = t, y = Total_Cum, linetype=factor(seed), color=factor(seed)))

