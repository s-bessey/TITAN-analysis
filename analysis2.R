datalist <- list()
temp <- dir(pattern = "10_")
for (i in 1:length(temp)){
  directory <- paste(temp[i], "/results/componentReport_ALL.txt")
  assign(paste("temp2",sep = ""), read.table(directory,header = TRUE))
  datalist[[i]] <- temp2
}
treatment10 <- do.call(rbind, datalist)
write.table(treatment10, file = "10Report.txt")