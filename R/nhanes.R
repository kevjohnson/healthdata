getNhanes <- function(years, files, variables, dir) {
  yearLetters <- c("1999" = "A", "2001" = "B", "2003" = "C", "2005" = "D",
                   "2007" = "E", "2009" = "F", "2011" = "G", "2013" = "H")
  dataList <- list()
  i <- 1
  for (y in years) {
    dataListYear <- list()
    j <- 1
    for (f in files) {
      url <- paste("http://wwwn.cdc.gov/Nchs/Nhanes/", y, "-", y+1, "/", f,
                   "_", yearLetters[as.character(y)], ".XPT", sep = "")
      dataListYear[[j]] <- retrieveFile(url, dir, format = "xpt")
      j <- j + 1
    }
    dataList[[i]] <- do.call(full_join, dataListYear)
    i <- i + 1
  }
  finalData <- do.call(rbind, dataList)
  return(finalData)
}
