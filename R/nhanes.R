getNhanes <- function(years, files, variables) {
  yearLetters <- c("1999" = "A", "2001" = "B", "2003" = "C", "2005" = "D",
                   "2007" = "E", "2009" = "F", "2011" = "G", "2013" = "H")
  urls <- list()
  for (y in years) {
    for (f in files) {
      urls[y][f] <- paste("http://wwwn.cdc.gov/Nchs/Nhanes/", y, "-", y+1, "/",
                          f, "_", yearLetters(as.character(y)), ".XPT",
                          sep = "")
    }
  }
}
