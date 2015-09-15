##' Get Census data.
##'
##' \code{getCensus} returns data from a Census API call based on the given
##' parameters.
##'
##' This function requires the survey type, year, variables, and geography for
##' the API call.  Please see
##' \url{http://www.census.gov/data/developers/data-sets.html} for documentation
##' on the required variables.
##'
##' @param survey "sf1" for Census.  "acs5", "acs3", "acs1" for ACS 5-year,
##'   3-year, and 1-year estimates, respectively.
##' @param year A numeric year.  I will not check to make sure your year is
##'   valid with the survey you selected.  That's on you.
##' @param vars A character vector listing the variables to request.  See Census
##'   API documentation for a list of available variables.  The maximum number
##'   of variables per query is 50.  I could do some magic to take care of
##'   splitting the queries for you but if you need more than 50 variables then
##'   you can take the time to implement that yourslef.
##' @param geo A string with the level of geography, should be something like
##'   "tract:*" or "block+group:*".  See Census API documentation for examples.
##' @param area A character vector listing the specific areas to include, should
##'   be something like "state:13" or c("state:13", "county:159").  See Census
##'   API documentation for examples (aren't you glad I linked it above?)
##' @param key A string with your Census API key.  I have filled in my own key
##'   as a default so you can start immediately but please register for your own
##'   at \url{http://api.census.gov/data/key_signup.html}.  Seriously...
##' @return 
##' @author Kevin Johnson
getCensus <- function(survey, year, vars, geo, area = NULL,
                      key = "842879abc12dbd7da277aac4dc1a77c8e0f8778d") {
  vars <- paste(vars, sep = "", collapse = ",")
  if (is.null(area)) {
    url <- paste("http://api.census.gov/data/", year, "/", survey, "?get=",
                 vars, "&for=", geo, "&key=", key, sep = "")
  } else {
    area <- paste(area, sep = "", collapse = "+")
    url <- paste("http://api.census.gov/data/", year, "/", survey, "?get=",
                 vars, "&for=", geo, "&in=", area, "&key=", key, sep = "")
  }
  rawData <- try(readLines(url), silent = TRUE)
  if (class(rawData) == "try-error") {
    print(url)
    return
  }
  dataDF <- data.frame()
  temp <- strsplit(gsub("[^[:alnum:], _]", "", rawData), "\\,")
  dataDF <- as.data.frame(do.call(rbind, temp[-1]), stringsAsFactors = FALSE)
  names(dataDF) <- tmp[[1]]
  numCols <- grep("[0-9]", names(dataDF), value = TRUE)
  for (c in numCols) {
    dataDF[,c] <- as.numeric(as.character(dataDF[,c]))
  }
  return(dataDF)
}
