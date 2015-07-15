readURL <- function(url) {
  out <- tryCatch(readLines(conn = url, warn = FALSE),
                  error = function(cond) {
                    message(cond)
                    message("URL does not exist, returning NA.")
                    message(url)
                    return(NA)
                  },
                  warning = function(cond) {
                    message(cond)
                    message("URL caused a warning, returning NA.")
                    message(url)
                    return(NA)
                  },
                  finally = message(paste("Processed URL:", url)))
  return(out)
}
