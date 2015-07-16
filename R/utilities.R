retrieveFile <- function(url, dir, ...) {
  split <- unlist(strsplit(url, "/"))
  fname <- paste(dir, split[length(split)], sep = "/")
  if (!file.exists(fname)) {
    message(paste(fname, "does not exist.  Downloading file..."))
    dir.create(dir, showWarnings = FALSE)
    download.file(url, fname)
  }
  message(paste("Importing", fname))
  return(import(fname, ...))
}
