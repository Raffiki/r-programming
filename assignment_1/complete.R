complete <- function(directory, id = 1:332) {
  files <- list.files(directory, pattern="\\.csv$")
  files.numeric <- as.numeric(sub("\\.csv$", "", files))
  
  selected.files = files[match(id,files.numeric)]
  
  nrows <- sapply(file.path(directory, selected.files), function(f) sum(complete.cases(read.csv(f))))
  ids <- sapply(names(nrows), function(f) as.numeric(sub("\\.csv$", "", basename(f))))
  data.frame(id=ids, nobs=nrows)
}