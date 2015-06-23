pollutantmean <- function(directory, pollutant, id = 1:332) {

  files <- list.files(directory, pattern="\\.csv$")
  files.numeric <- as.numeric(sub("\\.csv$", "", files))
  
  selected.files = files[match(id,files.numeric)]
  selected.files.data <- lapply(file.path(directory, selected.files), read.csv, header=TRUE)

  pollutant <- unlist(sapply(selected.files.data, function(x) x[ ,pollutant]))
  mean(pollutant, na.rm=TRUE)
}