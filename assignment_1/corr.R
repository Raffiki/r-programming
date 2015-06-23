corr <- function(directory, threshold = 0) {
  
  selected.files <- row.names(subset(complete(directory), nobs > threshold))
  result <- numeric(length(selected.files))
  
  for (i in seq_along(selected.files)) {
    dataframe <- na.omit(read.csv(selected.files[i]))
    result[i] = cor(dataframe$sulfate, dataframe$nitrate)
  }
  
  result
}