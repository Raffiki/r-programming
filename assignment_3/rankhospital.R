
rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  possible.states = unique(data$State)
  if (!is.element(state, possible.states)) stop("invalid state")
  possible.outcomes = list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if (!is.element(outcome, names(possible.outcomes))) stop("invalid outcome")
  
  data <- data[data[["State"]] == state,]
  cname <- possible.outcomes[[outcome]]
  data[,cname] <- as.numeric(data[,cname])
  data <- na.omit(data)
  if (num == 'best') num <- 1
  if (num == 'worst') num <- nrow(data)
  num <- as.numeric(num)
  data.sorted <- data[order(data[,cname], data[,'Hospital.Name']),]
  
  data.sorted[num,'Hospital.Name']
}