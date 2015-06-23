best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  possible.states = unique(data$State)
  if (!is.element(state, possible.states)) stop("invalid state")
  possible.outcomes = list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if (!is.element(outcome, names(possible.outcomes))) stop("invalid outcome")
  
  data <- data[data[["State"]] == state,]
  cname <- possible.outcomes[[outcome]]
  data[,cname] <- as.numeric(data[,cname])
  data <- na.omit(data)
  data.min <- min(data[, cname])
  result.raw <- (data[data[,cname] == data.min,])
  result.sorted <- order(result.raw[["Hospital.Name"]])
  head(result.raw[result.sorted,])[["Hospital.Name"]]
}

