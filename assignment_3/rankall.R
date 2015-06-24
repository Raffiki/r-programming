
rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  possible.states = unique(data$State)
  states.sorted = possible.states[order(possible.states)]
  
  possible.outcomes = list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if (!is.element(outcome, names(possible.outcomes))) stop("invalid outcome")
  
  N <- length(possible.states)
  result <- data.frame(hospital = character(N), state = character(N), stringsAsFactors=FALSE)
  
  cname <- possible.outcomes[[outcome]]
  
  
  for (i in seq_along(states.sorted)) {
    state <- states.sorted[i]
    data.state <- data[data[["State"]] == state,]
    data.state[,cname] <- as.numeric(data.state[,cname])
    data.state <- na.omit(data.state)
    
    if (num == 'best') rank <- 1
    else if (num == 'worst') rank <- nrow(data.state)
    else rank <- num
    rank <- as.numeric(rank)
    data.state.sorted <- data.state[order(data.state[,cname], data.state[,'Hospital.Name']),]
 
    result$hospital[i] <- data.state.sorted[rank,'Hospital.Name']
    result$state[i] <- state
  }
  
  result
}