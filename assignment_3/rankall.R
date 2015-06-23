
rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- na.omit(data)
  
  possible.states = unique(data$State)
  print(possible.states)
  states.sorted = possible.states[order(possible.states)]
  
  possible.outcomes = list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  if (!is.element(outcome, names(possible.outcomes))) stop("invalid outcome")
  
  N <- length(possible.states)
  print(N)
  result <- data.frame(hospital = character(N), state = character(N))
  print(result)
  
  cname <- possible.outcomes[[outcome]]
  
  
  for (i in seq_along(states.sorted)) {
    state <- states.sorted[i]
    data.state <- data[data[["State"]] == state,]
    data.state[,cname] <- as.numeric(data.state[,cname])
    
    if (num == 'best') num <- 1
    if (num == 'worst') num <- nrow(data)
    num <- as.numeric(num)
    
    data.state.sorted <- data.state[order(data.state[,cname], data.state[,'Hospital.Name']),]
    #print(data.state.sorted[num,'Hospital.Name'])
    print(class(data.state.sorted[num,'Hospital.Name']))
    result$hospital[i] <- data.state.sorted[num,'Hospital.Name']
    
    result$state[i] <- state
    print(result[i,])
  }
  
  result
}