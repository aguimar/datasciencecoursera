best <- function(state, outcome) {
  ## Read outcome data
  
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  
  ## Check that state and outcome are valid
  if(!any(state == outcome_data$State)) {
    stop('invalid state')
  }
  
  if (outcome == "heart attack")  {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else { best("NY", "hert attack")
    stop("invalid outcome")
  }
  
  names <- c("Hospital.Name", column)
  ## Return hospital name in that state with lowest 30-day death
  outcome_state <- outcome_data[outcome_data$State==state, ]
  ##outcome_state <- subset(outcome_data, outcome_data[["State"]] == state)
  outcome_state[,column] <- as.numeric(outcome_state[, column])
  result <- na.omit(outcome_state[names])
  ##result <- na.omit(result)
  
  retorno <- result[which.min(result[[column]]),]$Hospital.Name
  ##result[which(result[column]==min(result[column])), 1]
  ##result[which.min(result[[column]]),"Hospital.Name"]
  retorno
  ## rate
}

best2 <- function(state, outcome) {
  
  data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
  
  if(!any(state == data$State)) {
    stop('invalid state')
  }
  
  if(outcome == 'heart attack') {
    i <- 11
  }
  else if(outcome == 'heart failure') {
    i <- 17
  }
  else if(outcome == 'pneumonia') {
    i <- 23
  }
  else {
    stop('invalid outcome')
  }
  
  # print(i)
  
  # todo: handle the ties
  data.state <- data[data$State == state, ]
  data.state[, i] <- as.numeric(x=data.state[, i])
  
  data.state <- data.state[complete.cases(data.state), ]
  
  # print(data.state[, c(1, 2, i)])
  # print(data.state[, i])
  # min(data.state[, i]) -> mm
  # print(mm)
  # print(min(data.state[, i], na.rm=TRUE))
  
  # print(data.state[, i] == min(data.state[, i]))
  
  return.names <- data.state[(data.state[, i] == min(data.state[, i])), ]$Hospital.Name
  
  sort(return.names)[1]
}
