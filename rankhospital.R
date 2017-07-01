rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = 'character')
  
  ## Check that state and outcome are valid
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
  } else { 
    stop("invalid outcome")
  }
  
  names <- c("Hospital.Name", column)
  ## Return hospital name in that state with lowest 30-day death
  outcome_state <- outcome_data[outcome_data$State==state, ]
  ##outcome_state <- subset(outcome_data, outcome_data[["State"]] == state)
  outcome_state[,column] <- as.numeric(outcome_state[, column])
  result <- na.omit(outcome_state[names])
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  ##retorno <- result[ order(result[[column]]) ,]
  retorno <- result
  ordered_data_for_state <- order(result[column], result$Hospital.Name, na.last=NA)
  ##retorno <- retorno[order(retorno$Hospital.Name)]
  
  if (num=="best"){
    rank <- 1
  } else if (num=="worst"){
    rank <- nrow(result)
  } else {
    rank <- num
  }
  
  ##retorno<-retorno[rank,] 
  ## ordered_data_for_state[rank] retorna o indice do rankézimo elemento de acordo com a ordenacao
  retorno$Hospital.Name[ordered_data_for_state[rank]]
}