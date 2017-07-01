rankall <- function(outcome, num = "best") {
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
  } else { 
    stop("invalid outcome")
  }
  
  names <- c("State", "Hospital.Name", column)
  
  outcome_data[,column] <- as.numeric(outcome_data[, column])
  result <- na.omit(outcome_data[names])
  
  result <- result[order(result$State, result[column], result$Hospital.Name ), ]
  
  ordered_result <- split(result, result$State)
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  hospitalNameFunction <- function(x, num) {
    # Order by Deaths and then HospitalName
    #x = x[order(x$Deaths, x$Hospital.Name),]
    
    # Return
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }
  
  pre_result <- lapply(ordered_result, hospitalNameFunction, num)
  #Return data.frame with format
  return ( data.frame(hospital=unlist(pre_result), state=names(pre_result)) )
}