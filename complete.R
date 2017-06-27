complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
 
  
  files_names = list.files(path=directory, pattern="*.csv")[id]
  
  data <- data.frame()
  result <- data.frame()
  names(result) <- c('id', 'obs')
  for (value in id) {
    file_name = list.files(path=directory, pattern="*.csv")[value]
    file_data <- read.csv(paste(directory, "/", file_name, sep=""))
    result <- rbind(result, data.frame(value, sum(complete.cases(file_data))))
    ##data <- rbind(data,file_data)
  }
  
  complete.cases(data)
 
}