corr <- function (directory, threshold=0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of lenght 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  files_names = list.files(path=directory, pattern="*.csv")
  result <- vector()
  
  for (file in files_names) {
    file_data <- read.csv(paste(directory, "/", file, sep=""))
    cc <- complete.cases(file_data)
    nobs <- sum(cc)
    
    if (nobs > threshold) {
      correlation <- cor(file_data$sulfate[cc], file_data$nitrate[cc])
      result <- rbind(result, correlation)
      files_threshold <- rbind(files_threshold, file)
    }
    
      ##data <- rbind(data,file_data)
  }
  
  as.vector(result)
  
}