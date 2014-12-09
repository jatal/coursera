complete <- function(directory, id = 1:332) {
  
  resultDf <- data.frame(id=c(),nobs=c())
  
  for ( i in id ) {
    
    s <- sprintf("%03d",i)
    file <- paste(directory,"/",s,".csv",sep="")
    f <- read.csv(file)
    
    #...Row apply function that loops through every column looking for NA values
    #   Result is a Boolean Vector where TRUE elements mean the row has an NA value
    has.na <- function(x) {
      hasNA <- FALSE
      for(col in x) {
        if(is.na(col)) {
          hasNA <- TRUE
          break
        }
      }
      hasNA
    }

    completeRowTruthVector <- apply(f,1,has.na)

    #...Count rows with no NA (FALSE values in the truth vector)
    completeRowCount <- length( completeRowTruthVector[!completeRowTruthVector] )
                               
    #...Append to result
    resultDf <- rbind( resultDf, data.frame(id=i,nobs=completeRowCount) )
  }
  
  resultDf
}
