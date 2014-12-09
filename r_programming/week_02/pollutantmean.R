pollutantmean <- function(directory, pollutant, id = 1:332, precision=3) {
  
  #...Build a vector by concatenating all polutant values (removing NA) across passed files
  pollutantVals <- c()
  for ( i in id ) {

    s <- sprintf("%03d",i)
    file <- paste(directory,"/",s,".csv",sep="")
    f <- read.csv(file)
    
    vals <- f[,pollutant]
    pollutantVals <- c(pollutantVals, vals[!is.na(vals)])
  }

  #...Return mean with specified precision
  meanVal <- mean(pollutantVals)
  round(meanVal, precision)
}


