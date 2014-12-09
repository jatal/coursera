source("complete.R")

corr <- function(directory, threshold = 0, precision=5) {

  completeDf <- complete(directory)
  filesDf <- completeDf[completeDf$nobs > threshold,]
  
  corrls <- c()
  for( id in filesDf$id ) {
    s <- sprintf("%03d",id)
    file <- paste(directory,"/",s,".csv",sep="")
    df <- read.csv(file)
    corrl <- cor(df$sulfate, df$nitrate, use="complete.obs")
    corrls <- cbind(corrls, round(corrl,precision) )
  }
  
  as.vector(corrls)

}