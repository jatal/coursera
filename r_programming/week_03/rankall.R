

rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  ooc_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  validStates = tolower(levels( as.factor(ooc_measures[,"State"]) ))
  
  ## Validate parameters
  my_outcome <- tolower(outcome)
  if( ! my_outcome %in% c("heart attack", "heart failure", "pneumonia") ) {
    stop( "invalid outcome")
  }
  
  #...NOTE: tolower(integer) coerces to charater, so we're fine
  my_num <- tolower(num)
  if(!(
    (is.numeric(num) && num==round(as.numeric(num)))
    ||
      my_num %in% c("best","worst")
  )
  ) {
    stop( "invalid num")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ## Actual Column Names in file:
  ## - 11 :: "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  ## - 17 :: "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"  
  ## - 23 :: "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  colName = ""
  if( my_outcome == "heart attack") {
    colName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  }
  else if( my_outcome == "heart failure") {
    colName = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  else {
    colName = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  hospitals <- ooc_measures[ ooc_measures[,colName]!="Not Available", ]
  hospitals[,colName] <- as.numeric(hospitals[,colName])
  
  #...Change rank order for "worst" case, so that the worst will be in the first row
  sort_mult <- 
    if (my_num=="worst") {
      -1
    } else {
      1
    }
      
  hospitals <- hospitals[ order( hospitals$State, hospitals[,colName], hospitals$Hospital.Name ), ]

  hospitals$my_rank <- unlist( 
    tapply(hospitals[,colName], hospitals$State, FUN=function(x) rank(sort_mult*x, ties.method="first"), simplify=FALSE) 
  )
  
  match_hospitals <- 
    if (my_num %in% c("best","worst")) {
      hospitals[ hospitals$my_rank==as.numeric(1), ]
    } else {
      hospitals[ hospitals$my_rank==as.numeric(my_num), ]
    }

  result <- data.frame( State=unique(hospitals$State) )
  result <- merge(result, match_hospitals, by="State", all=TRUE)
  
  data.frame( hospital=result$Hospital.Name, state=result$State )
  
}