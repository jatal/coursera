


rankhospital <- function(state, outcome, num="best") {
  
  ## Read outcome data
  ooc_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  validStates = tolower(levels( as.factor(ooc_measures[,"State"]) ))
  
  ## Check that state and outcome are valid
  my_state <- tolower(state)
  if( ! my_state %in% validStates ) {
    stop( "invalid state")
  }
  
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
  
  hospitalsInState <- ooc_measures[ ooc_measures$State==toupper(state) & ooc_measures[,colName]!="Not Available", ]

  #...NOTE: we get the LOWEST value, since less death is best, and it is critical to sort on value as a numeric percentage
  hospitalsInState <- hospitalsInState[ order( as.numeric(hospitalsInState[,colName]), hospitalsInState[,"Hospital.Name"] ), ]
  
  
  #...Get the hospital with the indicated rank.  In case of tie, select the hospital with lowest alphanumeric sort value in name
  result <- 
  if (my_num=="best") {
    hospitalsInState[ as.numeric(1), ]
  } else if (my_num=="worst") {
    hospitalsInState[ as.numeric(nrow(hospitalsInState)), ]
  } else {
    hospitalsInState[ as.numeric(my_num), ]
  }
  
  result$Hospital.Name
}


