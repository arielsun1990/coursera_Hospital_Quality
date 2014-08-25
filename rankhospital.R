
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
# > rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. 
#
# The num argument can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. 
#
# Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.
# 
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.


# param outcome  
# param state : the 2-character abbreviated name of a state
# param num: ranking of a hospital in that state for that outcome (num)
rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character" )
  
  # Check if that state is valid
  states = unique(outcome_data$State) # creates a vector with the states from data
  str(states)
  if (!state %in% states) {
    stop ("invalid state")
  }
  
  # Check if the outcome is valid
  outcomes <- c(11, 17, 23)
  names(outcomes) <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% names(outcomes)) {
    stop ("invalid outcome")
  }
  relevant_data <- outcome_data[, c(2, 7, outcomes[outcome]),] # Reduce data frame to only relevant columns
  names(relevant_data) <- c("hospital", "state", "rate") # Assign shorter names for convenience
  relevant_data <- relevant_data[relevant_data$state == state & !is.na(relevant_data$rate) & relevant_data$rate!= 'Not Available', ] # Filter rows in needed state and remove rows with NA rate
  relevant_data$rate <- as.numeric(relevant_data$rate ) # Coerce rates from character to numeric
  result <- relevant_data[order(relevant_data$rate, relevant_data$hospital), ] #order by rate and name
  
  ############################
  # until here the function is the same as best.R
  # the difference is in the output
    
  if (num == 'best'){
    return(result$hospital[1])
  }  else if (num =='worst') {
        reverse <- rev(result$hospital)
        return(reverse[1])
    print(reverse)
  }
  else {
    return(result$hospital[num])
  }
}
