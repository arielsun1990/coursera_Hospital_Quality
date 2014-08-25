# state parameter - 2-character abbreviated name of a state
# outcome parameter 
# Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

best <- function(state, outcome) {
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
  
  
  
  
  # Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
  # be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals "b", "c",
  # and “f” are tied for best, then hospital "b" should be returned).
  
  relevant_data <- outcome_data[, c(2, 7, outcomes[outcome]),] # Reduce data frame to only relevant columns
  names(relevant_data) <- c("hospital", "state", "rate") # Assign shorter names for convenience
  relevant_data <- relevant_data[relevant_data$state == state & !is.na(relevant_data$rate) & relevant_data$rate!= 'Not Available', ] # Filter rows in needed state and remove rows with NA rate
  relevant_data$rate <- as.numeric(relevant_data$rate ) # Coerce rates from character to numeric
  result <- relevant_data[order(relevant_data$rate, relevant_data$hospital), ] #order by rate and name
  
  # Return hospital name in that state with lowest 30-day death
  # rate for the specified outcome  in that state
  result$hospital[1]
}
