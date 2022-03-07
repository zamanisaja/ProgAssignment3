best <- function (state = "TX", which_outcome="heart attack") {
    ## Read outcome data
    outcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
    
    state_index <- 7
    outcome_index <- 0
    
    if (which_outcome == "heart attack")
      outcome_index <- 11
    else if (which_outcome == "heart failure")
      outcome_index <- 17
    else if (which_outcome == "pneumonia")
      outcome_index <- 23
    else {
      print ("Unvalid outcome!")
      return() 
    }
    
    
    state_outcome <- subset(outcome, State==state, 
                            select = c(2, outcome_index), 
                            stringsAsFactors = FALSE)
    
    state_outcome[, 2] <- suppressWarnings (as.numeric(state_outcome[, 2] ) )
    min_outcome <- min(state_outcome[, 2], na.rm = TRUE)
    
    best_hospitals <- subset(state_outcome, state_outcome[2] == min_outcome)
    hospital_names <- sort(best_hospitals[,1])
    hospital_names[1]
    
}