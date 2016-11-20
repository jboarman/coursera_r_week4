# set working directory to location of source file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# init static data to null
outcome_data <- NULL
states <- NULL
valid_outcomes = NULL
death_rate_for = NULL   ## holds column position for each type of outcome-related death rate


best <- function(state, outcome) {
    # Returns a character vector with the name of the hospital that has the
    # best (i.e. lowest) 30-day mortality for the specified outcome in a given state.
    #
    # Args:
    #     state: a valid 2-character abbreviated name of a state (e.g., CA, FL, TX)
    #   outcome: can be one of "heart attack", "heart failure", or "pneumonia"

    ## read outcome data and initiatize static data
    if(is.null(outcome_data)) {
        outcome_data <<- data.frame(read.csv("outcome-of-care-measures.csv", colClasses = "character"))
        states <<- unique(outcome_data$State)
        valid_outcomes <<- c('heart attack','heart failure','pneumonia')
        death_rate_for <<- new.env(size = 3)
        death_rate_for[['heart attack']] <- 11    ## col position of heart attack death rate
        death_rate_for[['heart failure']] <- 17   ## col position of heart failure death rate
        death_rate_for[['pneumonia']] <- 23       ## col position of pneumonia death rate
    }

    ## check that state and outcome are valid
    if( !is.element( state, states ) ) {
        stop( 'invalid state' )
    }
    if( !is.element( outcome, valid_outcomes ) ) {
        stop( 'invalid outcome' )
    }

    ## return hospital name in that state with lowest 30-day death rate
    death_rate_column <- death_rate_for[[outcome]]
    hospital_name_column <- 2
    state_outcomes <- outcome_data[outcome_data$State == state, c(hospital_name_column, death_rate_column)]
    state_outcomes <- state_outcomes[ state_outcomes[2] != "Not Available", ]
    state_outcomes <- state_outcomes[ with( state_outcomes, order( state_outcomes[2], state_outcomes[1] ) ),]
    state_outcomes[1,1]    # return top row in sorted data frame
}

