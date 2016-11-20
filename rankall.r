# set working directory to location of source file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# init static data to null
outcome_data <- NULL
states <- NULL
valid_outcomes = NULL
death_rate_for = NULL   ## holds column position for each type of outcome-related death rate


rankall <- function(outcome, ranking="best") {
    # Returns a 2-column data frame with the name of the hospital in each state with the requested ranking.
    #
    # Args:
    #   outcome: can be one of "heart attack", "heart failure", or "pneumonia"
    #   ranking: rank within state as "best" or "worst" or as a numeric rank such as 2 for 2nd place ranking 
    
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
    
    ## check that requested outcome is valid
    if( !is.element( outcome, valid_outcomes ) ) {
        stop( 'invalid outcome' )
    }

    ## for each state, find the hospital of the given rank
    ranked_results <- data.frame( state=character(), hospital=character() )
    death_rate_column <- death_rate_for[[outcome]]
    hospital_name_column <- 2
    state_column <- 7
    outcomes <- outcome_data[, c(state_column, hospital_name_column, death_rate_column)]
    names(outcomes) <- c('state','hospital','death_rate')
    outcomes <- outcomes[ outcomes$death_rate != "Not Available", ]
    outcomes <- outcomes[ with( outcomes, order( outcomes$state, outcomes$death_rate, outcomes$hospital ) ),]
    rankhospital <- function( outcomes, state ) {
        state_outcomes <- outcomes[outcomes$state == state, ]
        hospital_count <- nrow( state_outcomes )
        row_of_rank <- NA
        if( ranking == "best" ) {
            row_of_rank <- 1
        } else if( ranking == "worst" ) {
            row_of_rank <- hospital_count
        } else if( ranking <= hospital_count ) {
            row_of_rank <- ranking
        }
        print(paste(state, "outcomes for rank", ranking, "is located at row", row_of_rank))
        if( is.na(row_of_rank) ) {
            ranked_results <<- rbind( ranked_results, data.frame( state=state, hospital=NA ) )
        } else{
            ranked_results <<- rbind( ranked_results, state_outcomes[ row_of_rank, c(1,2) ] )
        }
    }
    sapply( states, rankhospital, outcomes=outcomes )

    ## return a data frame with the hospital name and state with the requested rank
    return(ranked_results)
}

