
# set working directory to location of source file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
print(getwd())

# load test data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#x <- rnorm(5000,0,25)
#y <- rnorm(5000,0,25)
#plot(x,y)


best <- function(state, outcome) {
    ## read outcome data
    
    ## check that state and outcome are valid
    
    ## return hospital name in that state with lowest 30-day death rate
    
}