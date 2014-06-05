best <- function(state, outcome = "heart attack") {
        # read
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        if (outcome == "heart attack") {
                colnum <- 11
        } else if (outcome == "heart failure") {
                colnum <- 17
        } else if (outcome == "pneumonia") {
                colnum <-23
        } else {
                stop('invalid outcome')
        }
        
        hospital_col = 2
        state_col = 7
        
        data <- split(data[,c(hospital_col,colnum)],data[,state_col])
        if (state %in% names(data)) {
                data <- data[[state]]
                data[,2] <- as.numeric(data[,2])
                data <- data[complete.cases(data),]
                minrate <- min(data[,2])
                hospitals <- data[ data[,2] == minrate, 1]
                name <- hospitals[order(hospitals)[1]]
                name
        } else {
                stop('invalid state')
        }
        # check
        
        #rate
}