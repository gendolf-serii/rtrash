rankall <- function(outcome, num = "best") {
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
        
        hospital = character()
        state = character()
        
        data <- split(data[,c(hospital_col,colnum)],data[,state_col])
        
        for(state_name in names(data)) {
                data_state <- data[[state_name]]
                data_state[,2] <- as.numeric(data_state[,2])
                data_state <- data_state[complete.cases(data_state),]
                data_state <- data_state[order(data_state[,2],data_state[,1]),]
                
                if (num == "best") {
                        minrate <- min(data_state[,2])
                        hospitals <- data_state[ data_state[,2] == minrate, 1]
                        name <- hospitals[order(hospitals)[1]]                  
                } else if (num == "worst") {
                        maxrate <- max(data_state[,2])
                        hospitals <- data_state[ data_state[,2] == maxrate, 1]
                        name <- hospitals[order(hospitals)[1]]
                } else if (is.numeric(num) && num < nrow(data_state)) {
                        name <- data_state[[num,1]]
                } else {
                        name = NA
                }
        
                hospital <- c(hospital,name)
                state <- c(state,state_name)
        }
        data.frame(hospital,state)
        # check
        
        #rate
}